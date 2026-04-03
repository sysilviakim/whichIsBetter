import argparse
import json
import math
import re
from dataclasses import dataclass
from typing import List, Optional

import cv2
import numpy as np
from rapidocr_onnxruntime import RapidOCR


ocr_engine = RapidOCR()


@dataclass
class OcrToken:
    text: str
    score: float
    center_x: float
    center_y: float
    box: list


def load_image(path: str) -> np.ndarray:
    # cv2.imread() can fail on Windows for non-ASCII paths, so read bytes first.
    raw = np.fromfile(path, dtype=np.uint8)
    if raw.size == 0:
        raise RuntimeError(f"Could not read image: {path}")
    img = cv2.imdecode(raw, cv2.IMREAD_COLOR)
    if img is None:
        raise RuntimeError(f"Could not read image: {path}")
    return img


def run_ocr_variants(img: np.ndarray) -> List[OcrToken]:
    variants = [
        img,
        cv2.cvtColor(img, cv2.COLOR_BGR2GRAY),
        cv2.threshold(cv2.cvtColor(img, cv2.COLOR_BGR2GRAY), 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)[1],
    ]

    tokens: list[OcrToken] = []
    seen = set()
    for variant in variants:
        result, _ = ocr_engine(variant)
        if not result:
            continue
        for box, text, score in result:
            key = (round(sum(p[0] for p in box) / 4, 1), round(sum(p[1] for p in box) / 4, 1), text)
            if key in seen:
                continue
            seen.add(key)
            center_x = sum(point[0] for point in box) / 4
            center_y = sum(point[1] for point in box) / 4
            tokens.append(OcrToken(text=text, score=float(score), center_x=center_x, center_y=center_y, box=box))
    return tokens


def parse_average_rating(tokens: List[OcrToken], width: int) -> Optional[float]:
    candidates = []
    for token in tokens:
      # OCR often emits weird punctuation; normalize to simple decimal form.
        cleaned = token.text.replace(",", ".")
        match = re.search(r"\b([1-5]\.\d)\b", cleaned)
        if not match:
            continue
        value = float(match.group(1))
        if token.center_x < width * 0.45:
            continue
        candidates.append((token.score, value))
    if not candidates:
        return None
    candidates.sort(reverse=True)
    return candidates[0][1]


def parse_total_reviews(tokens: List[OcrToken]) -> Optional[int]:
    best = None
    for token in tokens:
        text = token.text.lower()
        if not re.search(r"review|rating|ratings|reviews|리뷰", text):
            continue
        number_match = re.search(r"(\d+)", text)
        if number_match:
            count = int(number_match.group(1))
            best = count if best is None else max(best, count)

    if best is not None:
        return best

    review_tokens = [token for token in tokens if re.search(r"review|rating|ratings|reviews|리뷰", token.text.lower())]
    digit_tokens = [token for token in tokens if re.fullmatch(r"\d+", token.text.strip())]
    for review_token in review_tokens:
        nearby = [
            token for token in digit_tokens
            if abs(token.center_y - review_token.center_y) < 25 and token.center_x < review_token.center_x + 80
        ]
        if nearby:
            nearby.sort(key=lambda token: (-token.score, abs(token.center_y - review_token.center_y)))
            return int(nearby[0].text.strip())

    return None


def detect_row_centers(tokens: List[OcrToken], width: int) -> list[float]:
    label_tokens = []
    for token in tokens:
        text = token.text.strip()
        if text in {"1", "2", "3", "4", "5"} and token.center_x < width * 0.25:
            label_tokens.append((int(text), token.center_y))

    if len(label_tokens) >= 5:
        centers = {label: y for label, y in label_tokens}
        if all(label in centers for label in range(1, 6)):
            return [centers[5], centers[4], centers[3], centers[2], centers[1]]

    return []


def contiguous_runs(mask_1d: np.ndarray) -> list[tuple[int, int]]:
    runs = []
    in_run = False
    start = 0
    for i, value in enumerate(mask_1d):
        if value and not in_run:
            start = i
            in_run = True
        elif not value and in_run:
            runs.append((start, i - 1))
            in_run = False
    if in_run:
        runs.append((start, len(mask_1d) - 1))
    return runs


def longest_run_length(mask_1d: np.ndarray) -> tuple[int, Optional[tuple[int, int]]]:
    runs = contiguous_runs(mask_1d)
    if not runs:
        return 0, None
    start, end = max(runs, key=lambda run: run[1] - run[0] + 1)
    return end - start + 1, (start, end)


def detect_bar_geometry(img: np.ndarray, tokens: List[OcrToken]) -> tuple[list[float], list[float], dict]:
    height, width = img.shape[:2]
    hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)

    top = 0
    bottom = int(height * 0.86)
    left = 0
    right = int(width * 0.78)
    crop = hsv[top:bottom, left:right]

    sat = crop[:, :, 1]
    val = crop[:, :, 2]
    hue = crop[:, :, 0]

    color_mask = ((sat > 45) & (val > 80)).astype(np.uint8)
    gray_mask = ((sat < 45) & (val > 180) & (val < 250)).astype(np.uint8)
    track_mask = np.maximum(color_mask, gray_mask)

    row_centers = detect_row_centers(tokens, width)

    contour_rows = []
    mask_uint8 = (track_mask * 255).astype(np.uint8)
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (9, 3))
    contour_mask = cv2.morphologyEx(mask_uint8, cv2.MORPH_CLOSE, kernel)
    contours, _ = cv2.findContours(contour_mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        if w >= int(crop.shape[1] * 0.35) and 4 <= h <= 20 and x < int(crop.shape[1] * 0.9):
            contour_rows.append((y + h / 2, x, y, w, h))

    contour_rows = sorted(contour_rows, key=lambda row: row[0])

    if len(contour_rows) >= 5:
        contour_rows = contour_rows[:5]
        row_centers = [row[0] for row in contour_rows]

    if not row_centers:
        row_scores = []
        for y in range(track_mask.shape[0]):
            run_len, _ = longest_run_length(track_mask[y, :] > 0)
            row_scores.append(run_len)
        row_scores = np.array(row_scores)
        threshold = max(25, int(row_scores.max() * 0.55))
        candidate_rows = np.where(row_scores >= threshold)[0]
        if len(candidate_rows):
            groups = np.split(candidate_rows, np.where(np.diff(candidate_rows) > 3)[0] + 1)
            centers = [float(group.mean()) for group in groups if len(group)]
            centers = sorted(centers)
            if len(centers) >= 5:
                centers = centers[:5]
            if len(centers) == 5:
                row_centers = centers
            elif len(centers) >= 2:
                start_center = centers[0]
                end_center = centers[-1]
                step = (end_center - start_center) / 4 if end_center > start_center else 16
                row_centers = [start_center + i * step for i in range(5)]
        if not row_centers:
            row_centers = list(np.linspace(bottom * 0.15, bottom * 0.55, 5))

    fill_lengths = []
    total_lengths = []

    for idx, center in enumerate(row_centers):
        y0 = max(int(center) - 4, 0)
        y1 = min(int(center) + 4, crop.shape[0] - 1)
        band_color = color_mask[y0:y1 + 1, :]
        band_track = track_mask[y0:y1 + 1, :]

        color_line = band_color.mean(axis=0) > 0.15
        track_line = band_track.mean(axis=0) > 0.15

        contour_run = None
        if len(contour_rows) >= 5:
            _, cx, cy, cw, ch = contour_rows[idx]
            contour_run = (cx, cx + cw - 1)

        total_len, total_run = longest_run_length(track_line)
        if contour_run is not None:
            total_run = contour_run
            total_len = contour_run[1] - contour_run[0] + 1
        if total_run is None:
            fill_lengths.append(0.0)
            total_lengths.append(0.0)
            continue

        start, end = total_run
        color_focus = color_line[start:end + 1]
        fill_len, _ = longest_run_length(color_focus)

        fill_lengths.append(float(fill_len))
        total_lengths.append(float(total_len))

    diagnostics = {
        "row_centers": row_centers,
        "fill_lengths": fill_lengths,
        "total_lengths": total_lengths,
    }

    return fill_lengths, total_lengths, diagnostics


def normalize_ratios(fill_lengths: list[float], total_lengths: list[float]) -> list[float]:
    ratios = []
    for fill_len, total_len in zip(fill_lengths, total_lengths):
        if total_len <= 0:
            ratios.append(0.0)
        else:
            ratios.append(max(0.0, min(1.0, fill_len / total_len)))

    if sum(ratios) <= 0:
        return [0.0, 0.0, 0.0, 0.0, 1.0]
    return ratios


def weighted_mean(counts_1_to_5: np.ndarray) -> float:
    stars = np.arange(1, 6)
    return float((stars * counts_1_to_5).sum() / counts_1_to_5.sum())


def candidate_weight_sums(total_reviews: int, average_rating: float, tolerance: float = 0.051) -> list[int]:
    lower = max(total_reviews, math.ceil(total_reviews * (average_rating - tolerance)))
    upper = min(5 * total_reviews, math.floor(total_reviews * (average_rating + tolerance) - 1e-9))
    candidates = list(range(lower, upper + 1))
    if candidates:
        return candidates

    nearest = int(round(total_reviews * average_rating))
    return [min(max(nearest, total_reviews), 5 * total_reviews)]


def objective_from_probs(current: np.ndarray, probs: np.ndarray) -> float:
    if current.sum() == 0:
        return float("inf")
    return float(np.square(current / current.sum() - probs).sum())


def infer_counts_with_mean_constraint(
    probs: np.ndarray,
    total_reviews: int,
    average_rating: float,
) -> Optional[np.ndarray]:
    if total_reviews > 250:
        return None

    best_counts = None
    best_score = float("inf")
    candidate_sums = candidate_weight_sums(total_reviews, average_rating)

    for weighted_sum in candidate_sums:
        for count_1 in range(total_reviews + 1):
            remaining_after_1 = total_reviews - count_1
            weighted_after_1 = weighted_sum - count_1
            if weighted_after_1 < 2 * remaining_after_1 or weighted_after_1 > 5 * remaining_after_1:
                continue

            for count_2 in range(remaining_after_1 + 1):
                remaining_after_2 = remaining_after_1 - count_2
                weighted_after_2 = weighted_after_1 - 2 * count_2
                if weighted_after_2 < 3 * remaining_after_2 or weighted_after_2 > 5 * remaining_after_2:
                    continue

                for count_3 in range(remaining_after_2 + 1):
                    remaining_after_3 = remaining_after_2 - count_3
                    weighted_after_3 = weighted_after_2 - 3 * count_3
                    if remaining_after_3 < 0:
                        continue

                    count_5 = weighted_after_3 - 4 * remaining_after_3
                    count_4 = remaining_after_3 - count_5
                    if count_4 < 0 or count_5 < 0:
                        continue

                    candidate = np.array([count_1, count_2, count_3, count_4, count_5], dtype=int)
                    score = objective_from_probs(candidate, probs)
                    if score + 1e-12 < best_score:
                        best_score = score
                        best_counts = candidate

    return best_counts


def infer_counts(ratios_top5_to1: list[float], total_reviews: int, average_rating: Optional[float]) -> np.ndarray:
    ratios_1_to_5 = np.array(list(reversed(ratios_top5_to1)), dtype=float)
    if ratios_1_to_5.sum() <= 0:
        ratios_1_to_5 = np.array([0, 0, 0, 0, 1], dtype=float)
    probs = ratios_1_to_5 / ratios_1_to_5.sum()

    if average_rating is not None:
        constrained = infer_counts_with_mean_constraint(probs, total_reviews, average_rating)
        if constrained is not None:
            return constrained

    raw = probs * total_reviews
    counts = np.floor(raw).astype(int)
    remainder = int(total_reviews - counts.sum())
    if remainder > 0:
        order = np.argsort(-(raw - counts))
        for idx in order[:remainder]:
            counts[idx] += 1

    def objective(current: np.ndarray) -> float:
        prob_error = objective_from_probs(current, probs)
        mean_error = 0.0
        if average_rating is not None:
            mean_error = 1.75 * (weighted_mean(current) - average_rating) ** 2
        return float(prob_error + mean_error)

    improved = True
    while improved:
        improved = False
        best_score = objective(counts)
        best_counts = counts.copy()
        for i in range(5):
            if counts[i] == 0:
                continue
            for j in range(5):
                if i == j:
                    continue
                candidate = counts.copy()
                candidate[i] -= 1
                candidate[j] += 1
                score = objective(candidate)
                if score + 1e-9 < best_score:
                    best_score = score
                    best_counts = candidate
                    improved = True
        counts = best_counts

    return counts


def parse_histogram_image(
    path: str,
    average_rating_override: Optional[float] = None,
    total_reviews_override: Optional[int] = None,
) -> dict:
    img = load_image(path)
    height, width = img.shape[:2]
    tokens = run_ocr_variants(img)
    ocr_average_rating = parse_average_rating(tokens, width)
    ocr_total_reviews = parse_total_reviews(tokens)
    average_rating = average_rating_override if average_rating_override is not None else ocr_average_rating
    total_reviews = total_reviews_override if total_reviews_override is not None else ocr_total_reviews
    fill_lengths, total_lengths, geometry = detect_bar_geometry(img, tokens)
    ratios = normalize_ratios(fill_lengths, total_lengths)

    counts = None
    if total_reviews is not None and total_reviews > 0:
        counts = infer_counts(ratios, total_reviews, average_rating).tolist()

    parser_notes = []
    if total_reviews_override is not None:
        parser_notes.append(f"Used user-supplied total review count: {total_reviews_override}.")
    elif total_reviews is None:
        parser_notes.append("Could not confidently OCR the total review count.")
    if average_rating_override is not None:
        parser_notes.append(f"Used user-supplied average rating: {average_rating_override:.1f}.")
    elif average_rating is None:
        parser_notes.append("Could not confidently OCR the displayed average rating.")

    return {
        "success": counts is not None,
        "counts_1_to_5": counts,
        "counts_5_to_1": list(reversed(counts)) if counts is not None else None,
        "bar_ratios_5_to_1": ratios,
        "ocr_average_rating": ocr_average_rating,
        "ocr_total_reviews": ocr_total_reviews,
        "used_average_rating": average_rating,
        "used_total_reviews": total_reviews,
        "ocr_tokens": [
            {
                "text": token.text,
                "score": token.score,
                "center_x": token.center_x,
                "center_y": token.center_y,
            }
            for token in tokens
        ],
        "geometry": geometry,
        "notes": parser_notes,
        "message": (
            "Parsed screenshot and estimated histogram counts."
            if counts is not None
            else "Parser could not estimate counts from this screenshot."
        ),
    }


def main():
    parser = argparse.ArgumentParser(description="Parse a review histogram screenshot.")
    parser.add_argument("image_path")
    parser.add_argument("--average-rating", type=float, default=None)
    parser.add_argument("--total-reviews", type=int, default=None)
    args = parser.parse_args()

    result = parse_histogram_image(
        args.image_path,
        average_rating_override=args.average_rating,
        total_reviews_override=args.total_reviews,
    )
    # Keep stdout ASCII-safe so Windows shells with cp949/cp1252 encodings
    # don't crash when OCR returns unexpected Unicode glyphs.
    print(json.dumps(result, ensure_ascii=True))


if __name__ == "__main__":
    main()
