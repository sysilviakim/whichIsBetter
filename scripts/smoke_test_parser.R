source("R/helpers.R")

result <- parse_histogram_screenshot("synthetic_histogram.png")

cat("Parser success:", result$success, "\n")
cat("Counts 1-to-5:", paste(result$counts_1_to_5, collapse = ", "), "\n")
cat("OCR total reviews:", result$ocr_total_reviews, "\n")
cat("OCR average rating:", result$ocr_average_rating, "\n")
