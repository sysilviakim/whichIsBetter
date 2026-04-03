suppressPackageStartupMessages({
  library(jsonlite)
})


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


find_python_binary <- function() {
  candidates <- c("python", "python3", "py")
  paths <- Sys.which(candidates)
  path <- unname(paths[nzchar(paths)][1])
  if (!nzchar(path)) {
    stop("Python was not found on PATH.")
  }
  path
}


python_parser_path <- function() {
  path <- normalizePath(file.path(getwd(), "python", "parse_histogram.py"), winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    stop("`python/parse_histogram.py` was not found.")
  }
  path
}


normalize_text_input <- function(text) {
  trimws(enc2utf8(text %||% ""))
}


extract_numeric_tokens <- function(text) {
  clean <- normalize_text_input(text)
  if (!nzchar(clean)) {
    return(numeric())
  }

  clean <- gsub("^c\\(|\\)$", "", clean)
  clean <- gsub("[\\[\\]]", "", clean)
  matches <- gregexpr("-?\\d+(?:\\.\\d+)?", clean, perl = TRUE)
  values <- regmatches(clean, matches)[[1]]
  as.numeric(values)
}


parse_rating_vector <- function(text) {
  values <- extract_numeric_tokens(text)
  if (!length(values)) {
    stop("Enter at least two ratings.")
  }

  if (any(!is.finite(values)) || any(values < 1 | values > 5)) {
    stop("Ratings must be numeric and between 1 and 5.")
  }

  values
}


parse_histogram_counts <- function(count_1, count_2, count_3, count_4, count_5) {
  counts <- as.integer(c(count_1, count_2, count_3, count_4, count_5))
  if (any(is.na(counts)) || any(counts < 0)) {
    stop("Histogram counts must be non-negative integers.")
  }
  if (sum(counts) < 2) {
    stop("Enter at least two ratings across the histogram bins.")
  }
  counts
}


expand_histogram_counts <- function(counts) {
  rep(1:5, times = counts)
}


collect_place_data <- function(label, mode, vector_text, histogram_counts) {
  if (identical(mode, "vector")) {
    ratings <- parse_rating_vector(vector_text)
    source <- "Raw vector"
  } else if (identical(mode, "screenshot")) {
    counts <- do.call(parse_histogram_counts, as.list(histogram_counts))
    ratings <- expand_histogram_counts(counts)
    source <- "Histogram screenshot"
  } else {
    counts <- do.call(parse_histogram_counts, as.list(histogram_counts))
    ratings <- expand_histogram_counts(counts)
    source <- "Histogram counts"
  }

  data.frame(
    group = label,
    rating = ratings,
    stringsAsFactors = FALSE
  ) |>
    transform(
      place = paste0("Item ", label),
      source = source
    )
}


summarize_group <- function(ratings_df) {
  group_levels <- levels(ratings_df$group)
  do.call(
    rbind,
    lapply(group_levels, function(group_name) {
      subset_df <- ratings_df[ratings_df$group == group_name, , drop = FALSE]
      data.frame(
        group = group_name,
        n = nrow(subset_df),
        mean = mean(subset_df$rating),
        sd = stats::sd(subset_df$rating),
        stringsAsFactors = FALSE
      )
    })
  )
}


frequentist_analysis <- function(ratings_df) {
  split_ratings <- split(ratings_df$rating, ratings_df$group)
  if (length(split_ratings) != 2 || any(lengths(split_ratings) < 2)) {
    stop("Each item needs at least two ratings for the frequentist test.")
  }

  test <- t.test(rating ~ group, data = ratings_df)

  list(
    group_summary = summarize_group(ratings_df),
    estimate = unname(test$estimate),
    statistic = unname(test$statistic),
    p_value = test$p.value,
    conf_int = unname(test$conf.int),
    method = test$method,
    difference_in_means = unname(test$estimate[2] - test$estimate[1])
  )
}

r_string_literal <- function(x) {
  escaped <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
  escaped <- gsub("\"", "\\\\\"", escaped, perl = TRUE)
  paste0("\"", escaped, "\"")
}


format_r_vector <- function(values) {
  paste0("c(", paste(values, collapse = ", "), ")")
}


build_local_model_code <- function(ratings_df, label_a, label_b, url_a = NULL, url_b = NULL) {
  ratings_a <- ratings_df$rating[ratings_df$group == "A"]
  ratings_b <- ratings_df$rating[ratings_df$group == "B"]

  lines <- c(
    "# Copy-paste into your local R session and fit any model you want.",
    sprintf("label_a <- %s", r_string_literal(label_a)),
    sprintf("label_b <- %s", r_string_literal(label_b))
  )

  if (!is.null(url_a) && nzchar(url_a)) {
    lines <- c(lines, sprintf("url_a <- %s", r_string_literal(url_a)))
  }
  if (!is.null(url_b) && nzchar(url_b)) {
    lines <- c(lines, sprintf("url_b <- %s", r_string_literal(url_b)))
  }

  lines <- c(
    lines,
    sprintf("ratings_a <- %s", format_r_vector(ratings_a)),
    sprintf("ratings_b <- %s", format_r_vector(ratings_b)),
    "",
    "ratings_df <- data.frame(",
    "  group = c(rep(label_a, length(ratings_a)), rep(label_b, length(ratings_b))),",
    "  rating = c(ratings_a, ratings_b)",
    ")",
    "",
    "# Example frequentist check",
    "stats::t.test(rating ~ group, data = ratings_df)",
    "",
    "# Add your own model below.",
    "# For example: lm(rating ~ group, data = ratings_df)"
  )

  paste(lines, collapse = "\n")
}


build_ratings_df <- function(input) {
  a <- collect_place_data(
    label = "A",
    mode = input$mode_a,
    vector_text = input$vector_a,
    histogram_counts = c(
      input$count_a_1,
      input$count_a_2,
      input$count_a_3,
      input$count_a_4,
      input$count_a_5
    )
  )

  b <- collect_place_data(
    label = "B",
    mode = input$mode_b,
    vector_text = input$vector_b,
    histogram_counts = c(
      input$count_b_1,
      input$count_b_2,
      input$count_b_3,
      input$count_b_4,
      input$count_b_5
    )
  )

  out <- rbind(a, b)
  out$group <- factor(out$group, levels = c("A", "B"))
  out
}


parse_json_line <- function(output_lines) {
  output_lines <- iconv(output_lines, from = "", to = "UTF-8", sub = "byte")
  json_lines <- output_lines[grepl("^\\s*\\{", output_lines, useBytes = TRUE)]
  if (!length(json_lines)) {
    stop(
      "The parser did not return JSON.\n\nRaw output:\n",
      paste(output_lines, collapse = "\n")
    )
  }
  fromJSON(json_lines[length(json_lines)], simplifyVector = TRUE)
}


parse_histogram_screenshot <- function(path, average_rating = NULL, total_reviews = NULL) {
  python_bin <- find_python_binary()
  script_path <- python_parser_path()
  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  ext <- tools::file_ext(source_path)
  if (!nzchar(ext)) {
    ext <- "png"
  }
  ascii_copy <- tempfile(pattern = "whichisbetter_upload_", fileext = paste0(".", ext))
  ok <- file.copy(source_path, ascii_copy, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(ok)) {
    stop("The app could not prepare the uploaded screenshot for parsing.")
  }
  on.exit(unlink(ascii_copy, force = TRUE), add = TRUE)

  args <- c(script_path, normalizePath(ascii_copy, winslash = "/", mustWork = TRUE))

  if (!is.null(average_rating) && length(average_rating) && is.finite(average_rating)) {
    args <- c(args, "--average-rating", sprintf("%.3f", as.numeric(average_rating)))
  }

  if (!is.null(total_reviews) && length(total_reviews) && is.finite(total_reviews)) {
    args <- c(args, "--total-reviews", as.character(as.integer(total_reviews)))
  }

  output <- system2(
    command = python_bin,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L)) {
    stop(
      "The screenshot parser failed.\n\nOutput:\n",
      paste(output, collapse = "\n")
    )
  }

  result <- parse_json_line(output)
  result$raw_output <- enc2utf8(output)
  result
}


analysis_db_path <- function() {
  custom_dir <- trimws(Sys.getenv("WHICHISBETTER_DATA_DIR", unset = ""))
  data_dir <- if (nzchar(custom_dir)) custom_dir else file.path(getwd(), "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  normalizePath(file.path(data_dir, "which_is_better.sqlite"), winslash = "/", mustWork = FALSE)
}


initialize_history_db <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    return(FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), analysis_db_path())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS analyses (",
      "id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "created_at TEXT NOT NULL,",
      "label_a TEXT,",
      "url_a TEXT,",
      "mode_a TEXT,",
      "input_payload_a TEXT,",
      "label_b TEXT,",
      "url_b TEXT,",
      "mode_b TEXT,",
      "input_payload_b TEXT,",
      "n_a INTEGER,",
      "mean_a REAL,",
      "n_b INTEGER,",
      "mean_b REAL,",
      "difference_in_means REAL,",
      "t_statistic REAL,",
      "p_value REAL,",
      "ci_low REAL,",
      "ci_high REAL",
      ")"
    )
  )

  TRUE
}


save_analysis_snapshot <- function(item_a, item_b, group_summary, frequentist) {
  if (!initialize_history_db()) {
    return(FALSE)
  }

  summary_a <- group_summary[group_summary$group == "A", , drop = FALSE]
  summary_b <- group_summary[group_summary$group == "B", , drop = FALSE]

  con <- DBI::dbConnect(RSQLite::SQLite(), analysis_db_path())
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- data.frame(
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    label_a = item_a$label %||% NA_character_,
    url_a = item_a$url %||% NA_character_,
    mode_a = item_a$mode %||% NA_character_,
    input_payload_a = jsonlite::toJSON(item_a$payload, auto_unbox = TRUE, null = "null"),
    label_b = item_b$label %||% NA_character_,
    url_b = item_b$url %||% NA_character_,
    mode_b = item_b$mode %||% NA_character_,
    input_payload_b = jsonlite::toJSON(item_b$payload, auto_unbox = TRUE, null = "null"),
    n_a = if (nrow(summary_a)) summary_a$n[[1]] else NA_integer_,
    mean_a = if (nrow(summary_a)) summary_a$mean[[1]] else NA_real_,
    n_b = if (nrow(summary_b)) summary_b$n[[1]] else NA_integer_,
    mean_b = if (nrow(summary_b)) summary_b$mean[[1]] else NA_real_,
    difference_in_means = frequentist$difference_in_means %||% NA_real_,
    t_statistic = frequentist$statistic %||% NA_real_,
    p_value = frequentist$p_value %||% NA_real_,
    ci_low = frequentist$conf_int[[1]] %||% NA_real_,
    ci_high = frequentist$conf_int[[2]] %||% NA_real_,
    stringsAsFactors = FALSE
  )

  DBI::dbAppendTable(con, "analyses", row)
  TRUE
}
