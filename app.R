library(shiny)
library(bslib)

source("R/helpers.R", local = TRUE)


display_label <- function(value, fallback) {
  trimmed <- trimws(value %||% "")
  if (nzchar(trimmed)) trimmed else fallback
}


display_url <- function(value) {
  trimmed <- trimws(value %||% "")
  if (nzchar(trimmed)) trimmed else NULL
}


default_vector_value <- function(side) {
  if (identical(side, "a")) {
    "c(5,5,5,5,5,5,5,5,4)"
  } else {
    "c(1,4,1,5,4,5,5,5,4,4)"
  }
}


optional_number <- function(value, integer = FALSE) {
  if (is.null(value) || !length(value)) {
    return(NULL)
  }

  text <- trimws(as.character(value[[1]]))
  if (!nzchar(text)) {
    return(NULL)
  }

  number <- suppressWarnings(as.numeric(text))
  if (!is.finite(number)) {
    return(NULL)
  }

  if (integer) {
    as.integer(round(number))
  } else {
    number
  }
}


counts_ui <- function(prefix) {
  layout_columns(
    col_widths = c(2, 2, 2, 2, 2),
    numericInput(paste0(prefix, "_1"), "1-star", value = 0, min = 0, step = 1),
    numericInput(paste0(prefix, "_2"), "2-star", value = 0, min = 0, step = 1),
    numericInput(paste0(prefix, "_3"), "3-star", value = 0, min = 0, step = 1),
    numericInput(paste0(prefix, "_4"), "4-star", value = 0, min = 0, step = 1),
    numericInput(paste0(prefix, "_5"), "5-star", value = 0, min = 0, step = 1)
  )
}


item_panel <- function(label, side) {
  card(
    full_screen = FALSE,
    card_header(uiOutput(paste0("panel_header_", side))),
    card_body(
      div(
        class = "inline-input-row",
        tags$label(`for` = paste0("label_", side), "Label"),
        div(
          class = "inline-input-box",
          textInput(paste0("label_", side), label = NULL, value = paste("Item", label))
        )
      ),
      div(
        class = "inline-input-row",
        tags$label(`for` = paste0("url_", side), "URL"),
        div(
          class = "inline-input-box inline-input-box-wide",
          textInput(paste0("url_", side), label = NULL, value = "")
        )
      ),
      radioButtons(
        paste0("mode_", side),
        "Input mode",
        choices = c(
          "Histogram counts" = "histogram",
          "Histogram screenshot" = "screenshot",
          "Raw vector" = "vector"
        ),
        selected = "histogram",
        inline = TRUE
      ),
      conditionalPanel(
        sprintf("input.mode_%s === 'screenshot'", side),
        tags$details(
          class = "screenshot-mode-details",
          open = NA,
          tags$summary("Screenshot parsing"),
          fileInput(
            paste0("screenshot_", side),
            "Histogram screenshot",
            accept = c("image/png", "image/jpeg", "image/webp")
          ),
          div(
            class = "parse-hint-grid",
            textInput(
              paste0("overall_rating_", side),
              "Overall rating shown",
              value = "",
              placeholder = "e.g. 4.8"
            ),
            textInput(
              paste0("rating_count_", side),
              "Rating count shown",
              value = "",
              placeholder = "e.g. 24"
            )
          ),
          actionButton(
            paste0("parse_", side),
            "Parse Screenshot",
            class = "btn btn-outline-primary w-100 parse-button"
          ),
          p(
            class = "text-muted compact-note",
            "Parse the screenshot to suggest 1-to-5 counts. Enter the shown average or count above if OCR misses them."
          ),
          uiOutput(paste0("parser_status_", side)),
          uiOutput(paste0("parsed_counts_", side)),
          uiOutput(paste0("screenshot_preview_", side))
        ),
      ),
      conditionalPanel(
        sprintf("input.mode_%s !== 'vector'", side),
        counts_ui(paste0("count_", side)),
        conditionalPanel(
          sprintf("input.mode_%s === 'histogram'", side),
          p(
            class = "text-muted compact-note",
            "Use this when you know the number of 1-star through 5-star ratings."
          )
        ),
        conditionalPanel(
          sprintf("input.mode_%s === 'screenshot'", side),
          p(
            class = "text-muted compact-note",
            "Review and edit the suggested 1-star through 5-star counts before clicking \"Analyze Inputs\"."
          )
        )
      ),
      conditionalPanel(
        sprintf("input.mode_%s === 'vector'", side),
        textAreaInput(
          paste0("vector_", side),
          "Ratings vector",
          rows = 4,
          value = default_vector_value(side)
        ),
        p(
          class = "text-muted compact-note",
          "Enter comma-separated ratings or an R-style vector such as `c(5, 5, 4, 5)`."
        )
      ),
      
    )
  )
}

app_theme <- bs_theme(
  version = 5,
  bg = "#f7f3ee",
  fg = "#1f2937",
  primary = "#2f6b5f",
  secondary = "#6b7280",
  success = "#2f6b5f",
  info = "#dcece7",
  warning = "#e8ddd0",
  danger = "#8c5a3c"
)


ui <- page_sidebar(
  title = "Which Is Better?",
  theme = app_theme,
  tags$head(
    tags$style(HTML("
      :root {
        --wib-ink: #1f2937;
        --wib-deep: #254f46;
        --wib-mid: #2f6b5f;
        --wib-gold: #5f9388;
        --wib-soft: #dcece7;
        --wib-pale: #ede5da;
        --wib-cream: #f7f3ee;
        --wib-panel: #ffffff;
      }
      body.bslib-page-sidebar {
        background: linear-gradient(180deg, #faf8f4 0%, var(--wib-cream) 58%, #efe9e0 100%);
        color: var(--wib-ink);
      }
      .bslib-page-sidebar > .navbar {
        background: rgba(246, 243, 238, 0.92);
        border-bottom: 1px solid rgba(31, 41, 55, 0.08);
        backdrop-filter: blur(10px);
      }
      .bslib-page-sidebar > .navbar .navbar-brand {
        color: var(--wib-ink);
        font-weight: 700;
        letter-spacing: 0.01em;
      }
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(180deg, #f2ece4 0%, #ebe3d8 100%);
        border-right: 1px solid rgba(31, 41, 55, 0.08);
      }
      .app-shell .card {
        border-radius: 18px;
        border: 1px solid rgba(31, 41, 55, 0.08);
        box-shadow: 0 14px 34px rgba(15, 23, 42, 0.06);
        background: var(--wib-panel);
      }
      .app-shell .card-header {
        background: #f6f1ea;
        border-bottom: 1px solid rgba(31, 41, 55, 0.06);
        font-weight: 600;
        font-size: 1.05rem;
        letter-spacing: 0.01em;
      }
      .app-shell {
        width: 100%;
        max-width: 1480px;
        margin: 0 auto;
        box-sizing: border-box;
        padding-left: clamp(24px, 5vw, 88px);
        padding-right: clamp(24px, 5vw, 88px);
        padding-top: 0.25rem;
        padding-bottom: 1.75rem;
      }
      .bslib-page-sidebar > .navbar .container-fluid {
        width: 100%;
        max-width: 1480px;
        margin: 0 auto;
        box-sizing: border-box;
        padding-left: clamp(24px, 5vw, 88px);
        padding-right: clamp(24px, 5vw, 88px);
      }
      .app-shell .item-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(430px, 1fr));
        gap: 1rem;
        align-items: start;
      }
      .app-shell .parse-hint-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 0.75rem;
        margin-bottom: 0.75rem;
      }
      .app-shell details.screenshot-mode-details {
        background: #faf7f3;
        border: 1px solid rgba(31, 41, 55, 0.08);
        border-radius: 14px;
        padding: 0.9rem 1rem;
        margin-bottom: 0.9rem;
      }
      .app-shell details.screenshot-mode-details > summary {
        cursor: pointer;
        font-weight: 600;
        list-style: none;
      }
      .app-shell details.screenshot-mode-details > summary::-webkit-details-marker {
        display: none;
      }
      .app-shell details.screenshot-mode-details[open] > summary {
        margin-bottom: 0.9rem;
      }
      .app-shell .inline-input-row {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        margin-bottom: 0.75rem;
      }
      .app-shell .inline-input-row > label {
        margin: 0;
        white-space: nowrap;
        font-weight: 600;
      }
      .app-shell .inline-input-box {
        flex: 0 1 220px;
      }
      .app-shell .inline-input-box-wide {
        flex: 1 1 420px;
      }
      .app-shell .inline-input-box .form-group {
        margin-bottom: 0;
      }
      .app-shell .parse-button {
        margin-bottom: 0.6rem;
      }
      .app-shell .btn-primary,
      .app-shell .btn-outline-primary:hover,
      .app-shell .btn-outline-primary:focus,
      .app-shell .btn-outline-primary:active {
        background: linear-gradient(180deg, #3d7c6f 0%, var(--wib-mid) 100%);
        border-color: var(--wib-mid);
        color: #fcfbf8;
      }
      .app-shell .btn-outline-primary {
        background: #f8f6f2;
        border-color: rgba(47, 107, 95, 0.3);
        color: var(--wib-deep);
      }
      .app-shell .btn-primary:hover,
      .app-shell .btn-primary:focus,
      .app-shell .btn-primary:active {
        background: linear-gradient(180deg, #356e63 0%, #265a50 100%);
        border-color: #265a50;
        color: #fcfbf8;
      }
      .app-shell .btn-report {
        background: linear-gradient(180deg, #335f55 0%, #26463f 100%);
        border-color: #1f3933;
        color: #f7fff2;
      }
      .app-shell .btn-report:hover,
      .app-shell .btn-report:focus,
      .app-shell .btn-report:active {
        background: linear-gradient(180deg, #2c544c 0%, #1f3933 100%);
        border-color: #18302a;
        color: #f7fff2;
      }
      .app-shell .form-control:focus,
      .app-shell .form-select:focus,
      .app-shell .shiny-input-container input:focus,
      .app-shell .shiny-input-container textarea:focus {
        border-color: var(--wib-gold);
        box-shadow: 0 0 0 0.2rem rgba(255, 229, 31, 0.2);
      }
      .app-shell .form-control,
      .app-shell .shiny-input-container input,
      .app-shell .shiny-input-container textarea {
        border-color: rgba(31, 41, 55, 0.14);
        background: #fbfaf8;
      }
      .app-shell .form-check-input:checked,
      .app-shell input[type='radio']:checked,
      .app-shell input[type='checkbox']:checked {
        background-color: var(--wib-mid);
        border-color: var(--wib-mid);
      }
      .app-shell .progress-bar,
      .app-shell .shiny-file-input-progress .progress-bar {
        background: linear-gradient(90deg, #dcece7 0%, var(--wib-mid) 100%);
        color: #1f2937;
        text-shadow: none;
      }
      .app-shell .btn-file,
      .app-shell .input-group-btn .btn,
      .app-shell .input-group-prepend .btn {
        background: #f3ece2;
        border-color: rgba(31, 41, 55, 0.12);
        color: #5f4a2d;
      }
      .app-shell .compact-note {
        margin-bottom: 0.75rem;
      }
      .app-shell .screenshot-hidden-note {
        margin-top: -0.15rem;
      }
      .app-shell .summary-headline {
        font-size: 1.55rem;
        font-weight: 700;
        line-height: 1.2;
        margin-bottom: 0.55rem;
      }
      .app-shell .summary-subline,
      .app-shell .summary-statline,
      .app-shell .summary-item-line {
        margin: 0 0 0.45rem 0;
      }
      .app-shell .summary-statline {
        font-size: 0.98rem;
      }
      .app-shell .report-body p:last-child {
        margin-bottom: 0;
      }
      .app-shell .report-export-row {
        display: flex;
        flex-wrap: wrap;
        gap: 0.85rem;
        align-items: center;
        margin-top: 1rem;
      }
      .app-shell .report-export-label {
        font-weight: 600;
        margin: 0;
      }
      .app-shell .report-export-row .btn,
      .app-shell .report-export-row .shiny-download-link {
        white-space: nowrap;
        margin: 0;
      }
      .app-shell .details-note {
        margin-bottom: 0.75rem;
      }
      .app-shell .details-stack {
        display: grid;
        gap: 0.85rem;
      }
      .app-shell details.result-details {
        background: #fbf8f4;
        border: 1px solid rgba(31, 41, 55, 0.08);
        border-radius: 14px;
        padding: 0.9rem 1rem;
      }
      .app-shell details.result-details > summary {
        cursor: pointer;
        font-weight: 600;
        list-style: none;
        font-size: 1.05rem;
        letter-spacing: 0.01em;
      }
      .app-shell details.result-details > summary::-webkit-details-marker {
        display: none;
      }
      .app-shell details.result-details[open] > summary {
        margin-bottom: 0.9rem;
      }
      .app-shell .code-export {
        width: 100%;
        min-height: 18rem;
        resize: vertical;
        border: 1px solid rgba(31, 41, 55, 0.1);
        border-radius: 12px;
        padding: 0.85rem 1rem;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 0.92rem;
        line-height: 1.5;
        color: #243240;
        background: #f8f6f2;
      }
      .app-shell .parser-result {
        margin-top: 0.5rem;
        padding: 0.85rem 1rem;
        border-radius: 14px;
        background: #eef5f3;
        border: 1px solid rgba(47, 107, 95, 0.18);
      }
      .app-shell .parser-meta-line {
        margin: 0 0 0.3rem 0;
        font-size: 0.95rem;
      }
      .app-shell .alert-warning {
        background: #f6eee2;
        border-color: rgba(47, 107, 95, 0.2);
        color: #5e4930;
      }
      .shiny-notification,
      .shiny-notification-message,
      .shiny-notification-warning,
      .shiny-notification-error {
        background: #faf6f0;
        border: 1px solid rgba(31, 41, 55, 0.12);
        color: #3d3121;
      }
      .shiny-notification-close {
        color: #6b5a45;
      }
      @media (max-width: 1100px) {
        .app-shell .item-grid {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 640px) {
        .app-shell .inline-input-row {
          flex-wrap: wrap;
          align-items: flex-start;
        }
        .app-shell .inline-input-box {
          flex: 1 1 100%;
          min-width: 0;
        }
      }
    "))
  ),
  class = "app-shell",
  sidebar = sidebar(
    actionButton("analyze_btn", "Analyze Inputs", class = "btn-primary w-100"),
    actionButton("report_btn", "Generate Report", class = "btn btn-report w-100"),
    h5("How It Works"),
    tags$ul(
      tags$li("Choose between \"Histogram counts\", \"Histogram screenshot\", or \"Raw vector\"."),
      tags$li("If you use screenshot mode, open \"Screenshot parsing\", upload the image, and then review the suggested counts."),
      tags$li("If you manually change \"Overall rating shown\" or \"Rating count shown\", click \"Parse Screenshot\" again so those corrections can update the suggested counts."),
      tags$li("Use raw rating vectors only when you already have the full list of scores."),
      tags$li("Click \"Analyze Inputs\" to get a plain-language answer, confidence interval, and an R code export for your own local modeling."),
      tags$li("Labels and pasted URLs are saved locally whenever you analyze a comparison.")
    )
  ),
  div(
    class = "item-grid",
    item_panel("A", "a"),
    item_panel("B", "b")
  ),
  card(
    fill = FALSE,
    card_header("Result"),
    card_body(
      fill = FALSE,
      fillable = FALSE,
      uiOutput("headline_result")
    )
  ),
  card(
    fill = FALSE,
    card_header("Generated Report"),
    card_body(
      fill = FALSE,
      fillable = FALSE,
      uiOutput("generated_report"),
      uiOutput("report_export_controls")
    )
  ),
  div(
    class = "details-stack",
    tags$details(
      class = "result-details",
      tags$summary("Frequentist Details"),
      p(
        class = "details-note text-muted",
        "This section keeps the formal numbers in one place after the plain-language result above."
      ),
      uiOutput("frequentist_line"),
      tableOutput("group_summary")
    ),
    uiOutput("distribution_section"),
    tags$details(
      class = "result-details",
      tags$summary("R Code for Local Modeling"),
      p(
        class = "details-note text-muted",
        "Copy this into your own local R session if you want to fit a custom model outside the app."
      ),
      uiOutput("local_model_code")
    )
  )
)


server <- function(input, output, session) {
  initialize_history_db()

  parser_status_a <- reactiveVal("No screenshot parsed yet.")
  parser_status_b <- reactiveVal("No screenshot parsed yet.")
  parsed_counts_a <- reactiveVal(NULL)
  parsed_counts_b <- reactiveVal(NULL)
  analysis_requested <- reactiveVal(FALSE)
  analysis_result <- reactiveVal(NULL)
  analysis_error <- reactiveVal(NULL)
  distribution_open <- reactiveVal(FALSE)
  generated_report <- reactiveVal(NULL)
  result_ui <- reactiveVal(NULL)
  report_ui <- reactiveVal(NULL)

  item_label <- function(side) {
    fallback <- if (identical(side, "a")) "Item A" else "Item B"
    display_label(input[[paste0("label_", side)]], fallback)
  }

  item_url <- function(side) {
    display_url(input[[paste0("url_", side)]])
  }

  resolved_vector_text <- function(side) {
    value <- input[[paste0("vector_", side)]] %||% default_vector_value(side)
    if (!length(value) || is.null(value[[1]])) {
      default_vector_value(side)
    } else {
      as.character(value[[1]])
    }
  }

  resolved_histogram_counts <- function(side) {
    input_counts <- as.integer(vapply(1:5, function(star) {
      value <- input[[paste0("count_", side, "_", star)]] %||% 0
      if (!length(value)) {
        return(0)
      }
      number <- suppressWarnings(as.numeric(value[[1]]))
      if (is.na(number) || !is.finite(number)) {
        return(0)
      }
      number
    }, numeric(1)))

    mode <- input[[paste0("mode_", side)]]
    parsed_result <- if (identical(side, "a")) parsed_counts_a() else parsed_counts_b()
    parsed_counts <- NULL
    if (!is.null(parsed_result) && isTRUE(parsed_result$success) && length(parsed_result$counts_1_to_5) == 5) {
      parsed_counts <- as.integer(parsed_result$counts_1_to_5)
    }

    input_valid <- !any(is.na(input_counts)) && all(input_counts >= 0) && sum(input_counts) >= 2
    if (identical(mode, "screenshot") && !input_valid && !is.null(parsed_counts)) {
      return(parsed_counts)
    }

    input_counts
  }

  build_current_ratings_df <- function() {
    a <- collect_place_data(
      label = "A",
      mode = input$mode_a,
      vector_text = resolved_vector_text("a"),
      histogram_counts = resolved_histogram_counts("a")
    )

    b <- collect_place_data(
      label = "B",
      mode = input$mode_b,
      vector_text = resolved_vector_text("b"),
      histogram_counts = resolved_histogram_counts("b")
    )

    out <- rbind(a, b)
    out$group <- factor(out$group, levels = c("A", "B"))
    out
  }

  item_snapshot <- function(side) {
    fallback <- if (identical(side, "a")) "Item A" else "Item B"
    mode <- input[[paste0("mode_", side)]] %||% "histogram"
    payload <- list(
      displayed_overall_rating = optional_number(input[[paste0("overall_rating_", side)]]),
      displayed_rating_count = optional_number(input[[paste0("rating_count_", side)]], integer = TRUE)
    )

    if (identical(mode, "histogram")) {
      payload$histogram_counts <- resolved_histogram_counts(side)
    } else if (identical(mode, "screenshot")) {
      payload$histogram_counts <- resolved_histogram_counts(side)
    } else {
      payload$ratings_vector <- normalize_text_input(resolved_vector_text(side))
    }

    list(
      label = display_label(input[[paste0("label_", side)]], fallback),
      url = item_url(side),
      mode = mode,
      payload = payload
    )
  }

  render_panel_header <- function(label, side) {
    renderUI({
      tags$span(
        sprintf("Item %s", label),
        if (nzchar(trimws(input[[paste0("label_", side)]] %||% ""))) {
          tags$span(paste0(": ", input[[paste0("label_", side)]]))
        }
      )
    })
  }

  output$panel_header_a <- render_panel_header("A", "a")
  output$panel_header_b <- render_panel_header("B", "b")

  render_screenshot <- function(file_input) {
    renderUI({
      req(file_input())
      encoded <- base64enc::dataURI(
        file = file_input()$datapath,
        mime = file_input()$type %||% "image/png"
      )
      tags$img(
        src = encoded,
        alt = "Uploaded histogram screenshot",
        style = "max-width: 100%; border-radius: 12px; border: 1px solid #d9e3e6; display: block;"
      )
    })
  }

  output$screenshot_preview_a <- render_screenshot(reactive(input$screenshot_a))
  output$screenshot_preview_b <- render_screenshot(reactive(input$screenshot_b))

  output$parser_status_a <- renderUI({
    tags$p(class = "text-muted", parser_status_a())
  })

  output$parser_status_b <- renderUI({
    tags$p(class = "text-muted", parser_status_b())
  })

  render_parsed_counts <- function(parsed_result) {
    renderUI({
      result <- parsed_result()
      req(result, isTRUE(result$success), length(result$counts_1_to_5) == 5)
      counts <- as.integer(result$counts_1_to_5)
      total <- sum(counts)
      implied_mean <- if (total > 0) sum((1:5) * counts) / total else NA_real_
      shown_mean <- result$used_average_rating
      shown_total <- result$used_total_reviews
      tags$div(
        class = "parser-result",
        tags$strong("Suggested histogram applied"),
        if (!is.null(shown_mean) && !is.na(shown_mean)) {
          tags$p(
            class = "parser-meta-line",
            sprintf("Overall rating shown: %.1f", shown_mean)
          )
        },
        if (!is.null(shown_total) && !is.na(shown_total)) {
          tags$p(
            class = "parser-meta-line",
            sprintf("Rating count shown: %d", as.integer(shown_total))
          )
        },
        tags$p(
          style = "margin: 0.45rem 0 0.25rem 0;",
          paste(sprintf("%s-star: %s", 1:5, counts), collapse = " | ")
        ),
        tags$p(
          class = "text-muted",
          style = "margin: 0;",
          sprintf("Implied mean %.2f across %d ratings.", implied_mean, total)
        )
      )
    })
  }

  output$parsed_counts_a <- render_parsed_counts(parsed_counts_a)
  output$parsed_counts_b <- render_parsed_counts(parsed_counts_b)

  reset_screenshot_state <- function(side) {
    parsed_store <- if (identical(side, "a")) parsed_counts_a else parsed_counts_b
    status_store <- if (identical(side, "a")) parser_status_a else parser_status_b

    parsed_store(NULL)
    status_store("New screenshot uploaded. Click \"Parse Screenshot\" to estimate the new counts.")
    for (i in seq_len(5)) {
      updateNumericInput(session, paste0("count_", side, "_", i), value = 0)
    }
    updateTextInput(session, paste0("overall_rating_", side), value = "")
    updateTextInput(session, paste0("rating_count_", side), value = "")
    distribution_open(FALSE)
  }

  build_report <- function(analyzed) {
    f <- analyzed$frequentist
    label_a <- analyzed$labels[["A"]]
    label_b <- analyzed$labels[["B"]]
    item_a <- analyzed$item_a
    item_b <- analyzed$item_b
    summary_a <- f$group_summary[f$group_summary$group == "A", , drop = FALSE]
    summary_b <- f$group_summary[f$group_summary$group == "B", , drop = FALSE]
    diff <- f$difference_in_means

    winner_sentence <- if (f$p_value < 0.05) {
      if (diff > 0) {
        sprintf("%s comes out ahead in this sample.", label_b)
      } else if (diff < 0) {
        sprintf("%s comes out ahead in this sample.", label_a)
      } else {
        "The two items come out tied in this sample."
      }
    } else {
      "This sample does not show a clear winner."
    }

    caution_sentence <- if (f$p_value < 0.05) {
      sprintf(
        "The estimated gap is %.2f points on the 1-to-5 scale, with a 95%% confidence interval from %.2f to %.2f.",
        diff,
        f$conf_int[1],
        f$conf_int[2]
      )
    } else {
      sprintf(
        "The estimated gap is %.2f points, but the 95%% confidence interval from %.2f to %.2f still includes no difference.",
        diff,
        f$conf_int[1],
        f$conf_int[2]
      )
    }

    item_detail_lines <- function(item, group_summary_row) {
      mode_value <- item$mode %||% ""
      mode_value <- if (length(mode_value)) as.character(mode_value[[1]]) else ""
      mode_label <- if (identical(mode_value, "histogram")) {
        "Histogram counts"
      } else if (identical(mode_value, "screenshot")) {
        "Histogram screenshot"
      } else if (identical(mode_value, "vector")) {
        "Raw vector"
      } else {
        "Unknown"
      }

      lines <- c(
        sprintf("Label: %s", item$label),
        sprintf("Input mode: %s", mode_label)
      )

      if (!is.null(item$url) && nzchar(item$url)) {
        lines <- c(lines, sprintf("URL: %s", item$url))
      }

      if (!is.null(item$payload$displayed_overall_rating)) {
        lines <- c(lines, sprintf("Displayed overall rating: %.1f", item$payload$displayed_overall_rating))
      }
      if (!is.null(item$payload$displayed_rating_count)) {
        lines <- c(lines, sprintf("Displayed rating count: %d", item$payload$displayed_rating_count))
      }

      if (!is.null(item$payload$histogram_counts)) {
        counts <- as.integer(item$payload$histogram_counts)
        lines <- c(
          lines,
          sprintf("Histogram counts (1-star to 5-star): %s", paste(counts, collapse = ", ")),
          sprintf("Reconstructed sample size: %d", sum(counts))
        )
      }

      if (!is.null(item$payload$ratings_vector)) {
        vector_values <- parse_rating_vector(item$payload$ratings_vector)
        lines <- c(
          lines,
          sprintf("Raw vector length: %d", length(vector_values)),
          sprintf("Raw vector preview: %s", paste(head(vector_values, 12), collapse = ", "))
        )
      }

      lines <- c(
        lines,
        sprintf("Mean: %.3f", group_summary_row$mean[[1]]),
        sprintf("SD: %.3f", group_summary_row$sd[[1]])
      )

      lines
    }

    local_model_code <- build_local_model_code(
      ratings_df = analyzed$ratings_df,
      label_a = label_a,
      label_b = label_b,
      url_a = item_a$url,
      url_b = item_b$url
    )

    summary_paragraphs <- c(
      sprintf(
        "%s %s has an average rating of %.2f from %d ratings, while %s averages %.2f from %d ratings.",
        winner_sentence,
        label_a,
        summary_a$mean[[1]],
        summary_a$n[[1]],
        label_b,
        summary_b$mean[[1]],
        summary_b$n[[1]]
      ),
      caution_sentence
    )

    list(
      title = "Which Is Better?",
      summary_paragraphs = summary_paragraphs,
      export_sections = list(
        list(
          title = "Formal Frequentist Result",
          lines = c(
            sprintf("Welch two-sample t-test"),
            sprintf("Difference in means (B - A): %.4f", diff),
            sprintf("95%% confidence interval: [%.4f, %.4f]", f$conf_int[1], f$conf_int[2]),
            sprintf("p-value: %.6f", f$p_value),
            sprintf("t statistic: %.4f", f$statistic)
          )
        ),
        list(
          title = sprintf("Input Details: %s", label_a),
          lines = item_detail_lines(item_a, summary_a)
        ),
        list(
          title = sprintf("Input Details: %s", label_b),
          lines = item_detail_lines(item_b, summary_b)
        ),
        list(
          title = "Reconstructed R Code",
          lines = strsplit(local_model_code, "\n", fixed = TRUE)[[1]],
          preformatted = TRUE
        )
      )
    )
  }

  build_result_ui <- function(analyzed) {
    f <- analyzed$frequentist
    label_a <- analyzed$labels[["A"]]
    label_b <- analyzed$labels[["B"]]
    summary_a <- f$group_summary[f$group_summary$group == "A", , drop = FALSE]
    summary_b <- f$group_summary[f$group_summary$group == "B", , drop = FALSE]

    diff <- f$difference_in_means
    headline <- if (isTRUE(f$p_value < 0.05)) {
      if (diff > 0) {
        sprintf("%s is rated higher in this sample.", label_b)
      } else if (diff < 0) {
        sprintf("%s is rated higher in this sample.", label_a)
      } else {
        "The two items are effectively tied in this sample."
      }
    } else {
      "This sample does not show a clear difference."
    }

    subline <- sprintf(
      "%s averages %.2f from %d ratings; %s averages %.2f from %d ratings.",
      label_a,
      summary_a$mean[[1]],
      summary_a$n[[1]],
      label_b,
      summary_b$mean[[1]],
      summary_b$n[[1]]
    )

    statline <- sprintf(
      "Difference (B - A) %.2f | 95%% CI [%.2f, %.2f] | p = %.4f | t = %.2f",
      diff,
      f$conf_int[1],
      f$conf_int[2],
      f$p_value,
      f$statistic
    )

    item_lines <- c(
      sprintf("%s: mean %.2f from %d ratings.", label_a, summary_a$mean[[1]], summary_a$n[[1]]),
      sprintf("%s: mean %.2f from %d ratings.", label_b, summary_b$mean[[1]], summary_b$n[[1]])
    )

    tags$div(
      tags$p(class = "summary-headline", headline),
      tags$p(class = "summary-subline", subline),
      tags$p(class = "summary-statline", statline),
      lapply(item_lines, function(line) tags$p(class = "summary-item-line text-muted", line))
    )
  }

  build_report_ui <- function(report) {
    if (is.null(report)) {
      return(
        tags$p(
          class = "text-muted",
          "Click \"Generate Report\" after analyzing the inputs to create a short plain-language summary."
        )
      )
    }

    tags$div(
      class = "report-body",
      tags$p(class = "summary-headline", report$title),
      lapply(report$summary_paragraphs %||% report$paragraphs %||% character(), tags$p)
    )
  }

  compute_analysis_result <- function() {
    ratings_df <- build_current_ratings_df()
    label_a <- item_label("a")
    label_b <- item_label("b")

    ratings_df$place <- c(
      rep(label_a, sum(ratings_df$group == "A")),
      rep(label_b, sum(ratings_df$group == "B"))
    )

    frequentist <- frequentist_analysis(ratings_df)

    save_analysis_snapshot(
      item_a = item_snapshot("a"),
      item_b = item_snapshot("b"),
      group_summary = frequentist$group_summary,
      frequentist = frequentist
    )

    list(
      ratings_df = ratings_df,
      frequentist = frequentist,
      labels = c(A = label_a, B = label_b),
      item_a = item_snapshot("a"),
      item_b = item_snapshot("b")
    )
  }

  run_analysis <- function(success_message = NULL) {
    result <- tryCatch(
      compute_analysis_result(),
      error = function(e) {
        analysis_error(conditionMessage(e))
        NULL
      }
    )

    if (is.null(result)) {
      analysis_result(NULL)
      result_ui(
        tags$div(
          class = "alert alert-warning",
          tags$strong("The app could not analyze these inputs yet. "),
          analysis_error() %||% "The analysis could not run with the current inputs."
        )
      )
      showNotification(
        analysis_error() %||% "The analysis could not run with the current inputs.",
        type = "error",
        duration = 6
      )
      return(invisible(FALSE))
    }

    analysis_error(NULL)
    analysis_result(result)
    result_ui(build_result_ui(result))
    if (!is.null(success_message)) {
      showNotification(
        success_message,
        type = "message",
        duration = 3
      )
    }
    invisible(TRUE)
  }

  maybe_refresh_analysis <- function(side_label) {
    if (!isTRUE(analysis_requested())) {
      showNotification(
        sprintf(
          "Parsed Item %s and applied the suggested counts. Click \"Analyze Inputs\" to refresh the comparison.",
          side_label
        ),
        type = "message",
        duration = 5
      )
      return(invisible(FALSE))
    }

    refreshed <- run_analysis()

    if (isTRUE(refreshed)) {
      showNotification(
        sprintf("Parsed Item %s and refreshed the comparison.", side_label),
        type = "message",
        duration = 4
      )
    } else {
      showNotification(
        sprintf("Parsed Item %s, but the comparison could not refresh yet.", side_label),
        type = "warning",
        duration = 6
      )
    }

    invisible(refreshed)
  }

  apply_parser_result <- function(result, side) {
    req(isTRUE(result$success))
    counts <- result$counts_1_to_5
    for (i in seq_len(5)) {
      updateNumericInput(session, paste0("count_", side, "_", i), value = counts[[i]])
    }
    if (!is.null(result$used_average_rating) && length(result$used_average_rating) && is.finite(result$used_average_rating)) {
      updateTextInput(
        session,
        paste0("overall_rating_", side),
        value = as.character(signif(as.numeric(result$used_average_rating), digits = 12))
      )
    }
    if (!is.null(result$used_total_reviews) && length(result$used_total_reviews) && is.finite(result$used_total_reviews)) {
      updateTextInput(
        session,
        paste0("rating_count_", side),
        value = as.integer(result$used_total_reviews)
      )
    }

    parts <- c(result$message %||% "Parsed screenshot.")
    if (!is.null(result$used_total_reviews) && !is.na(result$used_total_reviews)) {
      parts <- c(parts, sprintf("Total reviews used: %s.", result$used_total_reviews))
    }
    if (!is.null(result$used_average_rating) && !is.na(result$used_average_rating)) {
      parts <- c(parts, sprintf("Average rating used: %.1f.", result$used_average_rating))
    }
    parts <- c(
      parts,
      sprintf("Suggested counts (1-star to 5-star): %s.", paste(counts, collapse = ", "))
    )
    notes <- result$notes
    if (length(notes)) {
      parts <- c(parts, paste(notes, collapse = " "))
    }
    paste(parts, collapse = " ")
  }

  observeEvent(input$parse_a, {
    req(input$screenshot_a)
    parser_status_a("Parsing screenshot...")
    parsed_counts_a(NULL)
    result <- tryCatch(
      parse_histogram_screenshot(
        path = input$screenshot_a$datapath,
        average_rating = optional_number(input$overall_rating_a),
        total_reviews = optional_number(input$rating_count_a, integer = TRUE)
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )
    if (isTRUE(result$success)) {
      parsed_counts_a(result)
      parser_status_a(apply_parser_result(result, "a"))
      distribution_open(TRUE)
      maybe_refresh_analysis("A")
    } else {
      parser_status_a(result$message %||% "Parser could not estimate counts from this screenshot.")
      parsed_counts_a(NULL)
      showNotification(parser_status_a(), type = "error", duration = 6)
    }
  })

  observeEvent(input$screenshot_a, {
    req(input$screenshot_a)
    reset_screenshot_state("a")
  }, ignoreInit = TRUE)

  observeEvent(input$parse_b, {
    req(input$screenshot_b)
    parser_status_b("Parsing screenshot...")
    parsed_counts_b(NULL)
    result <- tryCatch(
      parse_histogram_screenshot(
        path = input$screenshot_b$datapath,
        average_rating = optional_number(input$overall_rating_b),
        total_reviews = optional_number(input$rating_count_b, integer = TRUE)
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )
    if (isTRUE(result$success)) {
      parsed_counts_b(result)
      parser_status_b(apply_parser_result(result, "b"))
      distribution_open(TRUE)
      maybe_refresh_analysis("B")
    } else {
      parser_status_b(result$message %||% "Parser could not estimate counts from this screenshot.")
      parsed_counts_b(NULL)
      showNotification(parser_status_b(), type = "error", duration = 6)
    }
  })

  observeEvent(input$screenshot_b, {
    req(input$screenshot_b)
    reset_screenshot_state("b")
  }, ignoreInit = TRUE)

  observeEvent(input$analyze_btn, {
    analysis_requested(TRUE)
    analysis_error(NULL)
    generated_report(NULL)
    report_ui(build_report_ui(NULL))
    result_ui(
      tags$div(
        tags$p(class = "summary-headline", "Updating analysis..."),
        tags$p(class = "text-muted", "The app is recomputing the comparison from the current inputs.")
      )
    )
    run_analysis("Analysis updated.")
  })

  observeEvent(input$report_btn, {
    if (!isTRUE(analysis_requested()) || !is.null(analysis_error()) || is.null(analysis_result())) {
      showNotification(
        "Run \"Analyze Inputs\" successfully before generating a report.",
        type = "warning",
        duration = 5
      )
      return()
    }

    report <- build_report(analysis_result())
    generated_report(report)
    report_ui(build_report_ui(report))
    showNotification(
      "Generated a short report from the current analysis.",
      type = "message",
      duration = 4
    )
  })

  current_ratings_data <- reactive({
    tryCatch({
      ratings_df <- build_current_ratings_df()
      ratings_df$place <- c(
        rep(item_label("a"), sum(ratings_df$group == "A")),
        rep(item_label("b"), sum(ratings_df$group == "B"))
      )
      ratings_df
    }, error = function(e) {
      NULL
    })
  })

  output$headline_result <- renderUI({
    result_ui() %||% tags$div(
      tags$p(
        class = "summary-headline",
        "Run an analysis to see a plain-language comparison here."
      ),
      tags$p(
        class = "text-muted",
        "The app will summarize which item looks better, how large the gap is, and how uncertain that difference still is."
      )
    )
  })

  output$generated_report <- renderUI({
    report_ui() %||% build_report_ui(NULL)
  })

  output$report_export_controls <- renderUI({
    req(!is.null(generated_report()))
    div(
      class = "report-export-row",
      tags$p(class = "report-export-label", "Export to:"),
      downloadButton("download_report_html", "HTML", class = "btn btn-outline-primary"),
      downloadButton("download_report_docx", "DOCX", class = "btn btn-outline-primary"),
      downloadButton("download_report_md", "Text (.md)", class = "btn btn-outline-primary"),
      downloadButton("download_report_tex", "TeX", class = "btn btn-outline-primary")
    )
  })

  output$frequentist_line <- renderUI({
    req(analysis_requested())
    req(is.null(analysis_error()))
    analyzed <- analysis_result()
    req(!is.null(analyzed))
    f <- analyzed$frequentist
    tags$p(
      class = "summary-statline",
      sprintf(
        "Difference (B - A) %.2f | 95%% CI [%.2f, %.2f] | p = %.4f | t = %.2f",
        f$difference_in_means,
        f$conf_int[1],
        f$conf_int[2],
        f$p_value,
        f$statistic
      )
    )
  })

  output$group_summary <- renderTable({
    req(analysis_requested())
    req(is.null(analysis_error()))
    analyzed <- analysis_result()
    req(!is.null(analyzed))
    summary <- analyzed$frequentist$group_summary
    summary$group <- c(analyzed$labels[["A"]], analyzed$labels[["B"]])
    summary$mean <- round(summary$mean, 3)
    summary$sd <- round(summary$sd, 3)
    names(summary)[1] <- "item"
    summary
  })

  output$distribution_section <- renderUI({
    ratings_df <- current_ratings_data()
    detail_args <- list(
      class = "result-details",
      tags$summary("Distribution View"),
      p(
        class = "details-note text-muted",
        "This chart shows the star-count distribution implied by your entered histograms or raw rating vectors."
      )
    )

    if (!is.null(ratings_df)) {
      detail_args <- c(
        detail_args,
        list(
          plotOutput("ratings_plot", height = 360),
          tableOutput("ratings_table")
        )
      )
    } else {
      detail_args <- c(
        detail_args,
        list(
          tags$p(
            class = "text-muted",
            "Enter valid histogram counts or raw ratings to preview the distribution here."
          )
        )
      )
    }

    if (isTRUE(distribution_open())) {
      detail_args <- c(list(open = NA), detail_args)
    }

    do.call(tags$details, detail_args)
  })

  output$ratings_plot <- renderPlot({
    ratings_df <- current_ratings_data()
    if (is.null(ratings_df)) {
      plot.new()
      text(
        x = 0.5,
        y = 0.5,
        labels = "Enter valid histogram counts or raw ratings to preview the distribution.",
        cex = 1.05
      )
      return(invisible(NULL))
    }
    rating_levels <- factor(ratings_df$rating, levels = 1:5)
    count_matrix <- table(ratings_df$place, rating_levels)
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5.5, 5, 3.5, 1.2) + 0.1)
    barplot(
      t(count_matrix),
      beside = TRUE,
      col = c("#e7f1ee", "#cfe1dc", "#a9c9c0", "#6fa194", "#2f6b5f"),
      border = "#254f46",
      names.arg = rownames(count_matrix),
      las = 0,
      cex.names = 1.05,
      cex.axis = 1.05,
      cex.lab = 1.15,
      cex.main = 1.18,
      ylab = "Count of ratings",
      xlab = "",
      main = "Star-count distribution"
    )
    legend(
      "topright",
      legend = paste(1:5, "star"),
      fill = c("#e7f1ee", "#cfe1dc", "#a9c9c0", "#6fa194", "#2f6b5f"),
      border = "#254f46",
      bty = "n",
      cex = 1.08
    )
  })

  output$ratings_table <- renderTable({
    ratings_df <- current_ratings_data()
    req(!is.null(ratings_df))
    count_matrix <- table(ratings_df$place, factor(ratings_df$rating, levels = 1:5))
    out <- as.data.frame.matrix(count_matrix)
    out$item <- rownames(out)
    rownames(out) <- NULL
    out <- out[, c("item", "1", "2", "3", "4", "5")]
    names(out) <- c("item", "1-star", "2-star", "3-star", "4-star", "5-star")
    out
  })

  output$local_model_code <- renderUI({
    req(analysis_requested())
    req(is.null(analysis_error()))
    analyzed <- analysis_result()
    req(!is.null(analyzed))
    code <- build_local_model_code(
      ratings_df = analyzed$ratings_df,
      label_a = analyzed$labels[["A"]],
      label_b = analyzed$labels[["B"]],
      url_a = item_url("a"),
      url_b = item_url("b")
    )
    tags$textarea(
      class = "code-export",
      readonly = "readonly",
      code
    )
  })

  build_report_filename <- function(report, extension) {
    paste0(
      sanitize_report_slug(report$title),
      "-",
      format(Sys.Date(), "%Y-%m-%d"),
      ".",
      extension
    )
  }

  output$download_report_html <- downloadHandler(
    filename = function() {
      report <- generated_report()
      req(!is.null(report))
      build_report_filename(report, "html")
    },
    content = function(file) {
      report <- generated_report()
      req(!is.null(report))
      write_report_export(report = report, format = "html", path = file)
    }
  )

  output$download_report_docx <- downloadHandler(
    filename = function() {
      report <- generated_report()
      req(!is.null(report))
      build_report_filename(report, "docx")
    },
    content = function(file) {
      report <- generated_report()
      req(!is.null(report))
      write_report_export(report = report, format = "docx", path = file)
    }
  )

  output$download_report_md <- downloadHandler(
    filename = function() {
      report <- generated_report()
      req(!is.null(report))
      build_report_filename(report, "md")
    },
    content = function(file) {
      report <- generated_report()
      req(!is.null(report))
      write_report_export(report = report, format = "md", path = file)
    }
  )

  output$download_report_tex <- downloadHandler(
    filename = function() {
      report <- generated_report()
      req(!is.null(report))
      build_report_filename(report, "tex")
    },
    content = function(file) {
      report <- generated_report()
      req(!is.null(report))
      write_report_export(report = report, format = "tex", path = file)
    }
  )
}


shinyApp(ui, server)
