# File: shiny-app.R
# Description: Interactive Shiny application for ICCDesign

#' Launch the ICCDesign Shiny Application
#'
#' @description
#' Starts an interactive Shiny application for ICC analysis, reliability
#' reporting, and sample size or power planning.
#'
#' @param host Host address passed to \code{shiny::runApp}. Default is
#'   \code{"127.0.0.1"}.
#' @param port Optional port passed to \code{shiny::runApp}. If \code{NULL},
#'   Shiny selects an available port.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default is \code{interactive()}.
#' @param ... Additional arguments passed to \code{shiny::runApp}.
#'
#' @return Runs the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the interactive Shiny application
#' run_icc_app()
#' }
run_icc_app <- function(host = "127.0.0.1", port = NULL,
                        launch.browser = interactive(), ...) {
  shiny::runApp(
    appDir = icc_shiny_app(),
    host = host,
    port = port,
    launch.browser = launch.browser,
    ...
  )
}

#' Build the ICCDesign Shiny Application
#'
#' @return A Shiny application object.
#' @keywords internal
icc_shiny_app <- function() {
  shiny::shinyApp(ui = .icc_app_ui(), server = .icc_app_server)
}

.icc_app_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$style(shiny::HTML(.icc_app_css()))
    ),
    shiny::div(
      class = "app-shell",
      shiny::div(
        class = "topbar",
        shiny::div(
          class = "brand-block",
          shiny::div(class = "brand-mark", "ICC"),
          shiny::div(
            shiny::h1("ICCDesign"),
            shiny::p("Interactive reliability analysis and study planning")
          )
        ),
        shiny::div(
          class = "topbar-actions",
          shiny::span(class = "status-pill", "Point estimate"),
          shiny::span(class = "status-pill", "Exact CI"),
          shiny::span(class = "status-pill", "Power")
        )
      ),
      shiny::tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("calculator"), "ICC analysis"),
          shiny::div(
            class = "workspace-grid",
            .icc_analysis_controls(),
            shiny::div(
              class = "result-stack",
              shiny::uiOutput("analysis_alert"),
              shiny::uiOutput("analysis_metrics"),
              shiny::div(
                class = "two-col",
                shiny::div(
                  class = "panel",
                  shiny::h3("Reliability interval"),
                  shiny::plotOutput("icc_plot", height = "230px")
                ),
                shiny::div(
                  class = "panel",
                  shiny::h3("Model summary"),
                  shiny::tableOutput("model_summary")
                )
              ),
              shiny::div(
                class = "two-col",
                shiny::div(
                  class = "panel",
                  shiny::h3("ANOVA components"),
                  shiny::tableOutput("anova_table")
                ),
                shiny::div(
                  class = "panel",
                  shiny::h3("Hypothesis tests"),
                  shiny::tableOutput("f_test_table")
                )
              ),
              shiny::div(
                class = "panel",
                shiny::div(
                  class = "panel-heading",
                  shiny::h3("Publication-ready report"),
                  shiny::downloadButton(
                    "download_report",
                    label = "Download"
                  )
                ),
                shiny::verbatimTextOutput("report_text")
              )
            )
          )
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-line"), "Sample size"),
          shiny::div(
            class = "workspace-grid",
            .icc_sample_size_controls(),
            shiny::div(
              class = "result-stack",
              shiny::uiOutput("ss_alert"),
              shiny::uiOutput("ss_metrics"),
              shiny::div(
                class = "two-col",
                shiny::div(
                  class = "panel",
                  shiny::h3("Power curve"),
                  shiny::plotOutput("power_plot", height = "260px")
                ),
                shiny::div(
                  class = "panel",
                  shiny::h3("Planning details"),
                  shiny::tableOutput("ss_details")
                )
              ),
              shiny::div(
                class = "panel",
                shiny::div(
                  class = "panel-heading",
                  shiny::h3("Design export"),
                  shiny::downloadButton(
                    "download_design",
                    label = "Download CSV"
                  )
                ),
                shiny::tableOutput("ss_export_preview")
              )
            )
          )
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("table"), "Data and guide"),
          shiny::div(
            class = "guide-grid",
            shiny::div(
              class = "panel",
              shiny::h3("Current data preview"),
              shiny::tableOutput("data_preview")
            ),
            shiny::div(
              class = "panel",
              shiny::h3("Data diagnostics"),
              shiny::tableOutput("data_diagnostics")
            ),
            shiny::div(
              class = "panel",
              shiny::h3("ICC selection guide"),
              shiny::tableOutput("selection_guide")
            ),
            shiny::div(
              class = "panel",
              shiny::h3("Reliability thresholds"),
              shiny::tableOutput("rating_guide")
            )
          )
        )
      )
    )
  )
}

.icc_analysis_controls <- function() {
  shiny::div(
    class = "control-panel",
    shiny::h2("Analysis setup"),
    shiny::p(class = "muted", "Upload or paste a numeric matrix, then choose the design."),
    shiny::radioButtons(
      "data_mode",
      "Data source",
      choices = c("Example data" = "example", "Upload file" = "upload", "Paste table" = "paste"),
      selected = "example",
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.data_mode == 'example'",
      shiny::div(
        class = "example-note",
        shiny::p(class = "muted", "Uses the fixed built-in icc_data dataset.")
      )
    ),
    shiny::conditionalPanel(
      "input.data_mode == 'upload'",
      shiny::fileInput("data_file", "CSV, TSV, or TXT file", accept = c(".csv", ".tsv", ".txt")),
      shiny::div(
        class = "input-grid",
        shiny::checkboxInput("file_header", "First row contains column names", value = TRUE),
        shiny::selectInput("file_sep", "Separator", choices = .icc_sep_choices(), selected = ","),
        shiny::selectInput("file_dec", "Decimal mark", choices = c("." = ".", "," = ","), selected = ".")
      )
    ),
    shiny::conditionalPanel(
      "input.data_mode == 'paste'",
      shiny::textAreaInput(
        "pasted_data",
        "Paste numeric table",
        value = paste(
          "Rater1,Rater2,Rater3",
          "76,79,77",
          "83,82,85",
          "91,89,90",
          "68,70,67",
          "74,75,76",
          sep = "\n"
        ),
        rows = 7
      ),
      shiny::div(
        class = "input-grid",
        shiny::checkboxInput("pasted_header", "First row contains column names", value = TRUE),
        shiny::selectInput("pasted_sep", "Separator", choices = .icc_sep_choices(), selected = ","),
        shiny::selectInput("pasted_dec", "Decimal mark", choices = c("." = ".", "," = ","), selected = ".")
      )
    ),
    shiny::hr(),
    shiny::h2("Design questions"),
    shiny::radioButtons(
      "same_raters",
      "Do all subjects share the same raters?",
      choices = c("Yes" = "TRUE", "No" = "FALSE"),
      selected = "TRUE",
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.same_raters == 'TRUE'",
      shiny::div(
        class = "input-grid",
        shiny::selectInput("rater_effect", "Rater effect", choices = c("Random" = "random", "Fixed" = "fixed")),
        shiny::selectInput(
          "agreement_type",
          "Agreement target",
          choices = c("Absolute agreement" = "absolute", "Consistency" = "consistency")
        )
      )
    ),
    shiny::selectInput("rating_type", "Rating unit", choices = c("Single rating" = "single", "Average rating" = "average")),
    shiny::div(
      class = "input-grid",
      shiny::numericInput("alpha", "Alpha", value = 0.05, min = 0.001, max = 0.5, step = 0.005),
      shiny::checkboxInput("na_rm", "Remove rows with missing values", value = TRUE),
      shiny::checkboxInput("interaction", "Include subject-rater interaction", value = TRUE),
      shiny::checkboxInput("use_rho0", "Test against custom rho0", value = FALSE)
    ),
    shiny::conditionalPanel(
      "input.use_rho0 == true",
      shiny::numericInput("rho0", "Null ICC rho0", value = 0.5, min = 0, max = 0.99, step = 0.01)
    ),
    shiny::actionButton(
      "run_analysis",
      label = shiny::tagList(shiny::icon("play"), "Run analysis"),
      class = "primary-action"
    )
  )
}

.icc_sample_size_controls <- function() {
  shiny::div(
    class = "control-panel",
    shiny::h2("Study planning"),
    shiny::p(class = "muted", "Estimate required subjects or the assurance probability for a fixed sample size."),
    shiny::radioButtons(
      "ss_method",
      "Planning method",
      choices = c(
        "Lower CI assurance" = "lower",
        "CI half-width assurance" = "width",
        "Power for fixed n" = "power"
      ),
      selected = "lower"
    ),
    shiny::div(
      class = "input-grid",
      shiny::numericInput("ss_rho", "Anticipated ICC", value = 0.75, min = 0.01, max = 0.99, step = 0.01),
      shiny::numericInput("ss_k", "Raters per subject", value = 3, min = 2, max = 50, step = 1),
      shiny::numericInput("ss_alpha", "Alpha", value = 0.05, min = 0.001, max = 0.5, step = 0.005),
      shiny::numericInput("ss_assurance", "Assurance target", value = 0.8, min = 0.01, max = 0.99, step = 0.01)
    ),
    shiny::conditionalPanel(
      "input.ss_method == 'lower'",
      shiny::numericInput("ss_lower_rho0", "Required lower confidence limit", value = 0.5, min = 0, max = 0.98, step = 0.01)
    ),
    shiny::conditionalPanel(
      "input.ss_method == 'width'",
      shiny::numericInput("ss_width_omega", "Target CI half-width", value = 0.1, min = 0.001, max = 0.8, step = 0.01)
    ),
    shiny::conditionalPanel(
      "input.ss_method == 'power'",
      shiny::div(
        class = "input-grid",
        shiny::numericInput("ss_n", "Subjects", value = 40, min = 2, max = 10000, step = 1),
        shiny::radioButtons(
          "ss_power_method",
          "Power target",
          choices = c("Lower CI" = "lower", "CI width" = "width"),
          selected = "lower",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        "input.ss_power_method == 'lower'",
        shiny::numericInput("ss_power_rho0", "Lower CI threshold", value = 0.5, min = 0, max = 0.98, step = 0.01)
      ),
      shiny::conditionalPanel(
        "input.ss_power_method == 'width'",
        shiny::numericInput("ss_power_omega", "CI half-width", value = 0.1, min = 0.001, max = 0.8, step = 0.01)
      )
    ),
    shiny::hr(),
    shiny::h2("Design questions"),
    shiny::radioButtons(
      "ss_same_raters",
      "Do all subjects share the same raters?",
      choices = c("Yes" = "TRUE", "No" = "FALSE"),
      selected = "TRUE",
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.ss_same_raters == 'TRUE'",
      shiny::div(
        class = "input-grid",
        shiny::selectInput("ss_rater_effect", "Rater effect", choices = c("Random" = "random", "Fixed" = "fixed")),
        shiny::selectInput(
          "ss_agreement_type",
          "Agreement target",
          choices = c("Absolute agreement" = "absolute", "Consistency" = "consistency")
        )
      )
    ),
    shiny::selectInput("ss_rating_type", "Rating unit", choices = c("Single rating" = "single", "Average rating" = "average")),
    shiny::actionButton(
      "run_ss",
      label = shiny::tagList(shiny::icon("play"), "Calculate"),
      class = "primary-action"
    )
  )
}

.icc_app_server <- function(input, output, session) {
  raw_data <- shiny::reactive({
    switch(
      input$data_mode,
      example = .icc_example_data(),
      upload = .icc_read_uploaded(input$data_file, input$file_header, input$file_sep, input$file_dec),
      paste = .icc_read_pasted(input$pasted_data, input$pasted_header, input$pasted_sep, input$pasted_dec),
      NULL
    )
  })

  analysis <- shiny::eventReactive(input$run_analysis, {
    tryCatch({
      data <- raw_data()
      if (is.null(data)) {
        stop("Provide numeric data before running the analysis.", call. = FALSE)
      }

      design <- .icc_design_args(input)
      result <- do.call(
        icc_calc,
        c(
          list(
            data = data,
            alpha = input$alpha,
            rho0 = if (isTRUE(input$use_rho0)) input$rho0 else NULL,
            interaction = isTRUE(input$interaction),
            na.rm = isTRUE(input$na_rm),
            verbose = FALSE
          ),
          design
        )
      )

      list(ok = TRUE, value = result)
    }, error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    })
  }, ignoreNULL = FALSE)

  ss_result <- shiny::eventReactive(input$run_ss, {
    tryCatch({
      design <- .icc_design_args(input, prefix = "ss")
      method <- input$ss_method

      value <- switch(
        method,
        lower = do.call(
          icc_sample_size_lower,
          c(
            list(
              rho = input$ss_rho,
              rho0 = input$ss_lower_rho0,
              k = input$ss_k,
              alpha = input$ss_alpha,
              assurance = input$ss_assurance,
              verbose = FALSE
            ),
            design
          )
        ),
        width = do.call(
          icc_sample_size_width,
          c(
            list(
              rho = input$ss_rho,
              omega = input$ss_width_omega,
              k = input$ss_k,
              alpha = input$ss_alpha,
              assurance = input$ss_assurance,
              verbose = FALSE
            ),
            design
          )
        ),
        power = do.call(
          icc_power,
          c(
            list(
              n = input$ss_n,
              rho = input$ss_rho,
              rho0 = if (input$ss_power_method == "lower") input$ss_power_rho0 else NULL,
              omega = if (input$ss_power_method == "width") input$ss_power_omega else NULL,
              k = input$ss_k,
              alpha = input$ss_alpha,
              method = input$ss_power_method,
              verbose = FALSE
            ),
            design
          )
        )
      )

      mapping <- do.call(icc_map_design_to_icc, design)
      list(ok = TRUE, value = value, method = method, design = design, mapping = mapping)
    }, error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    })
  }, ignoreNULL = FALSE)

  output$analysis_alert <- shiny::renderUI({
    x <- analysis()
    if (!isTRUE(x$ok)) {
      return(.icc_alert("Analysis cannot run", x$error, type = "error"))
    }

    result <- x$value
    messages <- c(
      result$data_summary$warning_msg,
      result$warning_msg,
      result$tip_msg
    )
    messages <- messages[!vapply(messages, is.null, logical(1))]
    if (!length(messages)) {
      return(.icc_alert("Ready", "The selected design and data passed validation.", type = "success"))
    }
    .icc_alert("Review notes", paste(messages, collapse = " "), type = "warning")
  })

  output$analysis_metrics <- shiny::renderUI({
    x <- analysis()
    if (!isTRUE(x$ok)) {
      return(.icc_empty_state("Run a valid analysis to see ICC results."))
    }
    result <- x$value$icc_result
    eval <- x$value$evaluation
    f_test <- result$F_test_null

    shiny::div(
      class = "metric-grid",
      .icc_metric_card("ICC", .icc_fmt(result$point_est, 4), result$icc_code, "accent"),
      .icc_metric_card(
        "Confidence interval",
        paste0("[", .icc_fmt(result$ci_lower, 4), ", ", .icc_fmt(result$ci_upper, 4), "]"),
        paste0(.icc_fmt(result$ci_level * 100, 0), "% CI"),
        "teal"
      ),
      .icc_metric_card("Reliability", eval$rating_en, "Koo and Li lower-bound rule", "green"),
      .icc_metric_card("F-test p-value", .icc_p_value(f_test$p_value), "H0: ICC = 0", "amber")
    )
  })

  output$icc_plot <- shiny::renderPlot({
    x <- analysis()
    shiny::req(isTRUE(x$ok))
    result <- x$value$icc_result
    .icc_plot_interval(result$point_est, result$ci_lower, result$ci_upper)
  })

  output$model_summary <- shiny::renderTable({
    x <- analysis()
    shiny::req(isTRUE(x$ok))
    result <- x$value
    data.frame(
      Item = c("ICC type", "ICC code", "Subjects", "Raters", "Rating", "Reliability"),
      Value = c(
        result$icc_result$icc_type,
        result$icc_result$icc_code,
        result$data_summary$n,
        result$data_summary$k,
        result$evaluation$rating_en,
        result$evaluation$explanation
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$anova_table <- shiny::renderTable({
    x <- analysis()
    shiny::req(isTRUE(x$ok))
    .icc_named_list_table(x$value$icc_result$anova_summary)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$f_test_table <- shiny::renderTable({
    x <- analysis()
    shiny::req(isTRUE(x$ok))
    .icc_f_test_table(x$value$icc_result)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$report_text <- shiny::renderText({
    x <- analysis()
    if (!isTRUE(x$ok)) {
      return(x$error)
    }
    x$value$report
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() paste0("icc-report-", Sys.Date(), ".txt"),
    content = function(file) {
      x <- analysis()
      text <- if (isTRUE(x$ok)) x$value$report else x$error
      writeLines(text, con = file, useBytes = TRUE)
    }
  )

  output$data_preview <- shiny::renderTable({
    data <- raw_data()
    if (is.null(data)) {
      return(data.frame(Message = "No data loaded.", check.names = FALSE))
    }
    utils::head(as.data.frame(data), 12)
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = TRUE)

  output$data_diagnostics <- shiny::renderTable({
    data <- raw_data()
    if (is.null(data)) {
      return(data.frame(Item = "Status", Value = "No data loaded.", check.names = FALSE))
    }
    prep <- icc_preprocess_data(data, na.rm = TRUE)
    data.frame(
      Item = c("Rows", "Columns", "Missing cells", "Validation", "Message"),
      Value = c(
        nrow(as.matrix(data)),
        ncol(as.matrix(data)),
        sum(is.na(as.matrix(data))),
        if (is.null(prep$error_msg)) "Valid numeric matrix" else "Invalid",
        if (is.null(prep$error_msg)) .icc_or(prep$warning_msg, "No issues detected.") else prep$error_msg
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$selection_guide <- shiny::renderTable({
    .icc_selection_guide()
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$rating_guide <- shiny::renderTable({
    data.frame(
      Rating = c("Poor", "Moderate", "Good", "Excellent"),
      Rule = c("Lower CI < 0.50", "0.50 <= lower CI < 0.75", "0.75 <= lower CI < 0.90", "Lower CI >= 0.90"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$ss_alert <- shiny::renderUI({
    x <- ss_result()
    if (!isTRUE(x$ok)) {
      return(.icc_alert("Planning cannot run", x$error, type = "error"))
    }
    message <- paste("Mapped design:", x$mapping$icc_full_name)
    extra <- c(x$mapping$warning_msg, x$mapping$tip_msg)
    extra <- extra[!vapply(extra, is.null, logical(1))]
    if (length(extra)) {
      message <- paste(message, paste(extra, collapse = " "))
    }
    .icc_alert("Planning ready", message, type = "success")
  })

  output$ss_metrics <- shiny::renderUI({
    x <- ss_result()
    if (!isTRUE(x$ok)) {
      return(.icc_empty_state("Set a valid planning design to see results."))
    }
    value_label <- if (x$method == "power") {
      paste0(.icc_fmt(100 * x$value, 1), "%")
    } else {
      as.character(x$value)
    }
    value_caption <- switch(
      x$method,
      lower = "Required subjects for lower CI assurance",
      width = "Required subjects for CI half-width assurance",
      power = "Estimated assurance probability"
    )

    shiny::div(
      class = "metric-grid",
      .icc_metric_card("Result", value_label, value_caption, "accent"),
      .icc_metric_card("ICC model", x$mapping$icc_code, x$mapping$icc_full_name, "teal"),
      .icc_metric_card("Anticipated ICC", .icc_fmt(input$ss_rho, 2), paste0("k = ", input$ss_k), "green"),
      .icc_metric_card("Alpha", .icc_fmt(input$ss_alpha, 3), paste0("Assurance target = ", .icc_fmt(input$ss_assurance, 2)), "amber")
    )
  })

  output$power_plot <- shiny::renderPlot({
    x <- ss_result()
    shiny::req(isTRUE(x$ok))
    .icc_plot_power_curve(input, x)
  })

  output$ss_details <- shiny::renderTable({
    x <- ss_result()
    shiny::req(isTRUE(x$ok))
    .icc_ss_table(input, x)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$ss_export_preview <- shiny::renderTable({
    x <- ss_result()
    if (!isTRUE(x$ok)) {
      return(data.frame(Item = "Status", Value = x$error, check.names = FALSE))
    }
    .icc_ss_table(input, x)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$download_design <- shiny::downloadHandler(
    filename = function() paste0("icc-design-", Sys.Date(), ".csv"),
    content = function(file) {
      x <- ss_result()
      table <- if (isTRUE(x$ok)) {
        .icc_ss_table(input, x)
      } else {
        data.frame(Item = "Error", Value = x$error, check.names = FALSE)
      }
      utils::write.csv(table, file, row.names = FALSE)
    }
  )
}

.icc_sep_choices <- function() {
  c("Comma (,)" = ",", "Tab" = "\t", "Semicolon (;)" = ";", "Space" = " ")
}

.icc_read_uploaded <- function(file, header, sep, dec) {
  if (is.null(file) || is.null(file$datapath)) {
    return(NULL)
  }
  utils::read.table(
    file = file$datapath,
    header = isTRUE(header),
    sep = sep,
    dec = dec,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.icc_read_pasted <- function(text, header, sep, dec) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(NULL)
  }
  con <- textConnection(text)
  on.exit(close(con), add = TRUE)
  utils::read.table(
    file = con,
    header = isTRUE(header),
    sep = sep,
    dec = dec,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.icc_example_data <- function() {
  env <- new.env(parent = emptyenv())
  loaded <- tryCatch({
    utils::data("icc_data", package = "ICCDesign", envir = env)
    exists("icc_data", envir = env, inherits = FALSE)
  }, error = function(e) {
    FALSE
  })

  if (!loaded && file.exists("data/icc_data.rda")) {
    load("data/icc_data.rda", envir = env)
    loaded <- exists("icc_data", envir = env, inherits = FALSE)
  }
  if (!loaded) {
    stop("The built-in icc_data dataset could not be loaded.", call. = FALSE)
  }

  as.data.frame(env$icc_data, check.names = FALSE)
}

.icc_design_args <- function(input, prefix = "") {
  id <- function(x) if (nzchar(prefix)) paste0(prefix, "_", x) else x
  same <- identical(input[[id("same_raters")]], "TRUE")

  list(
    same_raters = same,
    rater_effect = if (same) input[[id("rater_effect")]] else NULL,
    rating_type = input[[id("rating_type")]],
    agreement_type = if (same) input[[id("agreement_type")]] else NULL
  )
}

.icc_alert <- function(title, message, type = c("success", "warning", "error")) {
  type <- match.arg(type)
  shiny::div(
    class = paste("notice", paste0("notice-", type)),
    shiny::strong(title),
    shiny::span(message)
  )
}

.icc_empty_state <- function(message) {
  shiny::div(class = "empty-state", message)
}

.icc_metric_card <- function(label, value, caption = NULL, tone = "accent") {
  shiny::div(
    class = paste("metric-card", paste0("metric-", tone)),
    shiny::span(class = "metric-label", label),
    shiny::strong(class = "metric-value", value),
    shiny::span(class = "metric-caption", .icc_or(caption, ""))
  )
}

.icc_named_list_table <- function(x) {
  data.frame(
    Metric = names(x),
    Value = vapply(x, function(value) {
      if (is.null(value)) {
        "NA"
      } else {
        .icc_fmt(value, 4)
      }
    }, character(1)),
    check.names = FALSE
  )
}

.icc_f_test_table <- function(icc_result) {
  tests <- list("ICC = 0" = icc_result$F_test_null)
  if (!is.null(icc_result$F_test_rho0)) {
    tests[["ICC = rho0"]] <- icc_result$F_test_rho0
  }

  rows <- lapply(names(tests), function(name) {
    test <- tests[[name]]
    data.frame(
      Hypothesis = name,
      F_stat = .icc_fmt(test$F_stat, 4),
      df1 = .icc_fmt(test$df1, 0),
      df2 = .icc_fmt(test$df2, 2),
      p_value = .icc_p_value(test$p_value),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

.icc_ss_table <- function(input, x) {
  method_label <- switch(
    x$method,
    lower = "Lower CI assurance",
    width = "CI half-width assurance",
    power = "Power for fixed n"
  )
  result_value <- if (x$method == "power") {
    paste0(.icc_fmt(100 * x$value, 2), "%")
  } else {
    as.character(x$value)
  }

  data.frame(
    Item = c(
      "Method",
      "Result",
      "Mapped ICC type",
      "Anticipated ICC",
      "Raters per subject",
      "Alpha",
      "Assurance target"
    ),
    Value = c(
      method_label,
      result_value,
      x$mapping$icc_full_name,
      .icc_fmt(input$ss_rho, 3),
      input$ss_k,
      .icc_fmt(input$ss_alpha, 3),
      .icc_fmt(input$ss_assurance, 3)
    ),
    check.names = FALSE
  )
}

.icc_selection_guide <- function() {
  data.frame(
    Same_raters = c("No", "No", "Yes", "Yes", "Yes", "Yes"),
    Rater_effect = c("Not specified", "Not specified", "Random", "Random", "Fixed", "Fixed"),
    Agreement = c("Absolute", "Absolute", "Absolute", "Absolute", "Consistency", "Consistency"),
    Rating = c("Single", "Average", "Single", "Average", "Single", "Average"),
    ICC_type = c("ICC(1,1)", "ICC(1,k)", "ICC(2,1)", "ICC(2,k)", "ICC(3,1)", "ICC(3,k)"),
    check.names = FALSE
  )
}

.icc_plot_interval <- function(point, lower, upper) {
  x_min <- min(-0.2, point - 0.05, lower, na.rm = TRUE)
  x_max <- 1
  graphics::par(mar = c(4, 2, 2, 1), bg = "white")
  graphics::plot(
    NA,
    xlim = c(x_min, x_max),
    ylim = c(0.7, 1.3),
    xlab = "ICC value",
    ylab = "",
    yaxt = "n",
    bty = "n",
    main = ""
  )
  graphics::rect(x_min, 0.7, 0.5, 1.3, col = "#f7d6d0", border = NA)
  graphics::rect(0.5, 0.7, 0.75, 1.3, col = "#f4e7bd", border = NA)
  graphics::rect(0.75, 0.7, 0.9, 1.3, col = "#d7eadb", border = NA)
  graphics::rect(0.9, 0.7, x_max, 1.3, col = "#cfe8e5", border = NA)
  graphics::axis(1, at = c(0, 0.5, 0.75, 0.9, 1))
  graphics::abline(v = c(0.5, 0.75, 0.9), col = "#6b7280", lty = 3)
  graphics::segments(lower, 1, upper, 1, lwd = 5, col = "#256d75")
  graphics::points(point, 1, pch = 19, cex = 1.8, col = "#d85c48")
  graphics::text(
    x = c(0.25, 0.625, 0.825, 0.95),
    y = 1.22,
    labels = c("Poor", "Moderate", "Good", "Excellent"),
    cex = 0.82,
    col = "#374151"
  )
}

.icc_plot_power_curve <- function(input, x) {
  method <- if (x$method == "power") input$ss_power_method else x$method
  n_current <- if (x$method == "power") input$ss_n else x$value
  n_max <- max(12, ceiling(n_current * 1.8), n_current + 10)
  n_seq <- seq(2, n_max)

  powers <- vapply(n_seq, function(n) {
    tryCatch(
      do.call(
        icc_power,
        c(
          list(
            n = n,
            rho = input$ss_rho,
            rho0 = if (method == "lower") {
              if (x$method == "power") input$ss_power_rho0 else input$ss_lower_rho0
            } else {
              NULL
            },
            omega = if (method == "width") {
              if (x$method == "power") input$ss_power_omega else input$ss_width_omega
            } else {
              NULL
            },
            k = input$ss_k,
            alpha = input$ss_alpha,
            method = method,
            verbose = FALSE
          ),
          x$design
        )
      ),
      error = function(e) NA_real_
    )
  }, numeric(1))

  graphics::par(mar = c(4, 4, 2, 1), bg = "white")
  graphics::plot(
    n_seq,
    powers,
    type = "l",
    lwd = 3,
    col = "#256d75",
    ylim = c(0, 1),
    xlab = "Subjects",
    ylab = "Assurance probability",
    bty = "n"
  )
  graphics::grid(col = "#e5e7eb")
  graphics::abline(h = input$ss_assurance, col = "#d85c48", lty = 2, lwd = 2)
  graphics::abline(v = n_current, col = "#374151", lty = 3, lwd = 2)
  graphics::points(n_current, powers[which.min(abs(n_seq - n_current))], pch = 19, col = "#d85c48", cex = 1.4)
}

.icc_fmt <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  formatC(as.numeric(x), format = "f", digits = digits)
}

.icc_p_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  format.pval(x, digits = 4, eps = 0.0001)
}

.icc_or <- function(x, fallback) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) {
    fallback
  } else {
    x
  }
}

.icc_app_css <- function() {
  "
  :root {
    --ink: #1f2937;
    --muted: #6b7280;
    --line: #dbe3ea;
    --surface: #ffffff;
    --surface-soft: #f6f8fb;
    --teal: #256d75;
    --green: #3f7d58;
    --amber: #a66b12;
    --coral: #d85c48;
    --danger: #b42318;
  }

  body {
    background: var(--surface-soft);
    color: var(--ink);
    font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    letter-spacing: 0;
  }

  .container-fluid {
    padding: 0;
  }

  .app-shell {
    max-width: 1440px;
    margin: 0 auto;
    padding: 24px;
  }

  .topbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 18px;
    padding: 18px 20px;
    background: var(--surface);
    border: 1px solid var(--line);
    border-radius: 8px;
    box-shadow: 0 14px 34px rgba(31, 41, 55, 0.06);
    margin-bottom: 16px;
  }

  .brand-block {
    display: flex;
    align-items: center;
    gap: 14px;
  }

  .brand-mark {
    width: 48px;
    height: 48px;
    border-radius: 8px;
    display: grid;
    place-items: center;
    background: var(--teal);
    color: #ffffff;
    font-weight: 800;
  }

  h1, h2, h3 {
    margin-top: 0;
    letter-spacing: 0;
  }

  h1 {
    font-size: 28px;
    margin-bottom: 3px;
    font-weight: 750;
  }

  h2 {
    font-size: 18px;
    margin-bottom: 8px;
    font-weight: 720;
  }

  h3 {
    font-size: 16px;
    margin-bottom: 12px;
    font-weight: 700;
  }

  .brand-block p,
  .muted {
    margin: 0;
    color: var(--muted);
  }

  .topbar-actions {
    display: flex;
    flex-wrap: wrap;
    gap: 8px;
    justify-content: flex-end;
  }

  .status-pill {
    border: 1px solid var(--line);
    color: var(--teal);
    background: #f0f7f6;
    padding: 7px 10px;
    border-radius: 8px;
    font-size: 12px;
    font-weight: 700;
  }

  .nav-tabs {
    border-bottom: 1px solid var(--line);
    margin-bottom: 16px;
  }

  .nav-tabs > li > a {
    color: var(--muted);
    border-radius: 8px 8px 0 0;
    font-weight: 700;
  }

  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    color: var(--teal);
    border-color: var(--line) var(--line) var(--surface);
  }

  .workspace-grid {
    display: grid;
    grid-template-columns: minmax(300px, 380px) minmax(0, 1fr);
    gap: 16px;
    align-items: start;
  }

  .guide-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 16px;
  }

  .control-panel,
  .panel,
  .metric-card {
    background: var(--surface);
    border: 1px solid var(--line);
    border-radius: 8px;
    box-shadow: 0 12px 28px rgba(31, 41, 55, 0.05);
  }

  .control-panel {
    padding: 18px;
    position: sticky;
    top: 12px;
  }

  .panel {
    padding: 16px;
    overflow: hidden;
  }

  .panel-heading {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    margin-bottom: 8px;
  }

  .result-stack {
    display: grid;
    gap: 16px;
  }

  .two-col,
  .metric-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 16px;
  }

  .metric-grid {
    grid-template-columns: repeat(4, minmax(0, 1fr));
  }

  .metric-card {
    padding: 14px;
    min-height: 112px;
    border-top: 4px solid var(--teal);
  }

  .metric-accent {
    border-top-color: var(--coral);
  }

  .metric-teal {
    border-top-color: var(--teal);
  }

  .metric-green {
    border-top-color: var(--green);
  }

  .metric-amber {
    border-top-color: var(--amber);
  }

  .metric-label,
  .metric-caption {
    display: block;
    color: var(--muted);
    font-size: 12px;
    font-weight: 700;
  }

  .metric-value {
    display: block;
    font-size: 28px;
    line-height: 1.1;
    margin: 8px 0;
    overflow-wrap: anywhere;
  }

  .metric-caption {
    font-weight: 600;
    line-height: 1.35;
  }

  .input-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 10px 12px;
  }

  .form-group {
    margin-bottom: 12px;
  }

  label {
    font-size: 12px;
    color: var(--muted);
    font-weight: 700;
  }

  .form-control,
  .selectize-input,
  textarea {
    border-color: var(--line);
    border-radius: 8px;
    box-shadow: none;
  }

  .form-control:focus,
  .selectize-input.focus {
    border-color: var(--teal);
    box-shadow: 0 0 0 3px rgba(37, 109, 117, 0.12);
  }

  .primary-action,
  .btn-default {
    border-radius: 8px;
    font-weight: 700;
  }

  .primary-action {
    width: 100%;
    margin-top: 8px;
    background: var(--teal);
    color: #ffffff;
    border: 1px solid var(--teal);
  }

  .primary-action:hover,
  .primary-action:focus {
    background: #1f5e65;
    color: #ffffff;
    border-color: #1f5e65;
  }

  .notice {
    display: flex;
    gap: 10px;
    align-items: flex-start;
    padding: 12px 14px;
    border-radius: 8px;
    border: 1px solid var(--line);
    background: var(--surface);
  }

  .notice strong {
    white-space: nowrap;
  }

  .notice-success {
    border-color: #b9ddc5;
    background: #f1f8f3;
  }

  .notice-warning {
    border-color: #f1d184;
    background: #fff8e8;
  }

  .notice-error {
    border-color: #f0b8b1;
    background: #fff1ef;
    color: var(--danger);
  }

  .empty-state {
    padding: 22px;
    text-align: center;
    color: var(--muted);
    border: 1px dashed var(--line);
    border-radius: 8px;
    background: var(--surface);
  }

  table {
    width: 100%;
    font-size: 13px;
  }

  pre {
    border: 1px solid var(--line);
    background: #fbfcfe;
    border-radius: 8px;
    color: var(--ink);
    white-space: pre-wrap;
  }

  @media (max-width: 1100px) {
    .workspace-grid,
    .guide-grid,
    .two-col,
    .metric-grid {
      grid-template-columns: 1fr;
    }

    .control-panel {
      position: static;
    }

    .topbar {
      align-items: flex-start;
      flex-direction: column;
    }
  }

  @media (max-width: 640px) {
    .app-shell {
      padding: 12px;
    }

    .input-grid {
      grid-template-columns: 1fr;
    }

    .metric-value {
      font-size: 23px;
    }
  }
  "
}
