# Anomaly Detection Server Module
# Backs the anomaly_detection_ui module. Pulls the currently-uploaded BE
# dataset (via shared reactive) or accepts a separate upload, runs the pairwise
# / multivariate / distribution-check analyses, and renders results.

anomaly_detection_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # On startup, default to "Upload dataset" if no shared dataset is loaded.
    observe({
      d <- tryCatch(uploaded_data(), error = function(e) NULL)
      if (is.null(d) || (is.data.frame(d) && nrow(d) == 0)) {
        updateRadioButtons(session, "data_source", selected = "upload")
      }
    })

    # =========================================================================
    # Reactive data plumbing
    # =========================================================================

    # Locally uploaded data (used only when data_source == "upload")
    local_data <- reactive({
      req(input$data_source == "upload", input$ad_file)
      f <- input$ad_file
      ext <- tolower(tools::file_ext(f$name))
      tryCatch({
        if (ext %in% c("csv", "txt")) readr::read_csv(f$datapath, show_col_types = FALSE)
        else if (ext == "tsv")        readr::read_tsv(f$datapath, show_col_types = FALSE)
        else if (ext %in% c("xls", "xlsx")) readxl::read_excel(f$datapath)
        else                          read.csv(f$datapath, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification(paste("File read failed:", e$message), type = "error")
        NULL
      })
    })

    # Selected source data (long format)
    source_data <- reactive({
      d <- if (input$data_source == "upload") local_data() else uploaded_data()
      validate(need(!is.null(d) && nrow(d) > 0,
                    "No dataset available. Upload data or load a BE dataset first."))
      validate(need(all(c("Subject", "Time", "Concentration") %in% names(d)),
                    "Dataset must contain Subject, Time, Concentration columns for anomaly detection."))
      as.data.frame(d)
    })

    # Treatment filter UI removed — all profiles included by default.
    output$treatment_filter_ui <- renderUI({ NULL })

    # Filtered data: optionally drop zero/negative concentration points
    filtered_data <- reactive({
      d <- source_data()
      if (isTRUE(input$exclude_blq)) {
        d$Concentration[d$Concentration <= 0] <- NA_real_
      }
      d
    })

    # Profile matrix object (cached)
    profile_obj <- reactive({
      d <- filtered_data()
      validate(need(nrow(d) > 0, "No data after filtering."))
      prepare_profile_matrix(d)
    })

    # =========================================================================
    # Tab 1: Data Selection -- status panels
    # =========================================================================

    output$data_status <- renderUI({
      d <- tryCatch(source_data(), error = function(e) NULL)
      if (is.null(d)) {
        return(div(class = "alert alert-warning",
                   icon("circle-exclamation"), " No dataset loaded."))
      }
      div(class = "alert alert-success",
          icon("circle-check"), strong(" Dataset ready."),
          br(), sprintf("Rows: %d  |  Subjects: %d  |  Columns: %s",
                        nrow(d),
                        length(unique(d$Subject)),
                        paste(names(d), collapse = ", ")))
    })

    output$profile_summary <- renderUI({
      obj <- tryCatch(profile_obj(), error = function(e) NULL)
      if (is.null(obj)) return(NULL)
      div(class = "alert alert-info",
          icon("chart-line"),
          sprintf(" %d profile(s) across %d timepoint(s).",
                  nrow(obj$matrix), length(obj$times)))
    })

    output$profile_preview <- DT::renderDataTable({
      obj <- profile_obj()
      df <- as.data.frame(obj$matrix)
      df <- cbind(ID = rownames(df), df)
      # Render values exactly as stored in the uploaded data — do not pad decimals.
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # =========================================================================
    # Tab 2: Pairwise Comparison
    # =========================================================================

    pair_results <- reactiveVal(NULL)

    # Per-analysis metadata: which battery to run, score column to sort by,
    # and overlay rendering hint.
    pair_specs <- list(
      overlap = list(
        label   = "Overlapping duplicates",
        runner  = function(M) run_overlap_battery(M),
        score   = "ccc",          # Lin's CCC: closest to 1 = most similar
        overlay = "raw"
      ),
      scaled = list(
        label   = "Scaled duplicates",
        runner  = function(M) run_scaled_battery(M),
        score   = "scale_score",  # r2 * |slope-1|: line fit AND non-identity slope
        overlay = "normalised"
      ),
      lag = list(
        label   = "Time-shifted duplicates",
        runner  = function(M) run_lag_battery(M),
        score   = "shift_r2",   # post-shift agreement (r² of aligned overlap)
        overlay = "raw"
      ),
      dynamics = list(
        label   = "Dynamic pattern match",
        runner  = function(M) run_dynamics_battery(M),
        score   = "derivative_pearson",  # correlation of first differences
        overlay = "normalised"
      )
    )

    # Help text per analysis (shown in modal when ? icon clicked)
    pair_help_text <- list(
      overlap = tagList(
        h4("Overlapping duplicates"),
        p("Detects literal or near-literal re-use of a profile \u2014 ",
          "two subject-period records whose concentrations match across all timepoints."),
        tags$ul(
          tags$li(strong("Lin's CCC:"), " concordance correlation; penalises both shape and scale shifts."),
          tags$li(strong("f2 similarity factor:"), " mean-squared difference of raw values (FDA dissolution metric)."),
          tags$li(strong("SaToWIB regression:"), " linear fit B ~ A; slope \u2248 1 and r\u00B2 \u2248 1 = exact copy.")
        ),
        p(em("High score = candidate duplicate. Always confirm by inspecting the overlay."))
      ),
      scaled = tagList(
        h4("Scaled duplicates (proportional differences)"),
        p("Detects pairs whose values are a proportional rescaling of each other across ",
          "the whole profile \u2014 same shape, uniformly higher or lower amplitude ",
          "(e.g. a dilution / concentration error)."),
        tags$ul(
          tags$li(strong("SaToWIB regression r\u00B2 \u2248 1:"), " all timepoints fall on a single line."),
          tags$li(strong("|slope \u2212 1| large:"), " the line is not the identity \u2014 one profile is uniformly higher or lower.")
        ),
        p(em("Single-point Cmax spikes are intentionally not flagged here \u2014 they break r\u00B2 ",
             "and are easier to spot visually in the overlay plots."))
      ),
      lag = tagList(
        h4("Time-shifted duplicates"),
        p("Detects re-use disguised by shifting a profile in time \u2014 ",
          "shape is identical but the curve appears earlier or later."),
        tags$ul(
          tags$li(strong("Cross-correlation lag:"), " the timepoint offset at which the two profiles best align."),
          tags$li(strong("Cross-correlation peak:"), " correlation at that optimal lag (must be > 0)."),
          tags$li(strong("shift_r\u00B2 (sort key):"), " r\u00B2 of the two profiles AFTER applying the optimal lag. ",
                  "Only a true time-shifted copy gives shift_r\u00B2 \u2248 1; coincidental shape matches give shift_r\u00B2 << 1.")
        ),
        p(em("Gated to |lag| \u2265 1, peak > 0, and shift_r\u00B2 \u2265 0.90 so only genuine shifted copies appear."))
      ),
      dynamics = tagList(
        h4("Dynamic pattern match"),
        p("Detects re-use of rise / fall dynamics independent of magnitude \u2014 ",
          "the local slopes (\u0394-concentration) match even when absolute values differ."),
        tags$ul(
          tags$li(strong("Derivative Pearson:"), " correlation of first differences; matches \"motion\" of the curve."),
          tags$li(strong("Pearson on Cmax-normalised:"), " confirmation that overall shape also tracks."),
          tags$li(strong("Lin's CCC:"), " low CCC isolates pairs that share dynamics without sharing amplitude.")
        ),
        p(em("This is the subtlest pattern \u2014 use it as a confirmatory check on candidates already flagged elsewhere."))
      )
    )

    observeEvent(input$pair_help, {
      key <- input$pair_analysis %||% "overlap"
      showModal(modalDialog(
        title = NULL,
        pair_help_text[[key]],
        easyClose = TRUE,
        size = "m",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$run_pairwise, {
      obj <- tryCatch(profile_obj(), error = function(e) NULL)
      if (is.null(obj) || nrow(obj$matrix) < 2) {
        showNotification("Need at least 2 profiles to compare.", type = "warning")
        return()
      }
      key  <- input$pair_analysis %||% "overlap"
      spec <- pair_specs[[key]]
      withProgress(message = sprintf("Running %s...", spec$label), value = 0.2, {
        res <- tryCatch(
          spec$runner(obj$matrix),
          error = function(e) {
            showNotification(paste("Comparison failed:", e$message), type = "error")
            NULL
          }
        )
        incProgress(0.7)
        pair_results(if (is.null(res)) NULL
                     else list(table = res, spec = spec, key = key))
        incProgress(0.1)
      })
    })

    output$pair_table <- DT::renderDataTable({
      pr <- pair_results()
      validate(need(!is.null(pr), "Choose a comparison and click 'Run Comparison'."))
      df        <- pr$table
      score_col <- pr$spec$score
      # Drop rows with no score, then sort descending in R so DT
      # inherits the correct order regardless of NA handling.
      df <- df[!is.na(df[[score_col]]), , drop = FALSE]
      df <- df[order(-df[[score_col]]), , drop = FALSE]
      n  <- min(nrow(df), input$top_n_pairs %||% 50)
      df <- head(df, n)
      # Move the score column right after id1/id2 so it is visually obvious
      # which column the ranking is based on.
      front <- intersect(c("id1", "id2", score_col), names(df))
      df    <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
      num_cols  <- which(sapply(df, is.numeric))
      score_idx <- which(names(df) == score_col) - 1  # 0-indexed for DT
      # Rename id1/id2 for display only (after index math)
      names(df)[names(df) == "id1"] <- "Profile 1"
      names(df)[names(df) == "id2"] <- "Profile 2"
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE,
                                   dom = "rtip",
                                   order = list(list(score_idx, "desc")))) |>
        DT::formatRound(columns = num_cols, digits = 3)
    })

    output$pair_overlay <- plotly::renderPlotly({
      pr <- pair_results()
      validate(need(!is.null(pr), "Run a comparison."))
      sel <- input$pair_table_rows_selected
      validate(need(length(sel) == 1, "Select a row in the table to overlay."))
      pair <- pr$table[sel, ]
      obj  <- profile_obj()
      a <- obj$matrix[pair$id1, ]
      b <- obj$matrix[pair$id2, ]

      y_label <- "Concentration"
      if (identical(pr$spec$overlay, "normalised")) {
        # Normalise each profile by its own Cmax for shape comparison
        a_max <- suppressWarnings(max(a, na.rm = TRUE))
        b_max <- suppressWarnings(max(b, na.rm = TRUE))
        if (is.finite(a_max) && a_max > 0) a <- a / a_max
        if (is.finite(b_max) && b_max > 0) b <- b / b_max
        y_label <- "Concentration / Cmax"
      }

      df <- data.frame(
        Time = rep(obj$times, 2),
        Concentration = c(a, b),
        Profile = rep(c(pair$id1, pair$id2), each = length(obj$times))
      )
      score_val <- pair[[pr$spec$score]]
      plotly::plot_ly(df, x = ~Time, y = ~Concentration, color = ~Profile,
                      type = "scatter", mode = "lines+markers") |>
        plotly::layout(
          title = sprintf("%s  \u2014  %s vs %s  (score %.2f)",
                          pr$spec$label, pair$id1, pair$id2,
                          score_val %||% NA_real_),
          xaxis = list(title = "Time"),
          yaxis = list(title = y_label)
        )
    })

    # =========================================================================
    # Tab: Distribution Checks
    # =========================================================================

    output$cv_plot <- plotly::renderPlotly({
      d <- filtered_data()
      cv <- tryCatch(
        compute_within_subject_cv(d, metric = input$cv_metric %||% "Cmax"),
        error = function(e) NULL
      )
      validate(need(!is.null(cv) && nrow(cv) > 0,
                    "Within-subject CV requires a Treatment column with multiple periods."))
      plotly::plot_ly(cv, x = ~CV_within_pct, type = "histogram",
                      marker = list(color = "#3182ce")) |>
        plotly::layout(title = sprintf("Within-Subject CV (%%) \u2014 %s",
                                        input$cv_metric %||% "Cmax"),
                       xaxis = list(title = "Within-subject CV (%)"),
                       yaxis = list(title = "Subjects"))
    })

    output$tmax_plot <- plotly::renderPlotly({
      d <- filtered_data()
      tm <- tryCatch(compute_tmax_distribution(d), error = function(e) NULL)
      validate(need(!is.null(tm) && nrow(tm) > 0, "Tmax could not be computed."))
      plotly::plot_ly(tm, x = ~Tmax, type = "histogram",
                      marker = list(color = "#ed8936")) |>
        plotly::layout(title = "Tmax Distribution",
                       xaxis = list(title = "Tmax"),
                       yaxis = list(title = "Subject-periods"))
    })

    # =========================================================================
    # Tab: Trend Analysis (cumulative BE evolution)
    # =========================================================================

    # Compute per-subject-per-period PK parameter table from conc-time data.
    # Returns a long-format frame with columns: subject, treatment, period,
    # Cmax, AUC0t.
    trend_pk_table <- reactive({
      d <- filtered_data()
      validate(need("Treatment" %in% names(d),
                    "Trend analysis requires a Treatment column (Test/Reference)."))
      grp_cols <- c("Subject", "Treatment")
      if ("Period" %in% names(d)) grp_cols <- c(grp_cols, "Period")

      d <- d[!is.na(d$Concentration) & !is.na(d$Time), , drop = FALSE]

      pk <- d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
        dplyr::arrange(Time, .by_group = TRUE) |>
        dplyr::summarise(
          Cmax  = max(Concentration, na.rm = TRUE),
          AUC0t = {
            t <- Time; c <- Concentration
            ok <- is.finite(t) & is.finite(c)
            t <- t[ok]; c <- c[ok]
            if (length(t) < 2) NA_real_
            else sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2)
          },
          .groups = "drop"
        ) |>
        as.data.frame()

      pk$Cmax[!is.finite(pk$Cmax)]   <- NA_real_
      pk$AUC0t[!is.finite(pk$AUC0t)] <- NA_real_

      # Standardise treatment labels for downstream cumulative function
      pk$Treatment <- ifelse(pk$Treatment %in% c("T", "Test", "test"),  "Test",
                      ifelse(pk$Treatment %in% c("R", "Reference", "reference"),
                             "Reference", as.character(pk$Treatment)))
      # Standard column names expected by prepare_cumulative_data
      names(pk)[names(pk) == "Subject"]   <- "subject"
      names(pk)[names(pk) == "Treatment"] <- "treatment"
      if ("Period" %in% names(pk))
        names(pk)[names(pk) == "Period"] <- "period"
      pk
    })

    trend_results <- reactiveVal(NULL)

    observeEvent(input$run_trend, {
      pk <- tryCatch(trend_pk_table(), error = function(e) {
        showNotification(paste("Trend setup failed:", e$message), type = "error")
        NULL
      })
      req(pk)
      param <- input$trend_parameter %||% "Cmax"
      validate(need(param %in% names(pk),
                    sprintf("Parameter %s could not be computed from the data.", param)))

      subjects <- sort(unique(pk$subject))
      order_mode <- input$trend_order_mode %||% "id"
      ordered <- switch(order_mode,
        "id"     = subjects,
        "custom" = {
          raw <- strsplit(input$trend_custom_order %||% "", "[,\\s]+")[[1]]
          raw <- trimws(raw); raw <- raw[nzchar(raw)]
          # Try to coerce to numeric if subjects are numeric
          if (is.numeric(subjects) || all(grepl("^-?\\d+$", as.character(subjects)))) {
            raw_num <- suppressWarnings(as.numeric(raw))
            raw <- raw_num
          }
          keep <- raw[raw %in% subjects]
          if (length(keep) < 2) subjects else keep
        },
        subjects
      )

      withProgress(message = "Running cumulative BE analysis...", value = 0.1, {
        be_lo <- (input$trend_be_lower %||% 80) / 100
        be_hi <- (input$trend_be_upper %||% 125) / 100
        prog <- tryCatch(
          perform_progressive_be_analysis(
            data = pk,
            parameter = param,
            subject_order = ordered,
            anova_method = "fixed",
            be_limits = c(be_lo, be_hi)
          ),
          error = function(e) {
            showNotification(paste("Cumulative analysis failed:", e$message),
                             type = "error")
            NULL
          }
        )
        trend_results(list(progress = prog, parameter = param,
                           be_limits = c(be_lo * 100, be_hi * 100),
                           order = ordered))
      })
    })

    output$trend_plot <- plotly::renderPlotly({
      tr <- trend_results()
      validate(need(!is.null(tr) && !is.null(tr$progress) && nrow(tr$progress) > 0,
                    "Run cumulative analysis to view the trend plot."))
      p <- create_cumulative_plot(
        cumulative_data = tr$progress,
        parameter = tr$parameter,
        be_limits = tr$be_limits / 100,
        interactive = TRUE
      )
      plotly::ggplotly(p)
    })

    # =========================================================================
    # Subgroup / Exclusion ABE
    # =========================================================================

    # Dynamic UI: subject multi-select for exclusion mode
    output$sg_exclude_ui <- renderUI({
      pk <- tryCatch(trend_pk_table(), error = function(e) NULL)
      if (is.null(pk)) return(helpText("Load data first."))
      subjects <- sort(unique(pk$subject))
      selectizeInput(ns("sg_exclude_subjects"),
                     "Subjects to exclude",
                     choices  = subjects,
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(placeholder = "Select subjects to exclude..."))
    })

    # Dynamic UI: one text input per subgroup
    output$sg_groups_ui <- renderUI({
      n   <- max(1L, min(6L, as.integer(input$sg_n_groups %||% 2L)))
      pk  <- tryCatch(trend_pk_table(), error = function(e) NULL)
      hint <- if (!is.null(pk)) paste(sort(unique(pk$subject)), collapse = ", ") else ""
      lapply(seq_len(n), function(i) {
        textInput(ns(paste0("sg_subjects_", i)),
                  label       = paste0("Subgroup ", i, " subjects"),
                  placeholder = paste0("e.g. 1-7, 10, 12  \u2014 available: ", hint))
      })
    })

    sg_results <- reactiveVal(NULL)

    # Parse a free-text subject list supporting ranges (e.g. "1-7, 10, 12-14")
    parse_subject_tokens <- function(text, all_subj) {
      tokens <- trimws(strsplit(text, "[,\\s]+")[[1]])
      tokens <- tokens[nzchar(tokens)]
      is_numeric_subj <- all(grepl("^-?[0-9]+$", as.character(all_subj)))
      expanded <- unlist(lapply(tokens, function(tok) {
        if (grepl("^([0-9]+)-([0-9]+)$", tok)) {
          parts <- as.integer(regmatches(tok, regexec("^([0-9]+)-([0-9]+)$", tok))[[1]][2:3])
          as.character(parts[1]:parts[2])
        } else {
          tok
        }
      }))
      if (is_numeric_subj) {
        expanded <- suppressWarnings(as.numeric(expanded))
      }
      intersect(expanded, all_subj)
    }

    observeEvent(input$run_subgroup_abe, {
      pk <- tryCatch(trend_pk_table(), error = function(e) {
        showNotification(paste("Data error:", e$message), type = "error")
        NULL
      })
      req(pk)

      param    <- input$sg_parameter %||% "Cmax"
      sg_lo    <- (input$sg_be_lower %||% 80)  / 100
      sg_hi    <- (input$sg_be_upper %||% 125) / 100
      log_param <- paste0("ln", param)

      if (!log_param %in% names(pk)) {
        validate(need(param %in% names(pk),
                      sprintf("Parameter %s could not be found in data.", param)))
        pk[[log_param]] <- log(pk[[param]])
      }

      all_subj <- sort(unique(pk$subject))
      mode     <- input$subgroup_mode %||% "exclude"

      specs <- if (mode == "exclude") {
        excluded <- input$sg_exclude_subjects %||% character(0)
        kept     <- setdiff(all_subj, excluded)
        label    <- if (length(excluded) == 0) "All subjects"
                    else paste0("Excl. [", paste(excluded, collapse = ", "), "]")
        list(list(label = label, subjects = kept))
      } else {
        n <- max(1L, min(6L, as.integer(input$sg_n_groups %||% 2L)))
        lapply(seq_len(n), function(i) {
          subj <- parse_subject_tokens(
            input[[paste0("sg_subjects_", i)]] %||% "", all_subj)
          list(label = paste0("Subgroup ", i), subjects = subj)
        })
      }

      withProgress(message = "Running subgroup ABE...", value = 0.1, {
        rows <- lapply(specs, function(spec) {
          base_row <- data.frame(
            Subgroup   = spec$label,
            N          = length(spec$subjects),
            Parameter  = param,
            PE_pct     = NA_real_,
            CI_lower   = NA_real_,
            CI_upper   = NA_real_,
            BE_limits  = sprintf("[%.0f%%, %.0f%%]", sg_lo * 100, sg_hi * 100),
            Pass       = NA_character_,
            stringsAsFactors = FALSE
          )

          if (length(spec$subjects) < 4) {
            base_row$Pass <- "Need \u22654 subj."
            return(base_row)
          }

          subset_pk <- pk[pk$subject %in% spec$subjects, , drop = FALSE]
          res <- tryCatch(
            perform_subset_be_analysis(subset_pk, log_param, anova_method = "fixed"),
            error = function(e) NULL
          )

          if (is.null(res)) {
            base_row$Pass <- "Failed"
            return(base_row)
          }

          pass <- isTRUE(!is.na(res$ci_lower) && !is.na(res$ci_upper) &&
                         res$ci_lower >= sg_lo * 100 &&
                         res$ci_upper <= sg_hi * 100)

          base_row$PE_pct   <- res$point_estimate
          base_row$CI_lower <- res$ci_lower
          base_row$CI_upper <- res$ci_upper
          base_row$Pass     <- if (pass) "Yes" else "No"
          base_row
        })

        sg_results(do.call(rbind, rows))
      })
    })

    output$sg_results_table <- DT::renderDataTable({
      df <- sg_results()
      validate(need(!is.null(df), "Configure subgroups and click Run ABE."))
      num_cols <- which(sapply(df, is.numeric))

      dt <- DT::datatable(df,
                          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
                          rownames = FALSE) |>
        DT::formatRound(columns = num_cols, digits = 2)

      # Colour Pass column
      if ("Pass" %in% names(df)) {
        dt <- dt |>
          DT::formatStyle("Pass",
            backgroundColor = DT::styleEqual(c("Yes", "No"), c("#c6efce", "#ffc7ce")))
      }
      dt
    })

    # =========================================================================
    # Export
    # =========================================================================

    output$download_results <- downloadHandler(
      filename = function() {
        sprintf("anomaly_detection_pairwise_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        res <- pair_results()
        if (is.null(res)) {
          write.csv(data.frame(Note = "No results yet"), file, row.names = FALSE)
        } else {
          write.csv(res$table, file, row.names = FALSE)
        }
      }
    )

    # Local null-coalescing operator (in case not in scope)
    `%||%` <- function(a, b) if (is.null(a)) b else a
  })
}
