# Validation Module Server
#
# Wires the Validation UI to the R/validation_runner.R engine. Holds NO cache
# of BioEQ-computed results - every "Run" click invokes the runner fresh.

# Helper: render a per-dataset comparison panel (compact). Used for NCA
# datasets and any standalone result that isn't bundled into a BE group.
.validation_render_dataset_panel <- function(result) {
  status <- result$overall %||% "NOT_RUN"
  status_text <- switch(status,
    "PASS" = "PASS", "FAIL" = "FAIL",
    "NO_REFERENCE" = "NO REF", "DATA_MISSING" = "NO DATA",
    "ERROR" = "ERROR", "NOT_RUN" = "NOT RUN", status
  )
  layer <- toupper(result$layer %||% "")

  fmt <- function(x) {
    if (is.null(x) || length(x) == 0) return("-")
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(as.character(x))
    formatC(n, format = "f", digits = 2)
  }

  # ---- Body content -----------------------------------------------------
  body_content <- if (!is.null(result$error) && status %in% c("ERROR", "DATA_MISSING")) {
    div(class = "alert alert-warning", style = "margin: 0;", result$error)

  } else if (status == "NO_REFERENCE") {
    # No expected values yet. For NCA: show download button. For BE: show computed.
    computed_summary <- NULL
    if (!is.null(result$computed$study) && length(result$computed$study) > 0) {
      cs <- result$computed$study
      df <- data.frame(
        Parameter = names(cs),
        Computed  = vapply(cs, function(x) fmt(x[1]), character(1)),
        stringsAsFactors = FALSE
      )
      computed_summary <- tags$table(class = "table table-sm table-striped",
        style = "margin: 8px 0; font-size: 13px;",
        tags$thead(tags$tr(tags$th("Parameter"), tags$th("BioEQ"))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i)
          tags$tr(tags$td(df$Parameter[i]), tags$td(df$Computed[i]))))
      )
    }
    tagList(
      div(class = "alert alert-info", style = "margin: 0 0 8px 0; font-size: 13px;",
          "No reference values populated. BioEQ ran successfully."),
      if (layer == "NCA") downloadButton(
        paste0("dl_nca_", result$dataset_id),
        "Download full NCA results (CSV)",
        class = "btn-sm"
      ),
      computed_summary
    )

  } else if (!is.null(result$comparison)) {
    cmp <- result$comparison
    # Compact display: Parameter | Expected | BioEQ | Δ | Result
    rows <- lapply(seq_len(nrow(cmp)), function(i) {
      p   <- cmp$pass[i]
      lbl <- if (is.na(p)) "NO REF" else if (isTRUE(p)) "PASS" else "FAIL"
      colr <- if (is.na(p)) "#7f8c8d" else if (isTRUE(p)) "#27ae60" else "#c0392b"
      tags$tr(
        tags$td(cmp$parameter[i]),
        tags$td(fmt(cmp$expected_value[i])),
        tags$td(fmt(cmp$computed_value[i])),
        tags$td(fmt(cmp$deviation[i])),
        tags$td(style = sprintf("color:%s; font-weight:700;", colr), lbl)
      )
    })
    tbl <- tags$table(class = "table table-sm table-striped",
      style = "margin: 0; font-size: 13px;",
      tags$thead(tags$tr(
        tags$th("Parameter"), tags$th("Expected"),
        tags$th("BioEQ"), tags$th(HTML("&Delta;")), tags$th("Result")
      )),
      tags$tbody(rows)
    )
    extra <- NULL
    if (layer == "NCA") {
      extra <- div(style = "margin-top: 8px;",
        downloadButton(
          paste0("dl_nca_", result$dataset_id),
          "Download full NCA results (CSV)",
          class = "btn-sm"
        )
      )
    }
    tagList(tbl, extra)

  } else {
    div(class = "text-muted", em("No data."))
  }

  # ---- Header ----------------------------------------------------------
  status_color <- switch(status,
    "PASS" = "#27ae60", "FAIL" = "#c0392b",
    "ERROR" = "#c0392b", "DATA_MISSING" = "#7f8c8d",
    "NO_REFERENCE" = "#7f8c8d", "#7f8c8d"
  )
  s <- result$summary %||% c(total = 0, pass = 0, fail = 0, no_ref = 0)
  pass_n <- s[["pass"]] %||% 0; fail_n <- s[["fail"]] %||% 0
  total_n <- s[["total"]] %||% 0
  count_str <- if (total_n > 0) {
    sprintf("%d/%d", pass_n, total_n)
  } else "-"

  box(
    width = 12, collapsible = TRUE, collapsed = (status == "PASS"),
    title = tagList(
      span(style = "font-weight: 600;", result$name %||% result$dataset_id),
      span(style = "float: right; display: flex; gap: 12px; align-items: center;",
        span(style = sprintf("background: %s; color: white; padding: 2px 10px; border-radius: 10px; font-weight: 700; font-size: 11px;", status_color),
             status_text),
        span(style = "color: #7f8c8d; font-size: 12px;", count_str)
      )
    ),
    body_content
  )
}

# Helper: render a SINGLE collapsible panel that aggregates every dataset in
# one BE validation group (parallel / 2x2 / replicate) into one wide table
# matching the requested layout:
#
#   | <dataset> | Expected | <param1> | <param2> | ... | PASS/FAIL |
#   |           | BioEQ    | <param1> | <param2> | ... |           |
#
# Title is "<Group name> (PASS) k/n" or "(FAIL) k/n".
.validation_render_group_panel <- function(results, group_name) {
  if (length(results) == 0) return(NULL)

  fmt <- function(x) {
    if (is.null(x) || length(x) == 0) return("-")
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(as.character(x))
    formatC(n, format = "f", digits = 2)
  }

  # Strip common dataset_id prefixes for compact row labels (P01, A, rds01, ...)
  short_name <- function(id) {
    s <- sub("^fuglsang_parallel_", "", id)
    s <- sub("^schutz_2x2_", "", s)
    s <- sub("^replicateBE_", "", s)
    s
  }

  # Collect all parameter names across all results, preserving first-seen order
  param_order <- character()
  for (r in results) {
    cmp <- r$comparison
    if (is.null(cmp) || nrow(cmp) == 0) next
    for (p in cmp$parameter) if (!(p %in% param_order)) param_order <- c(param_order, p)
  }

  # Header row
  header <- tags$thead(tags$tr(
    tags$th(group_name),
    tags$th(""),                        # Expected/BioEQ label column
    lapply(param_order, function(p) tags$th(p)),
    tags$th("PASS/FAIL")
  ))

  body_rows <- list()
  n_pass <- 0
  n_total <- 0
  for (r in results) {
    n_total <- n_total + 1
    overall <- r$overall %||% "NOT_RUN"
    if (overall == "PASS") n_pass <- n_pass + 1
    pf_color <- switch(overall,
      "PASS" = "#27ae60", "FAIL" = "#c0392b",
      "NO_REFERENCE" = "#d35400", "#7f8c8d"
    )
    pf_text <- switch(overall,
      "PASS" = "PASS", "FAIL" = "FAIL",
      "NO_REFERENCE" = "NO REF", "DATA_MISSING" = "NO DATA",
      "ERROR" = "ERROR", overall
    )

    cmp <- r$comparison
    # Build per-parameter expected and computed values as named lookups
    exp_lookup <- setNames(rep(NA_character_, length(param_order)), param_order)
    obs_lookup <- setNames(rep(NA_character_, length(param_order)), param_order)
    if (!is.null(cmp) && nrow(cmp) > 0) {
      for (i in seq_len(nrow(cmp))) {
        pn <- cmp$parameter[i]
        exp_lookup[pn] <- fmt(cmp$expected_value[i])
        obs_lookup[pn] <- fmt(cmp$computed_value[i])
      }
    }

    label <- short_name(r$dataset_id)

    exp_row_style <- "background-color:#e8f8f0;"
    obs_row_style <- "background-color:#eef4fb;"
    exp_lbl_style <- "font-weight:700; color:#1a7a46; background-color:#e8f8f0;"
    obs_lbl_style <- "font-weight:700; color:#1a5fa8; background-color:#eef4fb;"

    body_rows[[length(body_rows) + 1]] <- tags$tr(
      style = exp_row_style,
      tags$td(rowspan = 2, style = "vertical-align: middle; font-weight: 600; background-color:#fff;", label),
      tags$td(style = exp_lbl_style, "Expected"),
      lapply(param_order, function(p) tags$td(style = exp_row_style, if (is.na(exp_lookup[p])) "-" else exp_lookup[p])),
      tags$td(rowspan = 2,
              style = sprintf("vertical-align: middle; color:%s; font-weight:700; background-color:#fff;", pf_color),
              pf_text)
    )
    body_rows[[length(body_rows) + 1]] <- tags$tr(
      style = obs_row_style,
      tags$td(style = obs_lbl_style, "BioEQ"),
      lapply(param_order, function(p) tags$td(style = obs_row_style, if (is.na(obs_lookup[p])) "-" else obs_lookup[p]))
    )
  }

  n_fail <- sum(vapply(results, function(r) identical(r$overall %||% "", "FAIL"), logical(1)))
  status_overall <- if (n_fail > 0) "FAIL" else if (n_pass == n_total && n_total > 0) "PASS" else "PARTIAL"
  status_color <- switch(status_overall,
    "PASS" = "#27ae60", "FAIL" = "#c0392b", "#d35400"
  )

  tbl <- div(
    style = "overflow-x: auto; width: 100%;",
    tags$table(
      class = "table table-sm table-bordered",
      style = "margin: 0; font-size: 13px; white-space: nowrap;",
      header,
      tags$tbody(body_rows)
    )
  )

  box(
    width = 12, collapsible = TRUE, collapsed = (status_overall == "PASS"),
    title = tagList(
      span(style = "font-weight: 600;",
           sprintf("%s (%s) %d/%d", group_name, status_overall, n_pass, n_total)),
      span(style = sprintf("float: right; background: %s; color: white; padding: 2px 10px; border-radius: 10px; font-weight: 700; font-size: 11px;", status_color),
           status_overall)
    ),
    tbl
  )
}

validation_server <- function(input, output, session) {

  # ---------------------------------------------------------------------
  # Load manifest + groups from the embedded store on session start
  # ---------------------------------------------------------------------
  manifest_rv <- reactiveVal(NULL)
  groups_rv   <- reactiveVal(NULL)
  observe({
    m <- tryCatch(load_validation_manifest(), error = function(e) {
      showNotification(paste("Failed to load validation manifest:", conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })
    g <- tryCatch(load_validation_groups(), error = function(e) {
      showNotification(paste("Failed to load validation groups:", conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })
    manifest_rv(m)
    groups_rv(g)
  })

  # Compute per-group summary stats from the manifest (ground truth: actual
  # number of datasets attached to each group_id and how many have data files).
  groups_with_stats <- reactive({
    g <- groups_rv()
    m <- manifest_rv()
    if (is.null(g) || is.null(m)) return(NULL)
    g$n_datasets_actual    <- vapply(g$group_id, function(gid) sum(m$group_id == gid, na.rm = TRUE), integer(1))
    g$n_datasets_runnable  <- vapply(g$group_id, function(gid) {
      ids <- m$dataset_id[!is.na(m$group_id) & m$group_id == gid]
      sum(vapply(ids, validation_dataset_available, logical(1)))
    }, integer(1))
    g$status <- ifelse(g$n_datasets_runnable == 0, "placeholder",
                ifelse(g$n_datasets_runnable < g$n_datasets_actual, "partial", "available"))
    g
  })

  # ---------------------------------------------------------------------
  # Groups table
  # ---------------------------------------------------------------------
  output$validation_groups_table <- DT::renderDT({
    g <- groups_with_stats()
    if (is.null(g) || nrow(g) == 0) {
      return(DT::datatable(data.frame(Message = "Validation groups not loaded.")))
    }
    # Build a "Source" cell that combines a tag + clickable link to the citation/URL
    src_cell <- vapply(seq_len(nrow(g)), function(i) {
      bioeq <- isTRUE(g$is_bioeq_generated[i])
      tag <- if (bioeq) {
        "<span class='validation-source-bioeq'>BioEQ</span>"
      } else {
        "<span class='validation-source-public'>PUBLIC</span>"
      }
      cite <- htmltools::htmlEscape(g$citation[i])
      url  <- g$reference_url[i]
      pmid <- if (!is.na(g$pmid[i]) && nzchar(g$pmid[i])) {
        sprintf("<br><span style='font-size:11px;color:#4a5568;'>PMID: <a href='https://pubmed.ncbi.nlm.nih.gov/%s/' target='_blank'>%s</a></span>",
                htmltools::htmlEscape(g$pmid[i]), htmltools::htmlEscape(g$pmid[i]))
      } else ""
      doi <- if (!is.na(g$doi[i]) && nzchar(g$doi[i])) {
        sprintf("<br><span style='font-size:11px;color:#4a5568;'>DOI: <a href='https://doi.org/%s' target='_blank'>%s</a></span>",
                htmltools::htmlEscape(g$doi[i]), htmltools::htmlEscape(g$doi[i]))
      } else ""
      cite_html <- if (!is.na(url) && nzchar(url)) {
        sprintf("<a href='%s' target='_blank'>%s</a>",
                htmltools::htmlEscape(url), cite)
      } else cite
      paste0(tag, "<br>", cite_html, pmid, doi)
    }, character(1))

    display <- data.frame(
      `Reference Data Set` = g$name,
      Source     = src_cell,
      Description = g$description,
      Status     = g$status,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    DT::datatable(
      display,
      rownames = FALSE,
      escape = FALSE,                                       # allow HTML in Source
      selection = list(mode = "multiple", target = "row"),
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'tip',
        columnDefs = list(
          list(targets = 0, width = "220px"),
          list(targets = 1, width = "260px"),
          list(targets = 2, width = "460px")
        )
      )
    ) |>
      DT::formatStyle("Status",
        target = "cell",
        backgroundColor = DT::styleEqual(
          c("available", "partial", "placeholder"),
          c("#d4edda",  "#fff3cd", "#f8d7da")
        )
      )
  })

  # ---------------------------------------------------------------------
  # Selected group detail panel
  # ---------------------------------------------------------------------
  output$validation_group_detail <- renderUI({
    g <- groups_with_stats()
    m <- manifest_rv()
    if (is.null(g) || is.null(m)) return(NULL)
    sel <- input$validation_groups_table_rows_selected
    if (length(sel) == 0) return(NULL)
    panels <- lapply(sel, function(i) {
      gid <- g$group_id[i]
      ds  <- m[!is.na(m$group_id) & m$group_id == gid, , drop = FALSE]
      bioeq <- isTRUE(g$is_bioeq_generated[i])
      links <- tagList(
        if (!is.na(g$reference_url[i]) && nzchar(g$reference_url[i]))
          tags$p(icon("external-link-alt"), " ",
                 tags$a(href = g$reference_url[i], target = "_blank",
                        "Open reference / source")),
        if (!is.na(g$pmid[i]) && nzchar(g$pmid[i]))
          tags$p(strong("PMID: "),
                 tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", g$pmid[i], "/"),
                        target = "_blank", g$pmid[i])),
        if (!is.na(g$doi[i]) && nzchar(g$doi[i]))
          tags$p(strong("DOI: "),
                 tags$a(href = paste0("https://doi.org/", g$doi[i]),
                        target = "_blank", g$doi[i]))
      )
      download_ctrl <- if (bioeq) {
        tagList(
          tags$p(strong("BioEQ-generated reference set:"),
                 " click below to download the bundled input data and ",
                 "expected reference values as a ZIP archive."),
          downloadButton(paste0("validation_download_group_", gid),
                         label = paste0("Download '", g$name[i], "' (.zip)"),
                         class = "btn-primary")
        )
      } else {
        tags$p(em("This is a publicly available reference set. Use the link ",
                  "above to access the original published data."))
      }
      ds_table <- if (nrow(ds) > 0) {
        DT::datatable(
          data.frame(
            ID = ds$dataset_id, Name = ds$name, Design = ds$design,
            N = ds$n_subjects, Status = ds$status,
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(pageLength = 5, scrollX = TRUE, dom = 'tip', searching = FALSE)
        )
      } else NULL
      div(class = "validation-group-detail",
        h4(g$name[i],
           tags$small(style = "margin-left: 10px; color: #4a5568;",
                      sprintf("(%d datasets, %d runnable)",
                              g$n_datasets_actual[i], g$n_datasets_runnable[i]))),
        tags$p(g$description[i]),
        links,
        download_ctrl,
        h5("Datasets in this group"),
        ds_table
      )
    })
    do.call(tagList, panels)
  })

  # Per-group BioEQ download handlers are not needed for the embedded store
  # (data is not user-accessible). This block is intentionally empty.

  # ---------------------------------------------------------------------
  # Run actions - ALWAYS computes fresh
  # ---------------------------------------------------------------------
  results_rv <- reactiveVal(NULL)

  do_run <- function(ids) {
    if (length(ids) == 0) {
      showNotification("No datasets selected.", type = "warning")
      return()
    }
    withProgress(message = "Running validation...", value = 0, {
      n <- length(ids)
      progress_fn <- function(i, total, id) {
        incProgress(1 / total, detail = paste0("[", i, "/", total, "] ", id))
      }
      # CRITICAL: this call performs analysis from raw data each time. No caching.
      results <- run_validation_suite(ids,
                                      progress_fn = progress_fn)
      results_rv(results)
    })
    showNotification(sprintf("Validation complete: %d dataset(s) processed.", length(ids)),
                     type = "message", duration = 4)
  }

  observeEvent(input$validation_run_all, {
    m <- manifest_rv()
    if (is.null(m)) return()
    # Only run datasets where the data file actually exists
    runnable <- m$dataset_id[vapply(m$dataset_id, validation_dataset_available, logical(1))]
    if (length(runnable) == 0) {
      showNotification("No validation datasets found in the embedded store.",
                       type = "warning", duration = 8)
      return()
    }
    do_run(runnable)
  })

  observeEvent(input$validation_run_selected, {
    g <- groups_with_stats()
    m <- manifest_rv()
    if (is.null(g) || is.null(m)) return()
    sel <- input$validation_groups_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select at least one group from the table.",
                       type = "warning")
      return()
    }
    selected_gids <- g$group_id[sel]
    ids <- m$dataset_id[!is.na(m$group_id) & m$group_id %in% selected_gids]
    # Filter to runnable
    ids <- ids[vapply(ids, validation_dataset_available, logical(1))]
    if (length(ids) == 0) {
      showNotification(
        paste0("No runnable datasets in the selected group(s). ",
               "Reference data files have not been populated yet - ",
               "see validation/datasets/PLACEHOLDERS.md."),
        type = "warning", duration = 8)
      return()
    }
    do_run(ids)
  })

  # ---------------------------------------------------------------------
  # Results rendering
  # ---------------------------------------------------------------------
  output$validation_run_summary <- renderUI({
    res <- results_rv()
    if (is.null(res)) {
      return(div(class = "validation-summary-box",
                 em("No validation runs yet. Click ",
                    strong("Run Complete Validation Set"),
                    " or select datasets from the table and click ",
                    strong("Run Selected Datasets"), ".")))
    }
    overall <- vapply(res, function(r) r$overall %||% "NOT_RUN", character(1))
    n_pass  <- sum(overall == "PASS")
    n_fail  <- sum(overall == "FAIL")
    n_noref <- sum(overall == "NO_REFERENCE")
    n_miss  <- sum(overall == "DATA_MISSING")
    n_err   <- sum(overall == "ERROR")
    started <- attr(res, "started_at")
    finished <- attr(res, "finished_at")
    div(class = "validation-summary-box",
      h4(sprintf("Validation run: %d dataset(s)", length(res))),
      p(
        span(class = "validation-status-PASS",         sprintf("%d PASS",  n_pass)), " | ",
        span(class = "validation-status-FAIL",         sprintf("%d FAIL",  n_fail)), " | ",
        span(class = "validation-status-NO_REFERENCE", sprintf("%d NO-REF", n_noref)), " | ",
        span(class = "validation-status-DATA_MISSING", sprintf("%d DATA-MISSING", n_miss)), " | ",
        span(class = "validation-status-ERROR",        sprintf("%d ERROR", n_err))
      ),
      p(em(sprintf("Started: %s | Finished: %s | Duration: %.2fs",
                   format(started, "%Y-%m-%d %H:%M:%S"),
                   format(finished, "%Y-%m-%d %H:%M:%S"),
                   as.numeric(difftime(finished, started, units = "secs")))))
    )
  })

  output$validation_results_panels <- renderUI({
    res <- results_rv()
    if (is.null(res) || length(res) == 0) return(NULL)
    m <- manifest_rv()
    g <- groups_rv()

    # Map dataset_id -> group_id
    if (is.null(m)) {
      gid_of <- function(id) NA_character_
    } else {
      gid_of <- function(id) {
        row <- m[m$dataset_id == id, , drop = FALSE]
        if (nrow(row) == 0) NA_character_ else row$group_id[1]
      }
    }
    gname_of <- function(gid) {
      if (is.null(g) || is.na(gid)) return(gid)
      row <- g[g$group_id == gid, , drop = FALSE]
      if (nrow(row) == 0) gid else row$name[1]
    }

    # Group ids that should render as a single combined table.
    combine_gids <- c("be_parallel", "be_2x2_crossover", "be_replicate_abel")

    # Bin results
    by_gid <- list()
    standalone <- list()
    for (r in res) {
      gid <- gid_of(r$dataset_id)
      if (!is.na(gid) && gid %in% combine_gids) {
        by_gid[[gid]] <- c(by_gid[[gid]], list(r))
      } else {
        standalone[[length(standalone) + 1]] <- r
      }
    }

    panels <- list()
    # Combined per-group panels (in display order)
    for (gid in combine_gids) {
      if (is.null(by_gid[[gid]])) next
      panels[[length(panels) + 1]] <- .validation_render_group_panel(
        by_gid[[gid]], gname_of(gid)
      )
    }
    # Standalone (NCA, etc.) appended after
    for (r in standalone) {
      panels[[length(panels) + 1]] <- .validation_render_dataset_panel(r)
    }
    do.call(tagList, panels)
  })

  # Register per-NCA-dataset full-results download handlers as results land.
  observe({
    res <- results_rv()
    if (is.null(res)) return()
    for (r in res) {
      if (!identical(toupper(r$layer %||% ""), "NCA")) next
      local({
        rr <- r
        out_id <- paste0("dl_nca_", rr$dataset_id)
        output[[out_id]] <- downloadHandler(
          filename = function() {
            sprintf("BioEQ_NCA_%s_%s.csv", rr$dataset_id,
                    format(Sys.time(), "%Y%m%d_%H%M%S"))
          },
          content = function(file) {
            df <- rr$computed$subject_df
            if (is.null(df) || nrow(df) == 0) {
              df <- data.frame(message = "No NCA results computed.")
            }
            utils::write.csv(df, file, row.names = FALSE)
          }
        )
      })
    }
  })

  output$validation_has_results <- reactive({ !is.null(results_rv()) })
  outputOptions(output, "validation_has_results", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------
  # Downloads
  # ---------------------------------------------------------------------
  output$validation_download_csv <- downloadHandler(
    filename = function() paste0("BioEQ_validation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      res <- results_rv()
      df <- if (!is.null(res)) validation_suite_to_dataframe(res) else data.frame()
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  output$validation_download_html <- downloadHandler(
    filename = function() paste0("BioEQ_validation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      res <- results_rv()
      df <- if (!is.null(res)) validation_suite_to_dataframe(res) else data.frame()
      html <- .validation_build_html_report(res, df)
      writeLines(html, file)
    }
  )

  output$validation_download_pdf <- downloadHandler(
    filename = function() paste0("BioEQ_validation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      res <- results_rv()
      df <- if (!is.null(res)) validation_suite_to_dataframe(res) else data.frame()
      ok <- tryCatch({
        if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("rmarkdown not installed")
        td <- tempfile(fileext = ".Rmd")
        writeLines(.validation_build_rmd(res, df), td)
        rmarkdown::render(td, output_format = "pdf_document",
                          output_file = file, quiet = TRUE)
        TRUE
      }, error = function(e) {
        # Fallback: write HTML with .pdf extension warning
        showNotification(paste("PDF rendering failed (", conditionMessage(e),
                               "). Use HTML export instead."),
                         type = "warning", duration = 8)
        writeLines(.validation_build_html_report(res, df), file)
        FALSE
      })
    }
  )

  # ---------------------------------------------------------------------
  # (No per-dataset query-param download handler in the groups-based UI;
  # downloads are surfaced per-group via the BioEQ ZIP download buttons.)
  # ---------------------------------------------------------------------
}

# ---------------------------------------------------------------------------
# HTML / Rmd report builders
# ---------------------------------------------------------------------------

.validation_build_html_report <- function(suite, df) {
  if (is.null(suite) || length(suite) == 0) {
    return("<html><body><h1>BioEQ Validation Report</h1><p>No results.</p></body></html>")
  }
  started  <- attr(suite, "started_at")
  finished <- attr(suite, "finished_at")

  esc <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }
  fmt2 <- function(x) {
    if (is.null(x) || length(x) == 0) return("-")
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(as.character(x))
    formatC(n, format = "f", digits = 2)
  }

  ds_html <- vapply(suite, function(r) {
    status <- r$overall %||% "NOT_RUN"
    sum_str <- if (!is.null(r$summary)) {
      sprintf("%d total | %d pass | %d fail | %d no-ref",
              r$summary[["total"]] %||% 0, r$summary[["pass"]] %||% 0,
              r$summary[["fail"]] %||% 0, r$summary[["no_ref"]] %||% 0)
    } else ""
    cmp_html <- if (!is.null(r$comparison) && nrow(r$comparison) > 0) {
      cmp <- r$comparison
      rows <- vapply(seq_len(nrow(cmp)), function(i) {
        pass <- cmp$pass[i]
        cls <- if (is.na(pass)) "noref" else if (isTRUE(pass)) "pass" else "fail"
        txt <- if (is.na(pass)) "NO REF" else if (isTRUE(pass)) "PASS" else "FAIL"
        sprintf(paste0(
          "<tr class='%s'>",
          "<td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
          "<td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
          "<td>%s</td><td><strong>%s</strong></td>",
          "</tr>"),
          cls,
          esc(cmp$parameter[i]), esc(cmp$scope[i]), esc(cmp$subject[i]), esc(cmp$treatment[i]),
          esc(fmt2(cmp$expected_value[i])), esc(fmt2(cmp$computed_value[i])),
          esc(cmp$tolerance_type[i]), esc(cmp$tolerance_value[i]),
          esc(if (is.na(cmp$deviation[i])) "-" else formatC(cmp$deviation[i], format = "f", digits = 2)),
          txt
        )
      }, character(1))
      paste0(
        "<table class='cmp'><thead><tr>",
        "<th>Parameter</th><th>Scope</th><th>Subject</th><th>Treatment</th>",
        "<th>Expected</th><th>Computed</th><th>Tol type</th><th>Tol value</th>",
        "<th>Deviation</th><th>Result</th>",
        "</tr></thead><tbody>",
        paste(rows, collapse = ""),
        "</tbody></table>"
      )
    } else if (!is.null(r$error)) {
      sprintf("<p class='error'>%s</p>", esc(r$error))
    } else {
      "<p><em>No comparison data.</em></p>"
    }
    sprintf(paste0(
      "<section class='ds %s'>",
      "<h3>%s <span class='status status-%s'>%s</span></h3>",
      "<p class='meta'>ID: <code>%s</code> | Layer: %s | Design: %s | Group: %s | %s</p>",
      "%s",
      "</section>"),
      status, esc(r$name %||% r$dataset_id), status, esc(status),
      esc(r$dataset_id), esc(r$layer), esc(r$design), esc(r$group), esc(sum_str),
      cmp_html)
  }, character(1))

  paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<title>BioEQ Validation Report</title>",
    "<style>",
    "body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "max-width:1200px;margin:24px auto;padding:0 16px;color:#1a202c;}",
    "h1{border-bottom:3px solid #2563eb;padding-bottom:6px;}",
    "table.cmp{border-collapse:collapse;width:100%;margin:12px 0;font-size:13px;}",
    "table.cmp th,table.cmp td{border:1px solid #cbd5e0;padding:6px 8px;text-align:left;}",
    "table.cmp th{background:#edf2f7;}",
    "tr.pass{background:#f0fdf4;} tr.fail{background:#fef2f2;} tr.noref{background:#fff7ed;}",
    "section.ds{margin:18px 0;padding:14px;border:1px solid #e2e8f0;border-radius:6px;",
    "border-left:5px solid #cbd5e0;}",
    "section.ds.PASS{border-left-color:#27ae60;}",
    "section.ds.FAIL{border-left-color:#c0392b;}",
    "section.ds.NO_REFERENCE{border-left-color:#d35400;}",
    "section.ds.DATA_MISSING{border-left-color:#95a5a6;}",
    "section.ds.ERROR{border-left-color:#c0392b;}",
    ".status{padding:2px 8px;border-radius:4px;font-size:13px;margin-left:8px;}",
    ".status-PASS{background:#27ae60;color:white;}",
    ".status-FAIL{background:#c0392b;color:white;}",
    ".status-NO_REFERENCE{background:#d35400;color:white;}",
    ".status-DATA_MISSING{background:#95a5a6;color:white;}",
    ".status-ERROR{background:#c0392b;color:white;}",
    ".meta{color:#4a5568;font-size:12px;margin:4px 0 12px;}",
    ".error{color:#c0392b;font-style:italic;}",
    "</style></head><body>",
    "<h1>BioEQ Black-Box Validation Report</h1>",
    sprintf("<p><strong>Started:</strong> %s &nbsp; <strong>Finished:</strong> %s &nbsp; <strong>Datasets:</strong> %d</p>",
            esc(format(started, "%Y-%m-%d %H:%M:%S")),
            esc(format(finished, "%Y-%m-%d %H:%M:%S")),
            length(suite)),
    "<p><em>This report was generated by running each BioEQ analysis from raw input data; ",
    "no cached or pre-computed BioEQ values were used.</em></p>",
    paste(ds_html, collapse = ""),
    "</body></html>"
  )
}

.validation_build_rmd <- function(suite, df) {
  paste0(
    "---\n",
    "title: 'BioEQ Black-Box Validation Report'\n",
    "date: '", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "'\n",
    "output: pdf_document\n",
    "---\n\n",
    "```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n```\n\n",
    "Datasets evaluated: ", length(suite), "\n\n",
    "```{r}\n",
    "df <- structure(", paste(deparse(df), collapse = ""), ", class='data.frame')\n",
    "knitr::kable(df[, intersect(c('dataset_id','parameter','expected_value','computed_value','deviation','pass','dataset_overall'), names(df))])\n",
    "```\n"
  )
}
