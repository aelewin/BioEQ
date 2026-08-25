# Randomization Server Module
# Backs the randomization_ui module: generates schedules via the engine in
# R/randomization.R, supports autofill from sample_size results, verification,
# and downloadable audit / report artefacts.

randomization_server <- function(id, ss_result = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Make the engine available (R/randomization.R is sourced from app.R).
    # If running this module standalone, source as a fallback.
    if (!exists("generate_randomization", mode = "function")) {
      r_dir <- if (exists(".BIOEQ_R_DIR")) .BIOEQ_R_DIR else "../R"
      try(source(file.path(r_dir, "randomization.R"), local = FALSE), silent = TRUE)
    }

    # =========================================================================
    # Random seed button
    # =========================================================================
    observeEvent(input$rand_seed, {
      updateNumericInput(session, "seed",
                         value = sample.int(.Machine$integer.max, 1L))
    })

    # =========================================================================
    # Autofill from Sample Size module
    # =========================================================================
    # Map sample_size designs -> randomization designs
    ss_to_rnd <- list(
      "parallel" = "parallel_2",
      "2x2x2"    = "crossover_2x2",
      "2x2x3"    = "replicate_2x2x3",
      "2x2x4"    = "replicate_2x2x4",
      "2x3x3"    = "partial_2x3x3"
    )

    .extract_n <- function(r) {
      if (is.null(r) || is.null(r$data)) return(NA_integer_)
      d <- r$data
      candidates <- c("Sample size", "n", "N", "Sample.size")
      for (col in candidates) {
        if (!is.null(d[[col]])) {
          v <- suppressWarnings(as.numeric(d[[col]]))
          v <- v[!is.na(v)]
          if (length(v) >= 1) return(as.integer(v[1]))
        }
      }
      # Last-ditch: scan numeric columns for a plausible sample size
      if (is.data.frame(d)) {
        for (col in names(d)) {
          v <- suppressWarnings(as.numeric(d[[col]]))
          v <- v[!is.na(v) & v >= 2 & v <= 10000 & v == round(v)]
          if (length(v) >= 1) return(as.integer(v[1]))
        }
      }
      NA_integer_
    }

    output$autofill_status <- renderUI({
      r <- if (is.function(ss_result)) tryCatch(ss_result(), error = function(e) NULL) else NULL
      if (is.null(r)) {
        return(div(class = "text-muted",
                   style = "font-size: 12px;",
                   icon("circle-info"),
                   " Run the Sample Size module first to enable autofill."))
      }
      n <- .extract_n(r)
      div(class = "alert alert-success", style = "padding: 6px 10px; font-size: 12px;",
          icon("circle-check"),
          sprintf(" Sample-size result available: %s, design = %s, N = %s.",
                  r$method, r$design, ifelse(is.na(n), "?", n)))
    })

    # Shared autofill action used by both the manual button and the auto-sync
    # observer below.
    .do_autofill <- function(verbose = TRUE) {
      r <- if (is.function(ss_result)) tryCatch(ss_result(), error = function(e) NULL) else NULL
      if (is.null(r)) {
        if (verbose) showNotification("No Sample Size result available. Run that module first.",
                                      type = "warning")
        return(invisible(FALSE))
      }
      design_key <- ss_to_rnd[[as.character(r$design)]]
      if (is.null(design_key)) {
        if (verbose) showNotification(
          sprintf("Sample-size design '%s' is not supported by the randomization module.", r$design),
          type = "warning")
        return(invisible(FALSE))
      }
      n <- .extract_n(r)

      # Update design first (so any input-validation tied to the design is
      # consistent), then push N. Use sendInputMessage as a belt-and-braces
      # alternative path in case updateNumericInput is racing with init.
      updateSelectInput(session, "design", selected = design_key)
      if (!is.na(n) && n >= 2) {
        updateNumericInput(session, "n_total", value = as.numeric(n))
        session$sendInputMessage("n_total", list(value = as.numeric(n)))
      }
      if (verbose) {
        showNotification(
          sprintf("Autofilled from Sample Size: design = %s, N = %s.",
                  design_key, ifelse(is.na(n), "?", n)),
          type = "message", duration = 4)
      }
      invisible(TRUE)
    }

    # Manual button
    observeEvent(input$autofill, {
      tryCatch(.do_autofill(verbose = TRUE),
               error = function(e) showNotification(
                 paste("Autofill error:", conditionMessage(e)), type = "error", duration = 6))
    }, ignoreInit = TRUE)

    # Auto-sync: whenever the sample-size result updates, push values into the
    # randomization inputs automatically (silent — no toast). The user can
    # still override afterwards.
    observe({
      r <- if (is.function(ss_result)) tryCatch(ss_result(), error = function(e) NULL) else NULL
      req(r)
      isolate(.do_autofill(verbose = FALSE))
    })

    # =========================================================================
    # Generate
    # =========================================================================
    rnd_result <- reactiveVal(NULL)

    .parse_strata <- function() {
      if (!isTRUE(input$use_strata)) return(NULL)
      out <- list()
      add <- function(nm, lv) {
        if (nzchar(nm) && nzchar(lv)) {
          parts <- trimws(strsplit(lv, ",", fixed = TRUE)[[1]])
          parts <- parts[nzchar(parts)]
          if (length(parts) >= 1) out[[nm]] <<- parts
        }
      }
      add(input$stratum1_name, input$stratum1_levels)
      add(input$stratum2_name, input$stratum2_levels)
      if (length(out) == 0) NULL else out
    }

    observeEvent(input$generate, {
      if (is.null(input$design) || !nzchar(input$design)) {
        showNotification("Select a study design before generating a schedule.",
                         type = "error")
        return()
      }
      if (is.null(input$n_total) || is.na(input$n_total) || input$n_total < 2) {
        showNotification("Enter a total sample size (N).", type = "error")
        return()
      }
      if (is.null(input$seed) || is.na(input$seed)) {
        showNotification("Enter an RNG seed, or click 'Random seed'.", type = "error")
        return()
      }
      if (isTRUE(input$use_blocks) &&
          (is.null(input$block_size) || is.na(input$block_size) || input$block_size <= 0)) {
        showNotification("Enter a group size, or turn off group randomization.",
                         type = "error")
        return()
      }
      tryCatch({
        bs <- if (isTRUE(input$use_blocks) && !is.null(input$block_size) &&
                  !is.na(input$block_size) && input$block_size > 0) {
                as.integer(input$block_size)
              } else {
                # No grouping: single big block = simple balanced random allocation
                as.integer(input$n_total)
              }
        res <- generate_randomization(
          design         = input$design,
          n_total        = as.integer(input$n_total),
          block_size     = bs,
          seed           = as.integer(input$seed),
          strata         = .parse_strata(),
          subject_prefix = input$subject_prefix,
          method_label   = if (isTRUE(input$use_blocks))
                              "Permuted block randomization (base R)"
                            else
                              "Simple random allocation, balanced (base R)"
        )
        # Tag display flags so output renderers can hide unused columns
        res$show_blocks <- isTRUE(input$use_blocks)
        res$show_strata <- !is.null(.parse_strata())
        rnd_result(res)
        showNotification("Schedule generated.", type = "message", duration = 3)
      }, error = function(e) {
        rnd_result(NULL)
        showNotification(paste("Generation failed:", e$message), type = "error",
                         duration = 8)
      })
    })

    # ---- Reset (Generate tab) ----------------------------------------------
    observeEvent(input$reset_generate, {
      updateSelectInput(session, "design", selected = "")
      updateNumericInput(session, "n_total", value = NA)
      updateCheckboxInput(session, "use_blocks", value = FALSE)
      updateNumericInput(session, "block_size", value = NA)
      updateNumericInput(session, "seed", value = NA)
      updateCheckboxInput(session, "use_strata", value = FALSE)
      updateTextInput(session, "stratum1_name",   value = "")
      updateTextInput(session, "stratum1_levels", value = "")
      updateTextInput(session, "stratum2_name",   value = "")
      updateTextInput(session, "stratum2_levels", value = "")
      updateTextInput(session, "subject_prefix",  value = "")
      rnd_result(NULL)
      showNotification("Generate Schedule inputs reset.", type = "message", duration = 3)
    })

    .strip_unused <- function(df, r) {
      drop <- character(0)
      if (!isTRUE(r$show_blocks)) drop <- c(drop, "Block")
      if (!isTRUE(r$show_strata)) drop <- c(drop, "Stratum")
      df[, setdiff(names(df), drop), drop = FALSE]
    }

    output$schedule_summary <- renderUI({
      r <- rnd_result()
      if (is.null(r)) return(NULL)
      n_total <- nrow(r$schedule)
      seqs <- unique(r$schedule$Sequence)
      n_seq <- length(seqs)
      per_seq <- floor(n_total / n_seq)
      div(style = "font-size: 13px; color: #555; margin-bottom: 8px;",
          tags$b("Total participants: "), n_total,
          tags$span(style = "margin: 0 10px; color: #ccc;", "|"),
          tags$b("Participants per sequence: "), per_seq
      )
    })

    output$schedule_table <- DT::renderDataTable({
      r <- rnd_result()
      validate(need(!is.null(r), "Set parameters and click 'Generate Schedule'."))
      df <- .strip_unused(r$schedule, r)
      front <- intersect(c("Subject", "Sequence", "Stratum", "Block"), names(df))
      df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE,
                                   dom = "rtip"))
    })

    output$schedule_long_table <- DT::renderDataTable({
      r <- rnd_result()
      validate(need(!is.null(r), ""))
      df <- .strip_unused(r$schedule_long, r)
      front <- intersect(c("Subject", "Period", "Treatment", "Sequence",
                           "Stratum", "Block"), names(df))
      df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE,
                                   dom = "rtip"))
    })

    output$balance_table <- DT::renderDataTable({
      r <- rnd_result()
      validate(need(!is.null(r), ""))
      tab <- as.data.frame(table(Stratum = r$schedule$Stratum,
                                 Sequence = r$schedule$Sequence))
      DT::datatable(tab, rownames = FALSE,
                    options = list(pageLength = 10, dom = "tip"))
    })

    # ---- Downloads ---------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() sprintf("randomization_seed%s_%s.csv",
                                    input$seed, format(Sys.Date(), "%Y%m%d")),
      content  = function(file) {
        r <- rnd_result()
        req(r)
        write.csv(.strip_unused(r$schedule, r), file, row.names = FALSE)
      }
    )
    output$dl_long <- downloadHandler(
      filename = function() sprintf("randomization_long_seed%s_%s.csv",
                                    input$seed, format(Sys.Date(), "%Y%m%d")),
      content  = function(file) {
        r <- rnd_result()
        req(r)
        write.csv(.strip_unused(r$schedule_long, r), file, row.names = FALSE)
      }
    )

    # =========================================================================
    # Audit / Report
    # =========================================================================

    # Just the reproduction code (no surrounding prose) — shared by the plain
    # -text audit's "Verification" section and the HTML report's.
    .repro_code <- function(m) {
      paste0(
        "  source('R/randomization.R')\n",
        "  res <- generate_randomization(\n",
        sprintf("    design     = '%s',\n", m$design),
        sprintf("    n_total    = %d,\n",   m$n_total),
        sprintf("    block_size = %d,\n",   m$block_size),
        sprintf("    seed       = %d%s)\n",  m$seed,
                if (!is.null(m$strata) && length(m$strata) > 0)
                  sprintf(",\n    strata = %s", deparse(m$strata)) else ""),
        "  identical(res$meta$schedule_hash, '", m$schedule_hash, "')\n"
      )
    }

    .audit_text <- function() {
      r <- rnd_result()
      if (is.null(r)) return("No schedule generated yet.")
      m <- r$meta
      strata_str <- if (is.null(m$strata) || length(m$strata) == 0) "None"
                    else paste(sapply(names(m$strata), function(n)
                      sprintf("%s = {%s}", n, paste(m$strata[[n]], collapse = ", "))),
                      collapse = "; ")
      paste0(
        "BioEQ Randomization Audit Record\n",
        "================================\n",
        sprintf("Generated:        %s\n", m$generated_at),
        sprintf("Design:           %s (%s)\n", m$design_label, m$design),
        sprintf("Sequences:        %s\n", paste(m$sequences, collapse = ", ")),
        sprintf("Periods:          %d\n", m$periods),
        sprintf("Total N:          %d\n", m$n_total),
        sprintf("Block size:       %d\n", m$block_size),
        sprintf("Stratification:   %s\n", strata_str),
        sprintf("Subject prefix:   %s\n", m$subject_prefix),
        "\n",
        "Reproducibility parameters\n",
        "--------------------------\n",
        sprintf("Algorithm:        %s\n", m$algorithm),
        sprintf("RNG kind:         %s\n", m$rng_kind),
        sprintf("Sample kind:      %s\n", m$sample_kind),
        sprintf("Normal kind:      %s\n", m$normal_kind),
        sprintf("Seed:             %d\n", m$seed),
        sprintf("R version:        %s\n", m$R_version),
        sprintf("Package:          %s\n", m$package),
        sprintf("Schedule SHA-256: %s\n", m$schedule_hash),
        "\n",
        "Verification\n",
        "------------\n",
        "Reproduce in R (>= 3.6.0):\n",
        .repro_code(m)
      )
    }

    output$audit_text <- renderText({ .audit_text() })

    output$dl_report <- downloadHandler(
      filename = function() sprintf("randomization_report_seed%s_%s.html",
                                    input$seed, format(Sys.Date(), "%Y%m%d")),
      content  = function(file) {
        r <- rnd_result()
        req(r)
        m <- r$meta
        # Self-contained HTML — no rmarkdown dependency
        html <- c(
          "<!doctype html><html><head><meta charset='utf-8'>",
          "<title>BioEQ Randomization Audit Record</title>",
          "<style>",
          "body{font-family:'Inter',Arial,sans-serif;max-width:1000px;margin:30px auto;padding:0 20px;color:#1f2937;}",
          "h1{color:#1e3a5f;border-bottom:2px solid #2c5282;padding-bottom:6px;}",
          "h2{color:#2c5282;margin-top:28px;}",
          "table{border-collapse:collapse;width:100%;margin:8px 0 18px;font-size:13px;}",
          "th,td{border:1px solid #e2e8f0;padding:6px 10px;text-align:left;}",
          "th{background:#f1f5f9;}",
          ".meta td:first-child{width:220px;font-weight:600;background:#f8fafc;}",
          "pre{background:#f8fafc;border:1px solid #e2e8f0;padding:10px;font-size:12px;}",
          ".sig{margin-top:40px;border-top:1px dashed #94a3b8;padding-top:14px;font-size:13px;}",
          "@media print{",
          "  body{margin:12px;max-width:none;}",
          "  h1,h2{break-after:avoid;page-break-after:avoid;}",
          "  pre{break-inside:avoid;page-break-inside:avoid;}",
          "  table tr{break-inside:avoid;page-break-inside:avoid;}",
          "}",
          "</style></head><body>",
          "<h1>BioEQ Randomization Audit Record</h1>",
          sprintf("<p><strong>Generated:</strong> %s</p>", m$generated_at),

          "<h2>Study Parameters</h2><table class='meta'>",
          sprintf("<tr><td>Design</td><td>%s (<code>%s</code>)</td></tr>", m$design_label, m$design),
          sprintf("<tr><td>Sequences</td><td>%s</td></tr>", paste(m$sequences, collapse = ", ")),
          sprintf("<tr><td>Periods</td><td>%d</td></tr>", m$periods),
          sprintf("<tr><td>Total N</td><td>%d</td></tr>", m$n_total),
          sprintf("<tr><td>Block size</td><td>%d</td></tr>", m$block_size),
          sprintf("<tr><td>Stratification</td><td>%s</td></tr>",
                  if (is.null(m$strata)) "None" else
                    paste(sapply(names(m$strata), function(n)
                      sprintf("%s = {%s}", n, paste(m$strata[[n]], collapse = ", "))),
                      collapse = "; ")),
          "</table>",

          "<h2>Reproducibility Parameters</h2><table class='meta'>",
          sprintf("<tr><td>Algorithm</td><td>%s</td></tr>", m$algorithm),
          sprintf("<tr><td>RNG kind</td><td>%s</td></tr>", m$rng_kind),
          sprintf("<tr><td>Sample kind</td><td>%s</td></tr>", m$sample_kind),
          sprintf("<tr><td>Normal kind</td><td>%s</td></tr>", m$normal_kind),
          sprintf("<tr><td>Seed</td><td>%d</td></tr>", m$seed),
          sprintf("<tr><td>R version</td><td>%s</td></tr>", m$R_version),
          sprintf("<tr><td>Package</td><td>%s</td></tr>", m$package),
          sprintf("<tr><td>Schedule SHA-256</td><td><code>%s</code></td></tr>", m$schedule_hash),
          "</table>",

          "<h2>Sequence Balance</h2>",
          .df_to_html(as.data.frame(table(Stratum = r$schedule$Stratum,
                                          Sequence = r$schedule$Sequence))),

          "<h2>Verification</h2>",
          "<pre>",
          .escape_html(paste0("Reproduce in R (>= 3.6.0):\n", .repro_code(m))),
          "</pre>",

          "<h2>Randomization Schedule (by Subject)</h2>",
          .df_to_html(r$schedule),

          "<div class='sig'>",
          "<p><strong>Prepared by:</strong> ___________________________&nbsp;&nbsp;",
          "<strong>Date:</strong> ____________</p>",
          "<p><strong>Reviewed by (Pharmacist):</strong> ___________________________&nbsp;&nbsp;",
          "<strong>Date:</strong> ____________</p>",
          "</div>",
          "</body></html>"
        )
        writeLines(html, file)
      }
    )

    # =========================================================================
    # Tab 2: Verify
    # =========================================================================
    verify_state <- reactiveVal(NULL)

    .read_provided <- function(file_info) {
      if (is.null(file_info)) return(NULL)
      ext <- tolower(tools::file_ext(file_info$name))
      tryCatch({
        if (ext %in% c("csv", "txt")) read.csv(file_info$datapath, stringsAsFactors = FALSE)
        else if (ext == "tsv")        read.delim(file_info$datapath, stringsAsFactors = FALSE)
        else if (ext %in% c("xls", "xlsx")) as.data.frame(readxl::read_excel(file_info$datapath))
        else                          read.csv(file_info$datapath, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification(paste("Failed to read provided schedule:", e$message),
                         type = "error")
        NULL
      })
    }

    .parse_v_strata <- function() {
      if (!isTRUE(input$v_use_strata)) return(NULL)
      out <- list()
      add <- function(nm, lv) {
        if (nzchar(nm) && nzchar(lv)) {
          parts <- trimws(strsplit(lv, ",", fixed = TRUE)[[1]])
          parts <- parts[nzchar(parts)]
          if (length(parts) >= 1) out[[nm]] <<- parts
        }
      }
      add(input$v_stratum1_name, input$v_stratum1_levels)
      add(input$v_stratum2_name, input$v_stratum2_levels)
      if (length(out) == 0) NULL else out
    }

    observeEvent(input$verify, {
      if (is.null(input$v_design) || !nzchar(input$v_design)) {
        showNotification("Select a study design before verifying.", type = "error")
        return()
      }
      if (is.null(input$v_n_total) || is.na(input$v_n_total) || input$v_n_total < 2) {
        showNotification("Enter a total sample size (N).", type = "error")
        return()
      }
      if (is.null(input$v_seed) || is.na(input$v_seed)) {
        showNotification("Enter the RNG seed used to generate the schedule.", type = "error")
        return()
      }
      if (isTRUE(input$v_use_blocks) &&
          (is.null(input$v_block_size) || is.na(input$v_block_size) || input$v_block_size <= 0)) {
        showNotification("Enter the group size that was used.", type = "error")
        return()
      }
      tryCatch({
        prov <- .read_provided(input$v_file)
        bs <- if (isTRUE(input$v_use_blocks)) as.integer(input$v_block_size) else NULL
        result <- verify_randomization(
          list(
            design         = input$v_design,
            n_total        = as.integer(input$v_n_total),
            block_size     = bs,
            seed           = as.integer(input$v_seed),
            strata         = .parse_v_strata(),
            subject_prefix = input$v_subject_prefix
          ),
          provided_schedule = prov
        )
        show_blocks <- isTRUE(input$v_use_blocks)
        show_strata <- !is.null(.parse_v_strata())
        verify_state(list(result = result, had_provided = !is.null(prov),
                          show_blocks = show_blocks, show_strata = show_strata))
      }, error = function(e) {
        verify_state(NULL)
        showNotification(paste("Verification failed:", e$message),
                         type = "error", duration = 8)
      })
    })

    # ---- Reset (Verify tab) ------------------------------------------------
    observeEvent(input$reset_verify, {
      updateSelectInput(session, "v_design", selected = "")
      updateNumericInput(session, "v_n_total", value = NA)
      updateCheckboxInput(session, "v_use_blocks", value = FALSE)
      updateNumericInput(session, "v_block_size", value = NA)
      updateNumericInput(session, "v_seed", value = NA)
      updateCheckboxInput(session, "v_use_strata", value = FALSE)
      updateTextInput(session, "v_stratum1_name",   value = "")
      updateTextInput(session, "v_stratum1_levels", value = "")
      updateTextInput(session, "v_stratum2_name",   value = "")
      updateTextInput(session, "v_stratum2_levels", value = "")
      updateTextInput(session, "v_subject_prefix",  value = "")
      if (exists("shinyjs_available") && shinyjs_available) {
        shinyjs::reset("v_file")
      }
      verify_state(NULL)
      showNotification("Verify Schedule inputs reset.", type = "message", duration = 3)
    })

    output$verify_status <- renderUI({
      v <- verify_state()
      if (is.null(v)) return(div(class = "text-muted",
                                 "Run the verification to see results."))
      r <- v$result
      if (!v$had_provided) {
        return(div(class = "alert alert-info",
                   icon("circle-info"),
                   " No comparison file uploaded. Showing the regenerated schedule below.",
                   sprintf(" Hash: %s", r$meta$schedule_hash)))
      }
      if (isTRUE(r$match)) {
        div(class = "alert alert-success",
            icon("circle-check"), strong(" MATCH."),
            sprintf(" All %d subject assignments are identical to the regenerated schedule.",
                    r$n_compared),
            br(), sprintf("Hash: %s", r$meta$schedule_hash))
      } else {
        div(class = "alert alert-danger",
            icon("triangle-exclamation"), strong(" MISMATCH."),
            sprintf(" %d differing rows out of %d. Inspect the table for details.",
                    nrow(r$diffs), r$n_compared))
      }
    })

    output$verify_table <- DT::renderDataTable({
      v <- verify_state()
      validate(need(!is.null(v), ""))
      r <- v$result
      df <- if (!v$had_provided || isTRUE(r$match)) r$regenerated else r$diffs
      # Drop unused columns
      drop <- character(0)
      if (!isTRUE(v$show_blocks)) drop <- c(drop, "Block")
      if (!isTRUE(v$show_strata)) drop <- c(drop, "Stratum")
      df <- df[, setdiff(names(df), drop), drop = FALSE]
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE,
                                   dom = "rtip"))
    })

    # Expose for app.R if needed
    return(list(result = rnd_result))
  })
}

# Helpers ---------------------------------------------------------------------
.escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}
.df_to_html <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("<p><em>(empty)</em></p>")
  hdr <- paste0("<th>", .escape_html(names(df)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- paste0("<td>", .escape_html(as.character(unlist(df[i, ]))), "</td>",
                    collapse = "")
    paste0("<tr>", cells, "</tr>")
  }, character(1))
  paste0("<table><thead><tr>", hdr, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}
