# Sample Size Report
#
# Self-contained HTML report for a PowerTOST-based sample size calculation.
# Reuses the same visual style (class names / CSS) as the BE analysis report
# (shiny/utils/sas_style_report.R) for a consistent look across BioEQ's
# reports. Sections:
#   1. Calculation Parameters (method, design, CV, theta0/1/2, alpha, power)
#   2. Result (sample size, achieved power)
#   3. PowerTOST Console Output (verbatim, for reproducibility)

.ss_method_label <- function(method) {
  switch(method,
    "ABE"   = "Average Bioequivalence",
    "ABEL"  = "Scaled ABE — Expanding Limits (ABEL)",
    "RSABE" = "Reference-Scaled ABE (FDA)",
    "NTID"  = "Narrow Therapeutic Index Drugs",
    method
  )
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------
generate_sample_size_report <- function(ss_res, output_file) {
  if (is.null(ss_res) || is.null(ss_res$data)) {
    writeLines("<html><body><h1>BioEQ Sample Size Report</h1><p>No calculation available.</p></body></html>",
               output_file)
    return(invisible(NULL))
  }

  esc <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  n_total <- ss_res$data[["Sample size"]]
  achieved_power <- ss_res$data[["Achieved power"]]
  method_label <- .ss_method_label(ss_res$method)
  console_text <- paste(ss_res$console %||% character(0), collapse = "\n")

  header_html <- paste0(
    "<div class='sas-header'>",
    "<h1>BioEQ &mdash; Sample Size Calculation Report</h1>",
    "<p class='sas-meta'>Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
    "<table class='sas-meta-tbl'>",
    "<tr><th>Method</th><td>", esc(method_label), "</td></tr>",
    "<tr><th>Design</th><td>", esc(ss_res$design), "</td></tr>",
    if (!is.null(ss_res$regulator))
      paste0("<tr><th>Regulator</th><td>", esc(ss_res$regulator), "</td></tr>") else "",
    "</table></div>"
  )

  params_block <- paste0(
    "<table class='sas-table'>",
    "<thead><tr><th>Parameter</th><th>Value</th></tr></thead><tbody>",
    sprintf("<tr><td>Within-Subject CV</td><td>%.2f%% (%.4f)</td></tr>", ss_res$cv * 100, ss_res$cv),
    sprintf("<tr><td>Assumed T/R Ratio (Δ)</td><td>%.4f</td></tr>", ss_res$theta0),
    sprintf("<tr><td>BE Acceptance Limits (θ₁ – θ₂)</td><td>%.2f – %.2f</td></tr>",
            ss_res$theta1, ss_res$theta2),
    sprintf("<tr><td>Target Power</td><td>%.0f%%</td></tr>", ss_res$target_power * 100),
    sprintf("<tr><td>Alpha</td><td>%.3f</td></tr>", ss_res$alpha),
    "</tbody></table>"
  )

  result_block <- paste0(
    "<div class='ss-result-cards'>",
    "<div class='ss-card'><div class='ss-card-value'>", n_total,
    "</div><div class='ss-card-label'>Total Subjects</div></div>",
    "<div class='ss-card'><div class='ss-card-value'>", sprintf("%.1f%%", achieved_power * 100),
    "</div><div class='ss-card-label'>Achieved Power</div></div>",
    "<div class='ss-card'><div class='ss-card-value'>", sprintf("%.0f%%", ss_res$cv * 100),
    "</div><div class='ss-card-label'>Within-Subject CV</div></div>",
    "</div>"
  )

  # Dropout-adjusted enrollment (optional — only when a dropout rate was entered)
  dropout_block <- if (!is.null(ss_res$dropout_info)) {
    di <- ss_res$dropout_info
    paste0(
      "<h2 class='sas-section'>3. Dropout-Adjusted Enrollment</h2>",
      sprintf(paste0(
        "<p class='sas-note'>PowerTOST's %d-subject result is the number expected to complete ",
        "the study. To end up with that many completers after an anticipated %.0f%% dropout ",
        "rate, enroll additional subjects, rounded up so all %d group(s) stay equal in size.</p>"),
        di$n_base, di$dropout_pct, di$n_groups),
      "<table class='sas-table'>",
      "<thead><tr><th>Quantity</th><th>Value</th></tr></thead><tbody>",
      sprintf("<tr><td>PowerTOST N (before dropout)</td><td>%d</td></tr>", di$n_base),
      sprintf("<tr><td>Anticipated Dropout Rate</td><td>%.0f%%</td></tr>", di$dropout_pct),
      sprintf("<tr><td>Additional Subjects Needed (raw)</td><td>%.2f</td></tr>", di$extra_raw),
      sprintf("<tr><td>Additional Subjects Added (rounded up to a multiple of %d)</td><td>%d</td></tr>",
              di$n_groups, di$extra_used),
      sprintf("<tr><td><strong>Adjusted Total Enrollment</strong></td><td><strong>%d</strong></td></tr>",
              di$n_adjusted),
      sprintf("<tr><td>Per Group (before / after)</td><td>%s / %s</td></tr>",
              format(di$per_group_base), format(di$per_group_adjusted)),
      "</tbody></table>"
    )
  } else ""

  console_block <- if (nzchar(console_text))
    paste0("<pre class='sas-code'>", esc(console_text), "</pre>") else ""

  body <- paste0(
    header_html,
    "<h2 class='sas-section'>1. Calculation Parameters</h2>", params_block,
    "<h2 class='sas-section'>2. Result (PowerTOST)</h2>", result_block,
    dropout_block,
    if (nzchar(console_block)) paste0(
      "<h2 class='sas-section'>", if (nzchar(dropout_block)) "4" else "3",
      ". PowerTOST Console Output</h2>", console_block) else "",
    "<hr class='sas-rule'>",
    "<p class='sas-footer'>Generated by BioEQ using the PowerTOST package. ",
    "Sample size is per treatment arm (parallel) or total subjects (crossover/replicate), ",
    "as reported by PowerTOST for the selected design. Dropout-adjusted enrollment, when shown, ",
    "is a BioEQ calculation (not part of PowerTOST's own output).</p>"
  )

  css <- "
    body { font-family: 'Helvetica Neue', Arial, sans-serif; color:#212529;
           margin:32px; line-height:1.45; max-width:1000px; }
    .sas-header { border-bottom:2px solid #1e3a5f; padding-bottom:12px; margin-bottom:24px; }
    .sas-header h1 { color:#1e3a5f; margin:0 0 4px 0; font-size:22px; }
    .sas-meta { color:#6c757d; font-size:12px; margin:0 0 10px 0; }
    table.sas-meta-tbl { border-collapse:collapse; font-size:13px; }
    table.sas-meta-tbl th { text-align:left; padding:2px 14px 2px 0; color:#495057; font-weight:600; }
    table.sas-meta-tbl td { padding:2px 0; }
    h2.sas-section { color:#1e3a5f; border-bottom:1px solid #dee2e6; margin-top:32px;
                     padding-bottom:4px; font-size:17px; }
    table.sas-table { border-collapse:collapse; margin:6px 0 16px 0; font-size:13px; min-width:50%; }
    table.sas-table th, table.sas-table td { border:1px solid #ced4da; padding:6px 12px; text-align:left; }
    table.sas-table thead th { background:#f1f3f5; color:#212529; font-weight:600; }
    table.sas-table tbody tr:nth-child(even) { background:#fafbfc; }
    p.sas-note { color:#6c757d; font-size:12px; font-style:italic; margin:4px 0 12px 0; }
    p.sas-footer { color:#6c757d; font-size:11px; margin-top:24px; }
    hr.sas-rule { border:none; border-top:1px dashed #ced4da; margin:18px 0; }
    pre.sas-code { background:#1e293b; color:#e2e8f0; border-radius:6px; padding:14px 16px;
                   font-size:12px; line-height:1.5; overflow-x:auto; white-space:pre;
                   font-family:'SF Mono',Consolas,'Courier New',monospace; max-height:420px;
                   overflow-y:auto; }
    .ss-result-cards { display:flex; gap:16px; margin:12px 0 20px 0; }
    .ss-card { flex:1; background:#f7fafc; border:1px solid #cbd5e0; border-radius:10px;
               padding:18px; text-align:center; }
    .ss-card-value { font-size:32px; font-weight:700; color:#1e3a5f; }
    .ss-card-label { font-size:13px; color:#4a5568; font-weight:600; margin-top:4px; }

    @media print {
      body { margin:12px; max-width:none; }
      h2.sas-section { break-after:avoid; page-break-after:avoid; }
      table.sas-table, .ss-result-cards, pre.sas-code {
        break-inside:avoid; page-break-inside:avoid; }
    }
  "

  html <- paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
    "<title>BioEQ – Sample Size Report</title>",
    "<style>", css, "</style></head><body>", body, "</body></html>"
  )
  writeLines(html, output_file)
  invisible(NULL)
}
