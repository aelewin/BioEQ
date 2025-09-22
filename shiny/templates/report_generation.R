# BioEQ Report Generation Functions
# Functions for generating regulatory-compliant bioequivalence analysis reports

#' Generate ANOVA Analysis Report
#' 
#' @param anova_results Results from ANOVA analysis
#' @param nca_results Results from NCA analysis
#' @param study_info Study design information
#' @param format Output format ("html", "pdf", "word")
#' @return Path to generated report file
#' @export
generate_anova_report <- function(anova_results, nca_results = NULL, study_info = NULL, format = "html") {
  
  # Validate inputs
  if (is.null(anova_results)) {
    stop("ANOVA results are required for report generation")
  }
  
  # Create report content
  report_content <- create_report_content(anova_results, nca_results, study_info)
  
  # Generate report based on format
  if (format == "html") {
    return(generate_html_report(report_content))
  } else if (format == "pdf") {
    return(generate_pdf_report(report_content))
  } else if (format == "word") {
    return(generate_word_report(report_content))
  } else {
    stop("Unsupported format. Use 'html', 'pdf', or 'word'")
  }
}

#' Create structured report content
#' @param anova_results ANOVA analysis results
#' @param nca_results NCA analysis results (optional)
#' @param study_info Study information (optional)
#' @return List with structured report content
create_report_content <- function(anova_results, nca_results, study_info) {
  
  # Report header
  header <- list(
    title = "Bioequivalence Analysis Report",
    subtitle = "ANOVA Statistical Analysis",
    date = Sys.Date(),
    time = Sys.time(),
    generated_by = "BioEQ Analysis Platform",
    version = "BETA"
  )
  
  # Study information
  study <- list(
    design = study_info$study_design %||% "2x2x2 Crossover",
    alpha = study_info$alpha_level %||% 0.05,
    be_limits = study_info$be_limits %||% list(lower = 80, upper = 125),
    regulatory_standard = study_info$regulatory_standard %||% "FDA"
  )
  
  # Analysis summary
  be_results <- anova_results$bioequivalence
  n_parameters <- nrow(be_results)
  n_bioequivalent <- sum(be_results$Bioequivalent == "Yes", na.rm = TRUE)
  
  summary <- list(
    n_subjects = length(unique(anova_results$prepared_data$Subject)),
    n_parameters = n_parameters,
    n_bioequivalent = n_bioequivalent,
    overall_conclusion = ifelse(n_bioequivalent == n_parameters, 
                               "BIOEQUIVALENT", "NOT BIOEQUIVALENT"),
    parameters_analyzed = be_results$Parameter
  )
  
  # Detailed results
  results <- list(
    bioequivalence_table = be_results,
    anova_tables = anova_results$anova_tables,
    confidence_intervals = be_results[, c("Parameter", "GMR", "Lower_CI", "Upper_CI", "Bioequivalent")]
  )
  
  # Conclusions
  conclusions <- create_conclusions(be_results, study)
  
  return(list(
    header = header,
    study = study,
    summary = summary,
    results = results,
    conclusions = conclusions
  ))
}

#' Create report conclusions
#' @param be_results Bioequivalence results
#' @param study_info Study information
#' @return Character vector of conclusions
create_conclusions <- function(be_results, study_info) {
  
  conclusions <- c()
  
  # Overall conclusion
  all_bioequivalent <- all(be_results$Bioequivalent == "Yes", na.rm = TRUE)
  
  if (all_bioequivalent) {
    conclusions <- c(conclusions, 
      paste("Based on the ANOVA analysis of the", nrow(be_results), 
            "pharmacokinetic parameters, the test formulation is bioequivalent",
            "to the reference formulation."))
  } else {
    conclusions <- c(conclusions,
      paste("Based on the ANOVA analysis, the test formulation is NOT bioequivalent",
            "to the reference formulation for one or more parameters."))
  }
  
  # Parameter-specific conclusions
  for (i in 1:nrow(be_results)) {
    param <- be_results$Parameter[i]
    is_be <- be_results$Bioequivalent[i] == "Yes"
    gmr <- round(be_results$GMR[i], 4)
    lower_ci <- round(be_results$Lower_CI[i], 2)
    upper_ci <- round(be_results$Upper_CI[i], 2)
    
    if (is_be) {
      conclusions <- c(conclusions,
        paste("For", param, ": GMR =", gmr, 
              "with 90% CI [", lower_ci, ",", upper_ci, "] - BIOEQUIVALENT"))
    } else {
      conclusions <- c(conclusions,
        paste("For", param, ": GMR =", gmr,
              "with 90% CI [", lower_ci, ",", upper_ci, "] - NOT BIOEQUIVALENT"))
    }
  }
  
  # Statistical notes
  conclusions <- c(conclusions,
    "",
    "Statistical Notes:",
    paste("- Analysis performed using", study_info$regulatory_standard, "guidelines"),
    paste("- Significance level (α):", study_info$alpha),
    paste("- Bioequivalence limits:", study_info$be_limits$lower, "- ", study_info$be_limits$upper, "%"),
    "- 90% confidence intervals calculated using ANOVA mixed-effects model",
    "- Log-transformation applied to AUC and Cmax parameters"
  )
  
  return(conclusions)
}

#' Generate HTML report
#' @param content Structured report content
#' @return Path to HTML file
generate_html_report <- function(content) {
  
  # Create temporary HTML file
  temp_file <- tempfile(fileext = ".html")
  
  # HTML template
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>", content$header$title, "</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }",
    "h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }",
    "h2 { color: #34495e; margin-top: 30px; }",
    "h3 { color: #7f8c8d; margin-top: 25px; }",
    "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }",
    "th { background-color: #3498db; color: white; font-weight: bold; }",
    "tr:nth-child(even) { background-color: #f8f9fa; }",
    ".summary-box { background-color: #ecf0f1; padding: 20px; border-radius: 8px; margin: 20px 0; }",
    ".conclusion-pass { color: #27ae60; font-weight: bold; }",
    ".conclusion-fail { color: #e74c3c; font-weight: bold; }",
    ".footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #bdc3c7; color: #7f8c8d; font-size: 12px; }",
    "</style>",
    "</head>",
    "<body>",
    
    # Header
    "<h1>", content$header$title, "</h1>",
    "<p><strong>", content$header$subtitle, "</strong></p>",
    "<p>Generated on: ", content$header$date, " at ", format(content$header$time, "%H:%M:%S"), "</p>",
    "<p>Generated by: ", content$header$generated_by, " v", content$header$version, "</p>",
    
    # Study Information
    "<h2>Study Information</h2>",
    "<div class='summary-box'>",
    "<p><strong>Study Design:</strong> ", content$study$design, "</p>",
    "<p><strong>Significance Level (α):</strong> ", content$study$alpha, "</p>",
    "<p><strong>Bioequivalence Limits:</strong> ", content$study$be_limits$lower, "% - ", content$study$be_limits$upper, "%</p>",
    "<p><strong>Regulatory Standard:</strong> ", content$study$regulatory_standard, "</p>",
    "</div>",
    
    # Summary
    "<h2>Analysis Summary</h2>",
    "<div class='summary-box'>",
    "<p><strong>Number of Subjects:</strong> ", content$summary$n_subjects, "</p>",
    "<p><strong>Parameters Analyzed:</strong> ", content$summary$n_parameters, "</p>",
    "<p><strong>Bioequivalent Parameters:</strong> ", content$summary$n_bioequivalent, "</p>",
    "<p><strong>Overall Conclusion:</strong> <span class='", 
    ifelse(content$summary$overall_conclusion == "BIOEQUIVALENT", "conclusion-pass", "conclusion-fail"),
    "'>", content$summary$overall_conclusion, "</span></p>",
    "</div>",
    
    # Bioequivalence Results Table
    "<h2>Bioequivalence Results</h2>",
    generate_html_table(content$results$bioequivalence_table, "Bioequivalence Analysis Results"),
    
    # Conclusions
    "<h2>Conclusions</h2>",
    paste(lapply(content$conclusions, function(x) paste0("<p>", x, "</p>")), collapse = ""),
    
    # Footer
    "<div class='footer'>",
    "<p>This report was generated automatically by the BioEQ Analysis Platform.</p>",
    "<p>Please verify all results and consult with a biostatistician for regulatory submissions.</p>",
    "</div>",
    
    "</body>",
    "</html>"
  )
  
  # Write HTML content to file
  writeLines(html_content, temp_file)
  
  return(temp_file)
}

#' Generate HTML table from data frame
#' @param data Data frame to convert
#' @param caption Table caption
#' @return HTML table string
generate_html_table <- function(data, caption = "") {
  
  if (is.null(data) || nrow(data) == 0) {
    return("<p>No data available</p>")
  }
  
  # Start table
  html <- paste0("<table><caption><strong>", caption, "</strong></caption><thead><tr>")
  
  # Column headers
  for (col_name in names(data)) {
    html <- paste0(html, "<th>", col_name, "</th>")
  }
  html <- paste0(html, "</tr></thead><tbody>")
  
  # Data rows
  for (i in 1:nrow(data)) {
    html <- paste0(html, "<tr>")
    for (j in 1:ncol(data)) {
      cell_value <- data[i, j]
      # Format numeric values
      if (is.numeric(cell_value)) {
        cell_value <- round(cell_value, 4)
      }
      # Color-code bioequivalence results
      if (names(data)[j] == "Bioequivalent") {
        if (cell_value == "Yes") {
          cell_value <- paste0("<span class='conclusion-pass'>", cell_value, "</span>")
        } else {
          cell_value <- paste0("<span class='conclusion-fail'>", cell_value, "</span>")
        }
      }
      html <- paste0(html, "<td>", cell_value, "</td>")
    }
    html <- paste0(html, "</tr>")
  }
  
  # Close table
  html <- paste0(html, "</tbody></table>")
  
  return(html)
}

#' Generate PDF report (placeholder - requires pandoc/rmarkdown)
#' @param content Structured report content
#' @return Path to PDF file
generate_pdf_report <- function(content) {
  # For now, create a simple text-based PDF report
  temp_file <- tempfile(fileext = ".txt")
  
  # Create text content
  text_content <- c(
    paste("BIOEQUIVALENCE ANALYSIS REPORT"),
    paste("Generated on:", content$header$date),
    "",
    "STUDY INFORMATION:",
    paste("Study Design:", content$study$design),
    paste("Significance Level:", content$study$alpha),
    paste("BE Limits:", content$study$be_limits$lower, "-", content$study$be_limits$upper, "%"),
    "",
    "ANALYSIS SUMMARY:",
    paste("Subjects:", content$summary$n_subjects),
    paste("Parameters:", content$summary$n_parameters),
    paste("Overall Conclusion:", content$summary$overall_conclusion),
    "",
    "CONCLUSIONS:",
    content$conclusions
  )
  
  writeLines(text_content, temp_file)
  return(temp_file)
}

#' Generate Word report (placeholder)
#' @param content Structured report content  
#' @return Path to Word file
generate_word_report <- function(content) {
  # Placeholder - would require officer or similar package
  return(generate_pdf_report(content))
}
