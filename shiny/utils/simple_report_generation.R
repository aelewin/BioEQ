# Simplified Report Generation
# HTML report generation without pandoc dependency

# Generate simple HTML report
generate_simple_html_report <- function(be_results, nca_results, analysis_config, output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- paste0("BioEQ_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  } else if (!grepl("\\.html$", output_file)) {
    output_file <- paste0(output_file, ".html")
  }
  
  # Overall BE status
  be_status <- tryCatch({
    if (!is.null(be_results$be_conclusions) && length(be_results$be_conclusions) > 0) {
      bioequivalent_results <- sapply(be_results$be_conclusions, function(x) {
        if (is.list(x) && "bioequivalent" %in% names(x)) {
          return(x$bioequivalent[1])  # Take first element if vector
        } else {
          return(FALSE)
        }
      })
      if (all(bioequivalent_results)) {
        "BIOEQUIVALENT"
      } else {
        "NOT BIOEQUIVALENT"
      }
    } else {
      "STATUS UNKNOWN"
    }
  }, error = function(e) {
    "STATUS ERROR"
  })
  
  # Create HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Bioequivalence Analysis Report</title>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            margin: 40px; 
            line-height: 1.6;
            color: #333;
        }
        .header { 
            background: #2c3e50; 
            color: white; 
            padding: 20px; 
            margin: -40px -40px 30px -40px;
        }
        .status-pass { 
            background: #27ae60; 
            color: white; 
            padding: 8px 16px; 
            border-radius: 4px; 
            display: inline-block;
        }
        .status-fail { 
            background: #e74c3c; 
            color: white; 
            padding: 8px 16px; 
            border-radius: 4px; 
            display: inline-block;
        }
        table { 
            border-collapse: collapse; 
            width: 100%; 
            margin: 20px 0;
        }
        th, td { 
            border: 1px solid #ddd; 
            padding: 12px; 
            text-align: left;
        }
        th { 
            background: #f8f9fa; 
            font-weight: bold;
        }
        .section { 
            margin: 30px 0; 
            border-left: 4px solid #3498db;
            padding-left: 20px;
        }
        .summary-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }
        .summary-card {
            background: #f8f9fa;
            padding: 15px;
            border-radius: 8px;
            border-left: 4px solid #3498db;
        }
        .metric-value {
            font-size: 24px;
            font-weight: bold;
            color: #2c3e50;
        }
        .metric-label {
            font-size: 14px;
            color: #7f8c8d;
            margin-bottom: 5px;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Bioequivalence Analysis Report</h1>
        <p>Generated on ', format(Sys.time(), "%B %d, %Y at %H:%M"), '</p>
    </div>

    <div class="section">
        <h2>Executive Summary</h2>
        <p><strong>Bioequivalence Status:</strong> 
           <span class="', ifelse(be_status == "BIOEQUIVALENT", "status-pass", "status-fail"), '">', 
           be_status, '</span></p>
        
        <div class="summary-grid">
            <div class="summary-card">
                <div class="metric-label">Study Design</div>
                <div class="metric-value">', be_results$design %||% "Unknown", '</div>
            </div>
            <div class="summary-card">
                <div class="metric-label">Number of Subjects</div>
                <div class="metric-value">', be_results$n_subjects %||% "N/A", '</div>
            </div>
            <div class="summary-card">
                <div class="metric-label">Parameters Analyzed</div>
                <div class="metric-value">', length(be_results$parameters %||% character(0)), '</div>
            </div>
            <div class="summary-card">
                <div class="metric-label">Regulatory Standard</div>
                <div class="metric-value">', analysis_config$regulatory_standard %||% "FDA", '</div>
            </div>
        </div>
    </div>
')
  
  # Add PK parameters table
  if (!is.null(nca_results$pk_summary)) {
    pk_table <- nca_results$pk_summary
    
    html_content <- paste0(html_content, '
    <div class="section">
        <h2>Pharmacokinetic Parameters Summary</h2>
        <table>
            <thead>
                <tr>')
    
    for (col in names(pk_table)) {
      html_content <- paste0(html_content, '<th>', col, '</th>')
    }
    
    html_content <- paste0(html_content, '
                </tr>
            </thead>
            <tbody>')
    
    for (i in 1:nrow(pk_table)) {
      html_content <- paste0(html_content, '<tr>')
      for (j in 1:ncol(pk_table)) {
        value <- pk_table[i, j]
        if (is.numeric(value)) {
          value <- round(value, 3)
        }
        html_content <- paste0(html_content, '<td>', value, '</td>')
      }
      html_content <- paste0(html_content, '</tr>')
    }
    
    html_content <- paste0(html_content, '
            </tbody>
        </table>
    </div>')
  }
  
  # Add bioequivalence results
  if (!is.null(be_results$bioequivalence)) {
    be_table <- be_results$bioequivalence
    
    html_content <- paste0(html_content, '
    <div class="section">
        <h2>Bioequivalence Assessment</h2>
        <table>
            <thead>
                <tr>')
    
    for (col in names(be_table)) {
      html_content <- paste0(html_content, '<th>', col, '</th>')
    }
    
    html_content <- paste0(html_content, '
                </tr>
            </thead>
            <tbody>')
    
    for (i in 1:nrow(be_table)) {
      html_content <- paste0(html_content, '<tr>')
      for (j in 1:ncol(be_table)) {
        value <- be_table[i, j]
        if (is.numeric(value)) {
          value <- round(value, 3)
        }
        
        # Add styling for bioequivalent column
        cell_class <- ""
        if (names(be_table)[j] == "Bioequivalent") {
          # Handle both logical and character values
          is_be <- FALSE
          if (is.logical(value)) {
            is_be <- value[1]
          } else if (is.character(value)) {
            is_be <- value[1] %in% c("Yes", "TRUE", "Pass")
          }
          
          cell_class <- ifelse(is_be, 
                              ' style="background: #d5f4e6; font-weight: bold;"',
                              ' style="background: #fadbd8; font-weight: bold;"')
        }
        
        html_content <- paste0(html_content, '<td', cell_class, '>', value, '</td>')
      }
      html_content <- paste0(html_content, '</tr>')
    }
    
    html_content <- paste0(html_content, '
            </tbody>
        </table>
    </div>')
  }
  
  # Add ANOVA results
  if (!is.null(be_results$anova_results)) {
    html_content <- paste0(html_content, '
    <div class="section">
        <h2>Statistical Analysis (ANOVA)</h2>')
    
    for (param in names(be_results$anova_results)) {
      anova_data <- be_results$anova_results[[param]]
      
      if (is.data.frame(anova_data)) {
        html_content <- paste0(html_content, '
        <h3>', param, '</h3>
        <table>
            <thead>
                <tr>')
        
        for (col in names(anova_data)) {
          html_content <- paste0(html_content, '<th>', col, '</th>')
        }
        
        html_content <- paste0(html_content, '
                </tr>
            </thead>
            <tbody>')
        
        for (i in 1:nrow(anova_data)) {
          html_content <- paste0(html_content, '<tr>')
          for (j in 1:ncol(anova_data)) {
            value <- anova_data[i, j]
            if (is.numeric(value)) {
              value <- round(value, 4)
            }
            html_content <- paste0(html_content, '<td>', value, '</td>')
          }
          html_content <- paste0(html_content, '</tr>')
        }
        
        html_content <- paste0(html_content, '
            </tbody>
        </table>')
      }
    }
    
    html_content <- paste0(html_content, '
    </div>')
  }
  
  # Add conclusions
  html_content <- paste0(html_content, '
    <div class="section">
        <h2>Conclusions</h2>
        <p><strong>Overall Assessment:</strong> The test and reference formulations are 
        <strong>', be_status, '</strong> based on the following criteria:</p>
        <ul>')
  
  if (!is.null(be_results$be_conclusions)) {
    for (param in names(be_results$be_conclusions)) {
      conclusion <- be_results$be_conclusions[[param]]
      
      # Handle different conclusion formats
      if (is.list(conclusion) && "bioequivalent" %in% names(conclusion)) {
        is_bioequivalent <- conclusion$bioequivalent[1]  # Take first element
      } else {
        is_bioequivalent <- FALSE
      }
      
      status <- ifelse(is_bioequivalent, "PASSED", "FAILED")
      html_content <- paste0(html_content, 
                            '<li><strong>', param, ':</strong> ', status, ' bioequivalence criteria</li>')
    }
  }
  
  html_content <- paste0(html_content, '
        </ul>
        
        <h3>Regulatory Compliance</h3>
        <p>This analysis was conducted in accordance with ', analysis_config$regulatory_standard %||% "FDA", 
        ' regulatory guidelines.</p>
        
        <h3>Statistical Model</h3>
        <p>The analysis employed a ', be_results$design %||% "crossover", ' design ANOVA model with appropriate 
        fixed and random effects.</p>
    </div>
    
    <div class="section">
        <h2>Technical Information</h2>
        <ul>
            <li><strong>Analysis Platform:</strong> BioEQ vBETA</li>
            <li><strong>R Version:</strong> ', R.version.string, '</li>
            <li><strong>Report Generated:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</li>
        </ul>
    </div>

</body>
</html>')
  
  # Write to file
  writeLines(html_content, output_file)
  
  return(list(
    success = TRUE,
    report_path = output_file,
    message = paste("HTML report generated successfully:", output_file)
  ))
}

# Generate CSV summary report
generate_csv_summary <- function(be_results, nca_results, analysis_config, output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- paste0("BioEQ_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  } else if (!grepl("\\.csv$", output_file)) {
    output_file <- paste0(output_file, ".csv")
  }
  
  # Create summary data frame
  summary_data <- data.frame(
    Metric = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # Add general information
  summary_data <- rbind(summary_data, data.frame(
    Metric = c("Study Design", "Number of Subjects", "Regulatory Standard", 
               "Analysis Date", "Bioequivalence Status"),
    Value = c(
      be_results$design %||% "Unknown",
      as.character(be_results$n_subjects %||% "N/A"),
      analysis_config$regulatory_standard %||% "FDA",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tryCatch({
        if (!is.null(be_results$be_conclusions) && length(be_results$be_conclusions) > 0) {
          bioequivalent_results <- sapply(be_results$be_conclusions, function(x) {
            if (is.list(x) && "bioequivalent" %in% names(x)) {
              return(x$bioequivalent[1])
            } else {
              return(FALSE)
            }
          })
          ifelse(all(bioequivalent_results), "BIOEQUIVALENT", "NOT BIOEQUIVALENT")
        } else {
          "STATUS UNKNOWN"
        }
      }, error = function(e) "STATUS ERROR")
    )
  ))
  
  # Add parameter-specific results
  if (!is.null(be_results$be_conclusions)) {
    for (param in names(be_results$be_conclusions)) {
      conclusion <- be_results$be_conclusions[[param]]
      
      # Handle bioequivalent status
      if (is.list(conclusion) && "bioequivalent" %in% names(conclusion)) {
        is_bioequivalent <- conclusion$bioequivalent[1]
      } else {
        is_bioequivalent <- FALSE
      }
      
      summary_data <- rbind(summary_data, data.frame(
        Metric = paste(param, "Bioequivalent"),
        Value = ifelse(is_bioequivalent, "Yes", "No")
      ))
      
      # Handle geometric mean ratio
      if (is.list(conclusion) && "geometric_mean_ratio" %in% names(conclusion)) {
        if (!is.null(conclusion$geometric_mean_ratio)) {
          summary_data <- rbind(summary_data, data.frame(
            Metric = paste(param, "Point Estimate (%)"),
            Value = round(conclusion$geometric_mean_ratio * 100, 2)
          ))
        }
      }
      
      # Handle confidence intervals
      if (is.list(conclusion) && "ci_lower" %in% names(conclusion) && "ci_upper" %in% names(conclusion)) {
        if (!is.null(conclusion$ci_lower) && !is.null(conclusion$ci_upper)) {
          summary_data <- rbind(summary_data, data.frame(
            Metric = paste(param, "90% CI"),
            Value = paste0(round(conclusion$ci_lower * 100, 1), " - ", 
                          round(conclusion$ci_upper * 100, 1), "%")
          ))
        }
      }
    }
  }
  
  # Write to file
  write.csv(summary_data, output_file, row.names = FALSE)
  
  return(list(
    success = TRUE,
    report_path = output_file,
    message = paste("CSV summary generated successfully:", output_file)
  ))
}
