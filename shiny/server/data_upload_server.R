# Data Upload Server Logic - Enhanced with Comprehensive Validation
# This file handles data upload, validation, and template generation

# Load required libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  message("readxl not available - Excel file support may be limited")
}

# Load required libraries and source validation functions
if (!exists("validate_be_data")) {
  source("../R/utils.R", local = TRUE)
}

# Enhanced data validation function that handles common column name variations
validate_bioeq_data_enhanced <- function(data) {
  errors <- c()
  warnings <- c()
  suggestions <- c()
  
  # Create a copy for processing
  original_data <- data
  processed_data <- data
  
  # =============================================================================
  # COLUMN NAME MAPPING AND STANDARDIZATION
  # =============================================================================
  
  # Define expected column mappings (case insensitive)
  column_mappings <- list(
    subject = c("subject", "subj", "id", "subjid", "subject_id", "patientid", "patient", "vol", "volunteer"),
    treatment = c("treatment", "tmt", "trt", "formulation", "form", "drug", "product", "regimen"),
    period = c("period", "per", "phase", "visit"),
    sequence = c("sequence", "seq", "period_sequence", "grp", "group"),
    time = c("time", "timepoint", "hour", "hours", "hr", "sampling_time"),
    concentration = c("concentration", "conc", "result", "value", "plasma_conc", "serum_conc")
  )
  
  # Additional optional columns
  optional_mappings <- list(
    dose = c("dose", "dosage", "dose_amount"),
    weight = c("weight", "bw", "body_weight"),
    age = c("age", "subject_age"),
    sex = c("sex", "gender", "subject_sex"),
    race = c("race", "ethnicity"),
    height = c("height", "ht", "subject_height"),
    bmi = c("bmi", "body_mass_index")
  )
  
  # Function to find column by possible names
  find_column <- function(data, possible_names) {
    col_names <- tolower(names(data))
    for (name in tolower(possible_names)) {
      idx <- which(col_names == name)
      if (length(idx) > 0) {
        return(names(data)[idx[1]])
      }
    }
    # Try partial matching if exact match not found
    for (name in tolower(possible_names)) {
      idx <- grep(name, col_names, fixed = TRUE)
      if (length(idx) > 0) {
        return(names(data)[idx[1]])
      }
    }
    return(NULL)
  }
  
  # Map required columns to standard names
  mapped_columns <- list()
  for (std_name in names(column_mappings)) {
    col <- find_column(data, column_mappings[[std_name]])
    if (!is.null(col)) {
      mapped_columns[[std_name]] <- col
      if (col != std_name) {
        names(processed_data)[names(processed_data) == col] <- std_name
      }
    }
  }
  
  # Map optional columns too
  for (std_name in names(optional_mappings)) {
    col <- find_column(data, optional_mappings[[std_name]])
    if (!is.null(col)) {
      mapped_columns[[std_name]] <- col
      if (col != std_name) {
        names(processed_data)[names(processed_data) == col] <- std_name
      }
    }
  }
  
  # Check for required columns
  required <- c("subject", "treatment")
  missing_required <- setdiff(required, names(mapped_columns))
  
  if (length(missing_required) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_required, collapse = ", ")))
  }
  
  # =============================================================================
  # RETURN VALIDATION RESULTS
  # =============================================================================
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    suggestions = suggestions,
    data = processed_data,
    processed_data = processed_data,
    original_data = original_data,
    mapped_columns = mapped_columns,
    summary = if(length(errors) == 0) create_data_summary_enhanced(processed_data) else NULL,
    data_type = "concentration"
  ))
}

# Enhanced data summary function
create_data_summary_enhanced <- function(data) {
  if (is.null(data)) return(NULL)
  
  tryCatch({
    # Basic counts
    n_subjects <- length(unique(data$subject))
    n_treatments <- length(unique(data$treatment))
    treatments <- sort(unique(data$treatment))
    total_obs <- nrow(data)
    
    # Check if this is concentration-time data or PK parameters data
    has_time <- "time" %in% names(data)
    has_concentration <- "concentration" %in% names(data)
    
    if (has_time && has_concentration) {
      # Concentration-time data
      n_timepoints <- length(unique(data$time))
      timepoints <- sort(unique(data$time))
      
      # Design analysis with error handling
      treatments_per_subject <- tryCatch({
        data %>%
          group_by(subject) %>%
          summarise(n_treatments = length(unique(treatment[!is.na(treatment)])), .groups = "drop")
      }, error = function(e) {
        data.frame(subject = unique(data$subject[!is.na(data$subject)]), n_treatments = 1)
      })
      
      is_crossover <- tryCatch({
        if (nrow(treatments_per_subject) > 0 && !any(is.na(treatments_per_subject$n_treatments))) {
          all(treatments_per_subject$n_treatments > 1)
        } else {
          FALSE
        }
      }, error = function(e) FALSE)
      design_type <- if (is_crossover) "Crossover" else "Parallel"
      
      # Missing data analysis
      missing_conc <- sum(is.na(data$concentration))
      missing_pct <- round(missing_conc / total_obs * 100, 2)
      
      # Concentration range with safe handling
      conc_range <- tryCatch({
        valid_conc <- data$concentration[is.finite(data$concentration)]
        if(length(valid_conc) > 0) {
          range(valid_conc, na.rm = TRUE)
        } else {
          c(NA, NA)
        }
      }, error = function(e) c(NA, NA))
      
      time_range <- tryCatch({
        valid_time <- data$time[is.finite(data$time)]
        if(length(valid_time) > 0) {
          range(valid_time, na.rm = TRUE)
        } else {
          c(NA, NA)
        }
      }, error = function(e) c(NA, NA))
      
      return(list(
        n_subjects = n_subjects,
        n_treatments = n_treatments,
        treatments = treatments,
        n_timepoints = n_timepoints,
        timepoints = timepoints,
        total_observations = total_obs,
        design_type = design_type,
        missing_concentrations = missing_conc,
        missing_percentage = missing_pct,
        concentration_range = conc_range,
        time_range = time_range,
        data_type = "concentration"
      ))
      
    } else {
      # PK parameters data
      # Get numeric columns (potential PK parameters)
      numeric_cols <- sapply(data, is.numeric)
      pk_cols <- names(data)[numeric_cols & !names(data) %in% c("subject", "sequence", "period", "dose", "weight", "age")]
      
      # Design analysis with error handling
      treatments_per_subject <- tryCatch({
        data %>%
          group_by(subject) %>%
          summarise(n_treatments = length(unique(treatment)), .groups = "drop")
      }, error = function(e) {
        data.frame(subject = unique(data$subject), n_treatments = 1)
      })
      
      is_crossover <- tryCatch({
        all(treatments_per_subject$n_treatments > 1)
      }, error = function(e) FALSE)
      
      design_type <- if (is_crossover) "Crossover" else "Parallel"
      
      # Missing data analysis for PK parameters
      if (length(pk_cols) > 0) {
        missing_pk <- tryCatch({
          sum(is.na(data[pk_cols]))
        }, error = function(e) 0)
        total_pk_values <- nrow(data) * length(pk_cols)
        missing_pct <- tryCatch({
          round(missing_pk / total_pk_values * 100, 2)
        }, error = function(e) 0)
      } else {
        missing_pk <- 0
        missing_pct <- 0
      }
      
      return(list(
        n_subjects = n_subjects,
        n_treatments = n_treatments,
        treatments = treatments,
        n_pk_parameters = length(pk_cols),
        pk_parameters = pk_cols,
        total_observations = total_obs,
        design_type = design_type,
        missing_pk_values = missing_pk,
        missing_percentage = missing_pct,
        data_type = "pk_parameters"
      ))
    }
  }, error = function(e) {
    # Return a safe default summary if anything goes wrong
    return(list(
      n_subjects = length(unique(data$subject)),
      n_treatments = length(unique(data$treatment)),
      treatments = unique(data$treatment),
      total_observations = nrow(data),
      design_type = "Unknown",
      data_type = "unknown",
      error_message = paste("Error creating summary:", e$message)
    ))
  })
}

# =============================================================================
# FILE UPLOAD HANDLER WITH ENHANCED VALIDATION
# =============================================================================

observeEvent(input$data_file, {
  req(input$data_file, input$data_type)
  
  file_ext <- tools::file_ext(input$data_file$datapath)
  file_size_mb <- round(file.size(input$data_file$datapath) / (1024^2), 2)
  
  # Check file size (50MB limit)
  if (file_size_mb > 50) {
    showNotification(paste("File too large:", file_size_mb, "MB. Maximum size is 50MB."), 
                    type = "error", duration = 5)
    return()
  }
  
  withProgress(message = 'Processing file...', value = 0, {
    
    tryCatch({
      incProgress(0.2, detail = paste("Reading", toupper(file_ext), "file..."))
      
      # Read the uploaded file with enhanced error handling
      uploaded_data <- NULL
      
      if (file_ext == "csv") {
        # Try different CSV reading approaches
        uploaded_data <- tryCatch({
          read_csv(input$data_file$datapath, show_col_types = FALSE)
        }, error = function(e) {
          # Try with different encoding
          tryCatch({
            read_csv(input$data_file$datapath, locale = locale(encoding = "latin1"), show_col_types = FALSE)
          }, error = function(e2) {
            # Try with semicolon separator
            read_csv2(input$data_file$datapath, show_col_types = FALSE)
          })
        })
      } else if (file_ext %in% c("xlsx", "xls")) {
        uploaded_data <- read_excel(input$data_file$datapath, .name_repair = "minimal")
      } else {
        showNotification("Unsupported file format. Please upload CSV or Excel files.", type = "error")
        return()
      }
      
      incProgress(0.3, detail = "Validating data structure...")
      
      # Basic data checks
      if (is.null(uploaded_data) || nrow(uploaded_data) == 0) {
        showNotification("File appears to be empty or corrupted", type = "error")
        return()
      }
      
      if (ncol(uploaded_data) < 2) {
        showNotification("File must contain at least 2 columns", type = "error")
        return()
      }
      
      # Store original data
      values$uploaded_data_original <- uploaded_data
      values$columns_mapped <- FALSE
      
      incProgress(0.3, detail = "Running comprehensive validation...")
      
      # Run appropriate validation based on data type
      if (input$data_type == "concentration") {
        validation_result <- validate_bioeq_data_enhanced(uploaded_data)
      } else if (input$data_type == "pk_parameters") {
        validation_result <- validate_pk_data_enhanced(uploaded_data)
      }
      
      # Store data type in validation result
      validation_result$data_type <- input$data_type
      values$validation_result <- validation_result
      
      incProgress(0.2, detail = "Finalizing...")
      
      # Store processed data if validation passed
      if (validation_result$valid) {
        values$uploaded_data <- validation_result$processed_data
        values$data_summary <- validation_result
        values$data_type <- input$data_type
        # Don't set columns_mapped to true here - require explicit mapping confirmation
        
        # Update progress indicator
        if (exists("shinyjs_available") && shinyjs_available) {
          shinyjs::runjs("
            $('.progress-step').removeClass('active');
            $('.progress-step:first-child').addClass('completed');
            $('.progress-step:nth-child(2)').addClass('active');
          ")
        }
        
        # Different success messages based on data type - use processed data
        processed_data <- validation_result$processed_data
        if (input$data_type == "concentration") {
          showNotification(
            paste("✅ Concentration data uploaded successfully!", 
                  length(unique(processed_data$subject)), "subjects,", 
                  nrow(processed_data), "observations"), 
            type = "message", duration = 5
          )
        } else {
          pk_params <- names(validation_result$mapped_pk_parameters)
          showNotification(
            paste("✅ PK parameter data uploaded successfully!", 
                  length(unique(processed_data$subject)), "subjects,", 
                  length(pk_params), "PK parameters"), 
            type = "message", duration = 5
          )
        }
      } else {
        values$uploaded_data <- NULL
        values$data_summary <- NULL
        
        error_count <- length(validation_result$errors)
        warning_count <- length(validation_result$warnings)
        
        showNotification(
          paste("⚠️ Data validation issues:", error_count, "errors,", warning_count, "warnings"), 
          type = "warning", duration = 8
        )
      }
      
    }, error = function(e) {
      values$uploaded_data <- NULL
      values$validation_result <- NULL
      showNotification(paste("❌ Error processing file:", e$message), type = "error", duration = 10)
    })
  })
})



# Reset button observer
observeEvent(input$reset_upload, {
  # Clear all data and reset state
  values$uploaded_data <- NULL
  values$uploaded_data_original <- NULL
  values$validation_result <- NULL
  values$data_summary <- NULL
  
  # Reset file input (this requires shinyjs)
  if (exists("shinyjs_available") && shinyjs_available) {
    shinyjs::reset("data_file")
  }
  
  showNotification("Upload reset", type = "message", duration = 3)
})

# =============================================================================
# REACTIVE OUTPUTS FOR UI DISPLAY
# =============================================================================

# Upload status indicator
output$upload_status <- reactive({
  !is.null(values$uploaded_data) && isTRUE(values$columns_mapped)
})
outputOptions(output, "upload_status", suspendWhenHidden = FALSE)

# Step 4 visibility - only show after columns are mapped and confirmed
output$step4_ready <- reactive({
  # Check that data is uploaded, columns are mapped, and validation result exists
  !is.null(values$uploaded_data) && 
  isTRUE(values$columns_mapped) && 
  !is.null(values$validation_result) &&
  !is.null(values$data_summary)
})
outputOptions(output, "step4_ready", suspendWhenHidden = FALSE)

# Data preview availability
output$data_preview_available <- reactive({
  !is.null(values$uploaded_data)
})
outputOptions(output, "data_preview_available", suspendWhenHidden = FALSE)

# Validation summary removed - keeping only data summary
# The validation_summary output has been removed to streamline the interface
# Data validation still occurs but the detailed output is no longer displayed

# Column mappings display for left column
output$column_mappings_display <- renderText({
  req(values$uploaded_data)
  
  data_type <- values$data_type %||% "concentration"
  validation <- values$validation_result
  
  # Create the mappings display
  mappings_text <- "🔗 COLUMN MAPPINGS\n"
  mappings_text <- paste0(mappings_text, paste(rep("─", 39), collapse = ""), "\n")
  
  if (data_type == "concentration") {
    # Standard concentration-time mappings
    mappings <- c(
      "Subject" = "Subject",
      "Sequence" = "Sequence", 
      "Period" = "Period",
      "Treatment" = "Treatment",
      "Time" = "Time [hours]",
      "Concentration" = "Concentration [ng/mL]"
    )
  } else {
    # PK parameter mappings - get from validation if available
    if (!is.null(validation$mapped_pk_parameters)) {
      pk_params <- names(validation$mapped_pk_parameters)
      mappings <- c(
        "Subject" = "Subject",
        "Treatment" = "Treatment"
      )
      
      # Add detected PK parameters
      for (param in pk_params) {
        unit <- switch(param,
          "AUC0t" = "[ng·h/mL]",
          "AUC0inf" = "[ng·h/mL]", 
          "Cmax" = "[ng/mL]",
          "Tmax" = "[hours]",
          "half_life" = "[hours]",
          "clearance" = "[L/h]",
          "volume" = "[L]",
          ""
        )
        mappings[[param]] <- paste0(param, " ", unit)
      }
    } else {
      mappings <- c(
        "Subject" = "Subject",
        "Treatment" = "Treatment",
        "AUC0t" = "AUC0t [ng·h/mL]",
        "Cmax" = "Cmax [ng/mL]"
      )
    }
  }
  
  # Add each mapping
  for (col_name in names(mappings)) {
    mappings_text <- paste0(mappings_text, "• ", col_name, ": ", mappings[[col_name]], "\n")
  }
  
  mappings_text <- paste0(mappings_text, paste(rep("─", 39), collapse = ""), "\n")
  
  # Add file processing info
  file_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  mappings_text <- paste0(mappings_text, "File processed: ", file_time, "\n")
  mappings_text <- paste0(mappings_text, "Status: ✅ Ready for analysis")
  
  return(mappings_text)
})

# Enhanced data preview with better formatting for both data types
output$data_preview <- DT::renderDataTable({
  req(values$uploaded_data)
  
  data <- values$uploaded_data
  data_type <- values$data_type %||% "concentration"
  
  # Format numeric columns for better display based on data type
  if (data_type == "concentration") {
    if ("concentration" %in% names(data)) {
      data$concentration <- round(data$concentration, 3)
    }
    if ("time" %in% names(data)) {
      data$time <- round(data$time, 2)
    }
  } else {
    # For PK parameters, format all numeric PK columns
    pk_columns <- c("AUC0t", "AUC0inf", "Cmax", "Tmax", "half_life", "clearance", "volume", "AUC_extrap_percent")
    for (col in pk_columns) {
      if (col %in% names(data) && is.numeric(data[[col]])) {
        if (col == "Tmax") {
          data[[col]] <- round(data[[col]], 2)  # Time values
        } else if (col == "AUC_extrap_percent") {
          data[[col]] <- round(data[[col]], 1)  # Percentages
        } else {
          data[[col]] <- round(data[[col]], 3)  # Other PK parameters
        }
      }
    }
  }
  
  DT::datatable(
    data,
    options = list(
      scrollX = TRUE,
      scrollY = "400px",
      pageLength = 15,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = 'copy', text = '<i class="fa fa-copy"></i> Copy'),
        list(extend = 'csv', text = '<i class="fa fa-file-csv"></i> CSV'),
        list(extend = 'excel', text = '<i class="fa fa-file-excel"></i> Excel')
      ),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all"),
        list(className = 'dt-nowrap', targets = "_all")
      ),
      language = list(
        emptyTable = "No data available",
        info = "Showing _START_ to _END_ of _TOTAL_ observations",
        infoEmpty = "No observations to show",
        lengthMenu = "Show _MENU_ observations per page"
      )
    ),
    class = "display nowrap table-striped table-hover",
    rownames = FALSE,
    escape = FALSE
  ) %>%
  DT::formatStyle(
    columns = names(data),
    backgroundColor = "rgba(255, 255, 255, 0.9)"
  )
})

# Enhanced data summary with comprehensive statistics
output$data_summary <- renderText({
  req(values$data_summary)
  
  summary <- values$data_summary
  data_type <- summary$data_type %||% "concentration"
  
  if (data_type == "concentration") {
    # Concentration-Time data summary (mappings now in separate column)
    paste0(
      "📊 DATA SUMMARY\n",
      "═══════════════════════════════════════\n",
      "Data Type: Concentration-Time\n",
      "Study Design: ", summary$design_type, "\n",
      "Subjects: ", summary$n_subjects, "\n",
      "Treatments: ", paste(summary$treatments, collapse = ", "), " (", summary$n_treatments, " total)\n",
      "Time Points: ", summary$n_timepoints, " (", 
      if(!is.na(summary$time_range[1]) && !is.na(summary$time_range[2])) {
        paste0(round(summary$time_range[1], 2), " - ", round(summary$time_range[2], 2), " hours")
      } else "Range not available", ")\n",
      "Total Observations: ", summary$total_observations, "\n",
      "Missing Concentrations: ", summary$missing_concentrations, " (", summary$missing_percentage, "%)\n",
      "Concentration Range: ", 
      if(!is.na(summary$concentration_range[1]) && !is.na(summary$concentration_range[2])) {
        paste0(round(summary$concentration_range[1], 3), " - ", round(summary$concentration_range[2], 3))
      } else "Range not available", "\n"
    )
  } else {
    # PK Parameters data summary (mappings now in separate column)
    paste0(
      "📊 DATA SUMMARY\n",
      "═══════════════════════════════════════\n",
      "Data Type: PK Parameters\n",
      "Study Design: ", summary$design_type, "\n",
      "Subjects: ", summary$n_subjects, "\n",
      "Treatments: ", paste(summary$treatments, collapse = ", "), " (", summary$n_treatments, " total)\n",
      "PK Parameters: ", summary$n_pk_parameters, "\n",
      if(isTRUE(length(summary$pk_parameters) > 0)) paste0("Parameters: ", paste(summary$pk_parameters, collapse = ", "), "\n") else "",
      "Total Observations: ", summary$total_observations, "\n",
      "Missing PK Values: ", summary$missing_pk_values, " (", summary$missing_percentage, "%)\n"
    )
  }
})

# Template download handlers
output$download_example_data <- downloadHandler(
  filename = function() {
    paste0("validation_data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # First try to use the full example data we created
    example_file_full <- "inst/templates/example_data_full.csv"
    example_file_validation <- "../data/user_validation_data.csv"
    
    if (file.exists(example_file_full)) {
      file.copy(example_file_full, file)
    } else if (file.exists(example_file_validation)) {
      file.copy(example_file_validation, file)
    } else {
      # Create comprehensive example data if files don't exist
      example_data <- data.frame(
        Subject = rep(1:4, each = 18),
        Treatment = rep(rep(c("R", "T"), each = 9), 4),
        Time = rep(c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 24), 8),
        Concentration = c(
          # Subject 1 - Reference
          0, 8.12, 15.43, 22.17, 18.95, 12.84, 7.23, 4.51, 1.02,
          # Subject 1 - Test  
          0, 7.89, 14.76, 21.34, 17.82, 11.97, 6.85, 4.12, 0.89,
          # Subject 2 - Test (RT sequence)
          0, 9.24, 16.78, 24.12, 20.45, 13.89, 8.12, 5.23, 1.34,
          # Subject 2 - Reference
          0, 8.67, 15.89, 23.45, 19.78, 13.21, 7.56, 4.89, 1.12,
          # Subject 3 - Reference
          0, 7.45, 14.23, 20.67, 17.34, 11.78, 6.89, 4.23, 0.95,
          # Subject 3 - Test
          0, 8.01, 15.12, 21.89, 18.45, 12.34, 7.12, 4.67, 1.08,
          # Subject 4 - Test (RT sequence)
          0, 8.78, 16.23, 23.67, 19.89, 13.45, 7.89, 5.01, 1.23,
          # Subject 4 - Reference
          0, 8.34, 15.67, 22.89, 19.23, 12.98, 7.45, 4.78, 1.09
        ),
        Sequence = rep(c("TR", "TR", "RT", "RT"), each = 18),
        Period = rep(c(1, 2, 1, 2), each = 9, times = 2)
      )
      write_csv(example_data, file)
    }
  }
)

output$download_template <- downloadHandler(
  filename = function() {
    paste0("bioeq_template_", Sys.Date(), ".csv")
  },
  content = function(file) {
    template_file <- "inst/templates/bioequivalence_data_template.csv"
    if (file.exists(template_file)) {
      file.copy(template_file, file)
    } else {
      # Fallback template with proper structure
      template_data <- data.frame(
        Subject = rep(1:2, each = 18),
        Treatment = rep(rep(c("R", "T"), each = 9), 2),
        Time = rep(c(0, 0.5, 1, 2, 4, 6, 8, 12, 24), 4),
        Concentration = c(
          # Subject 1 - Reference
          0, 12.5, 18.2, 15.8, 10.1, 7.2, 4.9, 2.8, 0.3,
          # Subject 1 - Test  
          0, 11.8, 17.5, 16.2, 9.8, 6.9, 4.7, 2.6, 0.2,
          # Subject 2 - Reference
          0, 12.1, 17.8, 15.2, 9.9, 7.1, 4.8, 2.7, 0.3,
          # Subject 2 - Test
          0, 13.2, 19.1, 16.8, 11.2, 8.1, 5.3, 3.1, 0.4
        ),
        Sequence = rep(c("TR", "RT"), each = 18),
        Period = rep(c(1, 2), each = 9, times = 2)
      )
      write_csv(template_data, file)
    }
  }
)

output$download_excel_template <- downloadHandler(
  filename = function() {
    paste0("bioeq_template_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    template_file <- "inst/templates/bioequivalence_data_template.csv"
    if (file.exists(template_file)) {
      template_data <- read_csv(template_file, show_col_types = FALSE)
    } else {
      # Fallback template with proper structure
      template_data <- data.frame(
        Subject = rep(1:2, each = 18),
        Treatment = rep(rep(c("R", "T"), each = 9), 2),
        Time = rep(c(0, 0.5, 1, 2, 4, 6, 8, 12, 24), 4),
        Concentration = c(
          # Subject 1 - Reference
          0, 12.5, 18.2, 15.8, 10.1, 7.2, 4.9, 2.8, 0.3,
          # Subject 1 - Test  
          0, 11.8, 17.5, 16.2, 9.8, 6.9, 4.7, 2.6, 0.2,
          # Subject 2 - Reference
          0, 12.1, 17.8, 15.2, 9.9, 7.1, 4.8, 2.7, 0.3,
          # Subject 2 - Test
          0, 13.2, 19.1, 16.8, 11.2, 8.1, 5.3, 3.1, 0.4
        ),
        Sequence = rep(c("TR", "RT"), each = 18),
        Period = rep(c(1, 2), each = 9, times = 2)
      )
    }
    writexl::write_xlsx(template_data, file)
  }
)

# PK parameters template download
output$download_pk_template <- downloadHandler(
  filename = function() {
    paste0("pk_parameters_template_", Sys.Date(), ".csv")
  },
  content = function(file) {
    pk_template_file <- "inst/templates/pk_parameters_template.csv"
    if (file.exists(pk_template_file)) {
      file.copy(pk_template_file, file)
    } else {
      # Fallback PK parameters template
      pk_template_data <- data.frame(
        Subject = rep(1:6, each = 2),
        Period = rep(1:2, times = 6),
        Formulation = rep(c("Reference", "Test"), times = 6),
        AUC0t = c(245.6, 235.8, 251.4, 248.2, 239.7, 228.5, 
                  256.3, 251.8, 242.1, 237.4, 248.9, 244.3),
        AUC0inf = c(251.2, 241.5, 257.8, 254.1, 245.3, 234.2,
                   262.7, 258.1, 247.9, 243.1, 254.6, 250.2),
        Cmax = c(18.2, 17.5, 19.1, 17.8, 16.9, 16.1,
                20.2, 18.7, 17.3, 16.8, 18.5, 17.9),
        Tmax = rep(1.0, 12)
      )
      write_csv(pk_template_data, file)
    }
  }
)

# Enhanced validation function for PK parameter data
validate_pk_data_enhanced <- function(data) {
  errors <- c()
  warnings <- c()
  suggestions <- c()
  
  # Create a copy for processing
  original_data <- data
  processed_data <- data
  
  # =============================================================================
  # COLUMN NAME MAPPING FOR PK DATA
  # =============================================================================
  
  # Define expected column mappings for PK data (case insensitive)
  column_mappings <- list(
    # Subject mappings
    subject = c("subject", "subj", "id", "subjid", "subject_id", "patientid", "patient", "vol"),
    # Treatment mappings  
    treatment = c("treatment", "tmt", "trt", "formulation", "form", "drug", "product")
  )
  
  # PK parameter mappings
  pk_parameter_mappings <- list(
    AUC0t = c("auc0t", "auc_0_t", "auc0-t", "auct", "auc_t", "auc_last", "auclast"),
    AUC0inf = c("auc0inf", "auc_0_inf", "auc0-inf", "aucinf", "auc_inf", "auc_infinity", "aucinfinity"),
    Cmax = c("cmax", "c_max", "peak", "maximum", "max_conc"),
    Tmax = c("tmax", "t_max", "time_max", "peak_time", "tpeak"),
    half_life = c("half_life", "t_half", "thalf", "elimination_half_life", "t12", "t1/2"),
    clearance = c("clearance", "cl", "clf", "cl_f", "total_clearance"),
    volume = c("volume", "vd", "vdf", "vd_f", "volume_distribution", "apparent_volume"),
    AUC_extrap_percent = c("auc_extrap_percent", "auc_extrap", "extrapolation", "auc_percent_extrap")
  )
  
  # Additional optional columns
  optional_mappings <- list(
    sequence = c("sequence", "seq", "period_sequence", "grp", "group"),
    period = c("period", "prd", "per", "phase", "occasion", "visit"),
    dose = c("dose", "amt", "amount_dose", "dosage"),
    weight = c("weight", "wt", "bw", "bodyweight"),
    age = c("age", "years"),
    gender = c("gender", "sex", "male_female", "m_f")
  )
  
  # Function to find column by possible names
  find_column <- function(data, possible_names) {
    data_names_lower <- tolower(names(data))
    possible_names_lower <- tolower(possible_names)
    
    for (name in possible_names_lower) {
      matches <- which(data_names_lower == name)
      if (length(matches) > 0) {
        return(names(data)[matches[1]])
      }
    }
    return(NULL)
  }
  
  # Map required columns to standard names
  mapped_columns <- list()
  for (std_name in names(column_mappings)) {
    found_col <- find_column(data, column_mappings[[std_name]])
    if (!is.null(found_col)) {
      mapped_columns[[std_name]] <- found_col
      # Rename column in processed data
      names(processed_data)[names(processed_data) == found_col] <- std_name
    }
  }
  
  # Map optional columns too
  for (std_name in names(optional_mappings)) {
    found_col <- find_column(data, optional_mappings[[std_name]])
    if (!is.null(found_col)) {
      mapped_columns[[std_name]] <- found_col
      # Rename column in processed data
      names(processed_data)[names(processed_data) == found_col] <- std_name
    }
  }
  
  # Map PK parameter columns
  mapped_pk_parameters <- list()
  for (pk_name in names(pk_parameter_mappings)) {
    found_col <- find_column(data, pk_parameter_mappings[[pk_name]])
    if (!is.null(found_col)) {
      mapped_pk_parameters[[pk_name]] <- found_col
      # Rename column in processed data
      names(processed_data)[names(processed_data) == found_col] <- pk_name
    }
  }
  
  # =============================================================================
  # REQUIRED COLUMN VALIDATION
  # =============================================================================
  
  required_columns <- c("subject", "treatment")
  missing_required <- setdiff(required_columns, names(mapped_columns))
  
  if (length(missing_required) > 0) {
    for (missing in missing_required) {
      possible_names <- paste(column_mappings[[missing]], collapse = ", ")
      errors <- c(errors, paste0("Missing required column '", missing, "'. Expected one of: ", possible_names))
    }
  }
  
  # Check for at least one PK parameter
  if (length(mapped_pk_parameters) == 0) {
    # Instead of error, set a flag for user specification
    warnings <- c(warnings, "No standard PK parameters automatically identified. You will need to specify which columns contain PK parameters.")
    suggestions <- c(suggestions, paste0("Available columns that could be PK parameters: ", 
                                       paste(setdiff(names(data), names(mapped_columns)), collapse = ", ")))
  } else {
    # Check for primary PK parameters
    primary_params <- c("AUC0t", "AUC0inf", "Cmax")
    found_primary <- intersect(names(mapped_pk_parameters), primary_params)
    
    if (length(found_primary) == 0) {
      warnings <- c(warnings, "No primary PK parameters (AUC0t, AUC0inf, Cmax) found. BE analysis may not be possible.")
    } else if (length(found_primary) < 2) {
      warnings <- c(warnings, "Only one primary PK parameter found. Standard BE analysis requires both AUC and Cmax.")
    }
  }
  
  # =============================================================================
  # DATA TYPE AND VALUE VALIDATION FOR PK DATA
  # =============================================================================
  
  if (length(errors) == 0) {  # Only proceed if we have required columns
    
    # Validate Subject column
    if ("subject" %in% names(processed_data)) {
      if (any(is.na(processed_data$subject))) {
        errors <- c(errors, "Subject column contains missing values")
      }
    }
    
    # Validate Treatment column
    if ("treatment" %in% names(processed_data)) {
      if (any(is.na(processed_data$treatment))) {
        errors <- c(errors, "Treatment column contains missing values")
      }
      
      # Check treatment values
      unique_treatments <- unique(processed_data$treatment)
      valid_treatments <- c("R", "T", "Reference", "Test", "r", "t", "reference", "test")
      
      invalid_treatments <- setdiff(unique_treatments, valid_treatments)
      if (length(invalid_treatments) > 0) {
        warnings <- c(warnings, paste0("Non-standard treatment values found: ", 
                                     paste(invalid_treatments, collapse = ", "), 
                                     ". Expected: R/T or Reference/Test"))
      }
    }
    
    # Validate PK parameter columns (should be numeric)
    for (pk_param in names(mapped_pk_parameters)) {
      if (pk_param %in% names(processed_data)) {
        col_data <- processed_data[[pk_param]]
        
        # Check if numeric
        if (!is.numeric(col_data)) {
          # Try to convert to numeric
          numeric_col <- suppressWarnings(as.numeric(col_data))
          if (all(is.na(numeric_col))) {
            errors <- c(errors, paste0("PK parameter '", pk_param, "' contains non-numeric values"))
          } else {
            warnings <- c(warnings, paste0("PK parameter '", pk_param, "' converted to numeric"))
            processed_data[[pk_param]] <- numeric_col
          }
        }
        
        # Check for negative values (not valid for PK parameters)
        if (is.numeric(processed_data[[pk_param]])) {
          negative_values <- sum(processed_data[[pk_param]] < 0, na.rm = TRUE)
          if (negative_values > 0) {
            warnings <- c(warnings, paste0("PK parameter '", pk_param, "' contains ", 
                                         negative_values, " negative values"))
          }
        }
        
        # Check for missing values
        missing_values <- sum(is.na(processed_data[[pk_param]]))
        if (missing_values > 0) {
          warnings <- c(warnings, paste0("PK parameter '", pk_param, "' has ", 
                                       missing_values, " missing values"))
        }
      }
    }
  }
  
  # =============================================================================
  # RETURN VALIDATION RESULTS
  # =============================================================================
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    suggestions = suggestions,
    data = processed_data,
    processed_data = processed_data,
    original_data = original_data,
    mapped_columns = mapped_columns,
    mapped_pk_parameters = mapped_pk_parameters,
    summary = if(length(errors) == 0) create_data_summary_enhanced(processed_data) else NULL,
    data_type = "pk_parameters"
  ))
}

# Helper function to find columns by pattern (for PK parameter processing)
find_column_by_pattern <- function(data, possible_names) {
  data_names_lower <- tolower(names(data))
  possible_names_lower <- tolower(possible_names)
  
  for (name in possible_names_lower) {
    matches <- which(data_names_lower == name)
    if (length(matches) > 0) {
      return(names(data)[matches[1]])
    }
  }
  return(NULL)
}

# Store unit specifications in reactive values for use in analysis
observeEvent(input$concentration_units, {
  if (!is.null(input$concentration_units)) {
    if (input$concentration_units == "other" && !is.null(input$concentration_units_custom)) {
      values$concentration_units <- input$concentration_units_custom
    } else {
      values$concentration_units <- input$concentration_units
    }
  }
})

observeEvent(input$time_units, {
  if (!is.null(input$time_units)) {
    if (input$time_units == "other" && !is.null(input$time_units_custom)) {
      values$time_units <- input$time_units_custom
    } else {
      values$time_units <- input$time_units
    }
  }
})

# Custom unit inputs
observeEvent(input$concentration_units_custom, {
  if (!is.null(input$concentration_units_custom) && input$concentration_units == "other") {
    values$concentration_units <- input$concentration_units_custom
  }
})

observeEvent(input$time_units_custom, {
  if (!is.null(input$time_units_custom) && input$time_units == "other") {
    values$time_units <- input$time_units_custom
  }
})

# Concentration data column mapping interface
output$concentration_column_mapper <- renderUI({
  req(values$uploaded_data_original, input$data_type == "concentration")
  
  data <- values$uploaded_data_original
  all_columns <- names(data)
  
  if (length(all_columns) == 0) {
    div(
      style = "background-color: #f8d7da; padding: 15px; border-radius: 8px; border-left: 4px solid #dc3545;",
      h5("⚠️ No columns found", style = "color: #721c24; margin-top: 0;"),
      p("The uploaded data appears to have no columns.", style = "color: #721c24;")
    )
  } else {
    
    # Auto-detect column mappings using existing logic
    detect_column_mapping <- function(col_name, data) {
      # Define expected column mappings (case insensitive)
      column_mappings <- list(
        subject = c("subject", "subj", "id", "subjid", "subject_id", "patientid", "patient", "vol", "volunteer"),
        treatment = c("treatment", "tmt", "trt", "formulation", "form", "drug", "product", "regimen"),
        period = c("period", "per", "phase", "visit"),
        sequence = c("sequence", "seq", "period_sequence", "grp", "group"),
        time = c("time", "timepoint", "hour", "hours", "hr", "sampling_time"),
        concentration = c("concentration", "conc", "result", "value", "plasma_conc", "serum_conc")
      )
      
      # Additional optional columns
      optional_mappings <- list(
        dose = c("dose", "dosage", "dose_amount"),
        weight = c("weight", "bw", "body_weight"),
        age = c("age", "subject_age"),
        gender = c("sex", "gender", "subject_sex")
      )
      
      # Combine all mappings
      all_mappings <- c(column_mappings, optional_mappings)
      
      # Check each mapping type
      for (data_type in names(all_mappings)) {
        possible_names <- tolower(all_mappings[[data_type]])
        col_lower <- tolower(col_name)
        
        # Exact match
        if (col_lower %in% possible_names) {
          return(data_type)
        }
        
        # Partial match
        for (name in possible_names) {
          if (grepl(name, col_lower, fixed = TRUE)) {
            return(data_type)
          }
        }
      }
      
      return("other")  # Default to "other" if no match found
    }
    
    div(
      # Combined auto-detection notification and column mapping guide
      div(style = "background-color: #d1ecf1; padding: 15px; border-radius: 6px; border-left: 4px solid #17a2b8; margin-bottom: 15px;",
        div(style = "margin-bottom: 10px;",
          icon("magic", style = "color: #0c5460;"), 
          strong(" Auto-Detection Applied", style = "color: #0c5460;")
        ),
        hr(style = "border-color: #17a2b8; margin: 10px 0;"),
        div(
          icon("info-circle", style = "color: #0c5460;"), 
          strong(" Column Mapping Guide:", style = "color: #0c5460;"), 
          br(),
          span("• Auto-detection has pre-selected the most likely data types", 
               style = "color: #0c5460; font-size: 13px;"),
          br(),
          span("• Required columns: Subject, Treatment, Time, Concentration", 
               style = "color: #0c5460; font-size: 13px;"),
          br(),
          span("• Verify selections and adjust if needed before confirming", 
               style = "color: #0c5460; font-size: 13px;"),
          br(),
          span("• Units will be used in analysis outputs and validation", 
               style = "color: #0c5460; font-size: 13px;")
        )
      ),
      
      div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;",
        
        # Dynamic mapping for each column
        div(id = "concentration_mapping_area",
          lapply(all_columns, function(col) {
            clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
            
            # Auto-detect the mapping for this column
            detected_mapping <- detect_column_mapping(col, data)
            
            fluidRow(style = "margin-bottom: 15px; background-color: white; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
              column(2,
                div(style = "background-color: #e9ecef; padding: 12px; border-radius: 6px; text-align: center; height: 60px; display: flex; align-items: center; justify-content: center;",
                  strong(col, style = "font-size: 14px;")
                )
              ),
              column(4,
                selectInput(
                  paste0("conc_mapping_", clean_col_id),
                  label = "Map to Data Type:",
                  choices = c("Select data type..." = "none",
                            "Subject ID" = "subject",
                            "Treatment/Formulation" = "treatment",
                            "Time" = "time",
                            "Concentration" = "concentration",
                            "Sequence" = "sequence",
                            "Period" = "period",
                            "Dose" = "dose",
                            "Weight" = "weight",
                            "Age" = "age",
                            "Gender" = "gender",
                            "Other/Ignore" = "other"),
                  selected = detected_mapping,  # Use auto-detected value
                  width = "100%"
                )
              ),
              column(3,
                # Units selection for concentration and time
                conditionalPanel(
                  condition = paste0("input.conc_mapping_", clean_col_id, " == 'concentration'"),
                  div(style = "margin-top: 25px;",
                    selectInput(
                      paste0("conc_units_", clean_col_id),
                      label = "Concentration Units:",
                      choices = list(
                        "ng/mL" = "ng/mL",
                        "μg/mL" = "μg/mL", 
                        "mg/mL" = "mg/mL",
                        "ng/L" = "ng/L",
                        "μg/L" = "μg/L",
                        "mg/L" = "mg/L",
                        "nmol/L" = "nmol/L",
                        "μmol/L" = "μmol/L",
                        "mmol/L" = "mmol/L",
                        "Other" = "other"
                      ),
                      selected = "ng/mL",
                      width = "100%"
                    ),
                    conditionalPanel(
                      condition = paste0("input.conc_units_", clean_col_id, " == 'other'"),
                      textInput(
                        paste0("conc_custom_units_", clean_col_id),
                        "Custom Units:",
                        placeholder = "e.g., pg/mL, IU/mL",
                        width = "100%"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = paste0("input.conc_mapping_", clean_col_id, " == 'time'"),
                  div(style = "margin-top: 25px;",
                    selectInput(
                      paste0("time_units_", clean_col_id),
                      label = "Time Units:",
                      choices = list(
                        "Hours" = "hours",
                        "Minutes" = "minutes",
                        "Days" = "days",
                        "Other" = "other"
                      ),
                      selected = "hours",
                      width = "100%"
                    ),
                    conditionalPanel(
                      condition = paste0("input.time_units_", clean_col_id, " == 'other'"),
                      textInput(
                        paste0("time_custom_units_", clean_col_id),
                        "Custom Units:",
                        placeholder = "e.g., weeks, seconds",
                        width = "100%"
                      )
                    )
                  )
                ),
                # For dose column
                conditionalPanel(
                  condition = paste0("input.conc_mapping_", clean_col_id, " == 'dose'"),
                  div(style = "margin-top: 25px;",
                    textInput(
                      paste0("dose_units_", clean_col_id),
                      label = "Dose Units:",
                      placeholder = "e.g., mg, μg, IU",
                      width = "100%"
                    )
                  )
                )
              ),
              column(3,
                # Show data preview for this column
                conditionalPanel(
                  condition = paste0("input.conc_mapping_", clean_col_id, " != 'none'"),
                  div(style = "margin-top: 25px;",
                    div(style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; max-height: 60px; overflow-y: auto;",
                      div(
                        strong("Sample values:", style = "font-size: 10px; color: #6c757d;"),
                        br(),
                        span(paste(head(data[[col]], 3), collapse = ", "), 
                             style = "font-size: 10px; color: #495057;")
                      )
                    )
                  )
                )
              )
            )
          })
        )
      )
    )
  }
})

# Handle concentration column mapping confirmation
observeEvent(input$confirm_concentration_mapping, {
  req(values$uploaded_data_original, input$data_type == "concentration")
  
  data <- values$uploaded_data_original
  all_columns <- names(data)
  
  # Collect user mappings and units
  column_mappings <- list()
  units_settings <- list()
  
  for (col in all_columns) {
    clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
    mapping_input <- input[[paste0("conc_mapping_", clean_col_id)]]
    
    if (!is.null(mapping_input) && mapping_input != "none") {
      # Store the mapping
      column_mappings[[mapping_input]] <- col
      
      # Get units for concentration
      if (mapping_input == "concentration") {
        units_input <- input[[paste0("conc_units_", clean_col_id)]]
        if (!is.null(units_input)) {
          if (units_input == "other") {
            custom_units <- input[[paste0("conc_custom_units_", clean_col_id)]]
            if (!is.null(custom_units) && custom_units != "") {
              units_settings[["concentration"]] <- custom_units
            }
          } else {
            units_settings[["concentration"]] <- units_input
          }
        }
      }
      
      # Get units for time
      if (mapping_input == "time") {
        units_input <- input[[paste0("time_units_", clean_col_id)]]
        if (!is.null(units_input)) {
          if (units_input == "other") {
            custom_units <- input[[paste0("time_custom_units_", clean_col_id)]]
            if (!is.null(custom_units) && custom_units != "") {
              units_settings[["time"]] <- custom_units
            }
          } else {
            units_settings[["time"]] <- units_input
          }
        }
      }
      
      # Get units for dose
      if (mapping_input == "dose") {
        units_input <- input[[paste0("dose_units_", clean_col_id)]]
        if (!is.null(units_input) && units_input != "") {
          units_settings[["dose"]] <- units_input
        }
      }
    }
  }
  
  # Validate required mappings
  required_mappings <- c("subject", "treatment", "time", "concentration")
  missing_required <- setdiff(required_mappings, names(column_mappings))
  
  if (length(missing_required) > 0) {
    showNotification(
      paste("⚠️ Missing required column mappings:", paste(missing_required, collapse = ", ")), 
      type = "warning", duration = 8
    )
    return()
  }
  
  # Check for duplicate mappings
  mapped_types <- names(column_mappings)
  duplicated_types <- mapped_types[duplicated(mapped_types)]
  if (length(duplicated_types) > 0) {
    showNotification(
      paste("⚠️ Duplicate column mappings found for:", paste(unique(duplicated_types), collapse = ", ")), 
      type = "warning", duration = 8
    )
    return()
  }
  
  # Store the mappings and units
  values$user_column_mappings <- column_mappings
  values$concentration_units_settings <- units_settings
  
  # Update the processed data with new column names
  processed_data <- values$uploaded_data_original
  for (data_type in names(column_mappings)) {
    original_col <- column_mappings[[data_type]]
    names(processed_data)[names(processed_data) == original_col] <- data_type
  }
  
  # Re-run validation with user mappings
  validation_result <- validate_bioeq_data_enhanced(processed_data)
  validation_result$column_mappings <- column_mappings
  validation_result$units_settings <- units_settings
  validation_result$user_specified <- TRUE
  
  # Only mark as valid if validation actually passed
  if (validation_result$valid) {
    validation_result$errors <- character(0)
  }
  
  values$uploaded_data <- processed_data
  values$validation_result <- validation_result
  values$columns_mapped <- TRUE
  
  # Recreate data summary with mapped data
  values$data_summary <- create_data_summary_enhanced(processed_data)
  values$data_summary$column_mappings_confirmed <- TRUE
  values$data_summary$mapped_columns <- names(column_mappings)
  values$data_summary$units_settings <- units_settings
  
  showNotification(
    paste("✅ Column mapping confirmed!", length(column_mappings), "columns mapped"), 
    type = "message", duration = 5
  )
})

# Simplified PK parameter selector - only column mapping
output$pk_parameter_selector <- renderUI({
  req(values$uploaded_data_original)
  
  data <- values$uploaded_data_original
  all_columns <- names(data)
  
  if (length(all_columns) == 0) {
    div(
      style = "background-color: #f8d7da; padding: 15px; border-radius: 8px; border-left:  4px solid #dc3545;",
      h5("⚠️ No columns found", style = "color: #721c24; margin-top: 0;"),
      p("The uploaded data appears to have no columns.", style = "color: #721c24;")
    )
  } else {
    
    # Complete auto-detect function for PK data
    detect_column_mapping_pk <- function(col_name) {
      # Define expected column mappings (case insensitive)
      column_mappings <- list(
        subject = c("subject", "subj", "id", "subjid", "subject_id", "patientid", "patient", "vol", "volunteer"),
        treatment = c("treatment", "tmt", "trt", "formulation", "form", "drug", "product", "regimen"),
        period = c("period", "per", "phase", "visit"),
        sequence = c("sequence", "seq", "period_sequence", "grp", "group"),
        dose = c("dose", "dosage", "dose_amount"),
        weight = c("weight", "bw", "body_weight"),
        age = c("age", "subject_age"),
        gender = c("sex", "gender", "subject_sex")
      )
      
      # PK parameter mappings
      pk_mappings <- list(
        AUC0t = c("auc0t", "auc_0_t", "auc0-t", "auc_0t", "auct", "auc_t", "auc_last", "auclast"),
        AUC0inf = c("auc0inf", "auc_0_inf", "auc0-inf", "auc_inf", "aucinfinity", "auc_infinity", "aucinf", "auc_0_infinity"),
        Cmax = c("cmax", "c_max", "peak", "maximum_concentration", "max_conc"),
        Tmax = c("tmax", "t_max", "time_max", "peak_time", "time_to_max", "timemax"),
        half_life = c("half_life", "halflife", "t_half", "t1/2", "t12", "elimination_half_life", "thalf", "hl"),
        clearance = c("clearance", "cl", "clear", "apparent_clearance", "cl_f", "clf"),
        volume = c("volume", "vd", "vdf", "vd_f", "volume_distribution", "apparent_volume")
      )
      
      # Combine all mappings
      all_mappings <- c(column_mappings, pk_mappings)
      
      # Check each mapping type
      col_lower <- tolower(col_name)
      for (data_type in names(all_mappings)) {
        possible_names <- tolower(all_mappings[[data_type]])
        
        # Exact match
        if (col_lower %in% possible_names) {
          return(data_type)
        }
        
        # Partial match
        for (name in possible_names) {
          if (grepl(name, col_lower, fixed = TRUE)) {
            return(data_type)
          }
        }
      }
      
      return("other")  # Default to "other" if no match found
    }
    
    # Build the UI
    div(
      # Combined auto-detection notification and required columns information
      div(style = "background-color: #fff3cd; padding: 15px; border-radius: 6px; border-left: 4px solid #ffc107; margin-bottom: 15px;",
        div(style = "margin-bottom: 10px;",
          icon("magic", style = "color: #856404;"), 
          strong(" Auto-Detection Applied", style = "color: #856404;"), 
          br(),
          span("Column types have been automatically identified for all columns. Please verify and adjust if needed.", 
               style = "color: #856404; font-size: 13px;")
        ),
        hr(style = "border-color: #ffc107; margin: 10px 0;"),
        div(
          icon("info-circle", style = "color: #856404;"), 
          strong(" Required Columns:", style = "color: #856404;"), 
          br(),
          span("• Subject ID and Treatment/Formulation are required for PK analysis", 
               style = "color: #856404; font-size: 13px;"),
          br(),
          span("• Columns identified as 'PK Parameter' will be configured in Section B below", 
               style = "color: #856404; font-size: 13px;")
        )
      ),
      
      div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;",
        
        # Section A: General Column Mapping (changed from Step 1)
        div(
          h5("📋 Section A: General Column Mapping", style = "color: #495057; margin-bottom: 15px; border-bottom: 2px solid #dee2e6; padding-bottom: 8px;"),
          p("Map columns for subject identification, treatment groups, and demographic information:", 
            style = "color: #6c757d; margin-bottom: 15px; font-size: 14px;"),
          
          # Dynamic mapping for each column
          div(id = "pk_general_mapping_area",
            lapply(all_columns, function(col) {
              clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
              
              # Auto-detect the mapping for this column
              detected_mapping <- detect_column_mapping_pk(col)
              
              # Determine if this should be marked as PK parameter
              is_pk_param <- detected_mapping %in% c("AUC0t", "AUC0inf", "Cmax", "Tmax", "half_life", "clearance", "volume")
              
              fluidRow(style = "margin-bottom: 15px; background-color: white; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
                column(2,
                  div(style = "background-color: #e9ecef; padding: 12px; border-radius: 6px; text-align: center; height: 60px; display: flex; align-items: center; justify-content: center;",
                    strong(col, style = "font-size: 14px;")
                  )
                ),
                column(4,
                  selectInput(
                    paste0("pk_general_mapping_", clean_col_id),
                    label = "Map to Data Type:",
                    choices = c("Select data type..." = "none",
                              "Subject ID" = "subject",
                              "Treatment/Formulation" = "treatment",
                              "Sequence" = "sequence",
                              "Period" = "period",
                              "Dose" = "dose",
                              "Weight" = "weight",
                              "Age" = "age",
                              "Gender" = "gender",
                              "PK Parameter" = "pk_parameter",
                              "Other/Ignore" = "other"),
                    selected = if(is_pk_param) "pk_parameter" else detected_mapping,
                    width = "100%"
                  )
                ),
                column(3,
                  # Units selection for dose
                  conditionalPanel(
                    condition = paste0("input.pk_general_mapping_", clean_col_id, " == 'dose'"),
                    div(style = "margin-top: 25px;",
                      textInput(
                        paste0("dose_units_", clean_col_id),
                        label = "Dose Units:",
                        placeholder = "e.g., mg, μg, IU",
                        width = "100%"
                      )
                    )
                  )
                ),
                column(3,
                  # Show data preview for this column
                  conditionalPanel(
                    condition = paste0("input.pk_general_mapping_", clean_col_id, " != 'none'"),
                    div(style = "margin-top: 25px;",
                      div(style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; max-height: 60px; overflow-y: auto;",
                        div(
                          strong("Sample values:", style = "font-size: 10px; color: #6c757d;"),
                          br(),
                          span(paste(head(data[[col]], 3), collapse = ", "), 
                               style = "font-size: 10px; color: #495057;")
                        )
                      )
                    )
                  )
                )
              )
            })
          )
        ),
        
        br(), hr(), br(),
        
        # Section B: PK Parameter Identification (changed from Step 2)
        div(
          h5("🔬 Section B: PK Parameter Identification", style = "color: #495057; margin-bottom: 15px; border-bottom: 2px solid #dee2e6; padding-bottom: 8px;"),
          p("Configure columns identified or selected as PK parameters with units and log-transformation settings:", 
            style = "color: #6c757d; margin-bottom: 15px; font-size: 14px;"),
          
          # Information about log transformation
          div(style = "margin-bottom: 20px; background-color: #fff3cd; padding: 12px; border-radius: 6px; border-left: 4px solid #ffc107;",
            div(
              icon("info-circle", style = "color: #856404; margin-right: 8px;"),
              strong("Log-Transformation Information", style = "color: #856404;"),
              br(),
              span("For each parameter, indicate if the data is already log-transformed. This is important for proper statistical analysis.", 
                   style = "font-size: 12px; color: #856404; font-weight: normal;")
            )
          ),
          
          # PK Parameter configuration area - THIS IS THE KEY CHANGE
          # Replace the static div with uiOutput
          uiOutput("pk_parameter_config_area")
        ),
        
        br(),
        div(style = "text-align: center;",
          actionButton(
            "confirm_pk_mapping",
            "Confirm Column Mapping",
            class = "btn btn-primary",
            icon = icon("check"),
            style = "padding: 10px 20px; font-weight: 500;"
          )
        )
      )
    )
  }
})

# Dynamic PK parameter configuration based on Step 1 selections
# Create a reactive value to store PK parameter columns
values$pk_parameter_columns <- character(0)

# Create a reactive expression that properly tracks all general mapping inputs
pk_parameter_columns_reactive <- reactive({
  req(values$uploaded_data_original)
  
  data_cols <- names(values$uploaded_data_original)
  pk_cols <- c()
  
  # Check each column's mapping selection
  for (col in data_cols) {
    clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
    input_name <- paste0("pk_general_mapping_", clean_col_id)
    
    # Force dependency on this input
    input_value <- input[[input_name]]
    
    # Check if this input exists and is set to "pk_parameter"
    if (!is.null(input_value) && input_value == "pk_parameter") {
      pk_cols <- c(pk_cols, col)
    }
  }
  
  # Return the PK columns
  pk_cols
})

# Update the pk_parameter_config_area output to use the reactive
output$pk_parameter_config_area <- renderUI({
  req(values$uploaded_data_original)
  
  # Use the reactive expression to get PK columns
  pk_columns <- pk_parameter_columns_reactive()
  
  # Force re-execution when any mapping changes
  isolate({
    # This ensures we re-render whenever the reactive updates
    trigger <- runif(1)
  })
  
  if (length(pk_columns) == 0) {
    div(
      p("Select columns as 'PK Parameter' in Section A above to configure them here.", 
        style = "color: #6c757d; font-style: italic; text-align: center; padding: 20px."),
      p(paste("Monitoring", length(names(values$uploaded_data_original)), "columns for PK parameter selections..."), 
        style = "color: #6c757d; font-size: 11px; text-align: center;")
    )
  } else {
    
    # Auto-detect PK parameter types
    detect_pk_parameter_type <- function(col_name) {
      pk_mappings <- list(
        AUC0t = c("auc0t", "auc_0_t", "auc0-t", "auct", "auc_t", "auc_last", "auclast"),
        AUC0inf = c("auc0inf", "auc_0_inf", "auc0-inf", "auc_inf", "aucinfinity", "auc_infinity", "aucinf", "auc_0_infinity"),
        Cmax = c("cmax", "c_max", "peak", "maximum_concentration", "max_conc"),
        Tmax = c("tmax", "t_max", "time_max", "peak_time", "time_to_max", "timemax"),
        half_life = c("half_life", "halflife", "t_half", "t1/2", "t12", "elimination_half_life", "thalf", "hl"),
        clearance = c("clearance", "cl", "clear", "apparent_clearance", "cl_f", "clf"),
        volume = c("volume", "vd", "vdf", "vd_f", "volume_distribution", "apparent_volume")
      )
      
      col_lower <- tolower(col_name)
      for (param_type in names(pk_mappings)) {
        possible_names <- tolower(pk_mappings[[param_type]])
        if (col_lower %in% possible_names) return(param_type)
        for (name in possible_names) {
          if (grepl(name, col_lower, fixed = TRUE)) return(param_type)
        }
      }
      return("other")
    }
    
    div(
      div(style = "background-color: #d1ecf1; padding: 12px; border-radius: 6px; border-left: 4px solid #17a2b8; margin-bottom: 15px;",
        icon("check-circle", style = "color: #0c5460;"), 
        strong(paste(" Found", length(pk_columns), "PK Parameter column(s)"), style = "color: #0c5460;"), 
        br(),
        span(paste("Configuring:", paste(pk_columns, collapse = ", ")), 
             style = "color: #0c5460; font-size: 12px;")
      ),
      lapply(pk_columns, function(col) {
        clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
        detected_pk <- detect_pk_parameter_type(col)
        
        fluidRow(style = "margin-bottom: 15px; background-color: white; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
          column(2,
            div(style = "background-color: #e9ecef; padding: 12px; border-radius: 6px; text-align: center; height: 60px; display: flex; align-items: center; justify-content: center;",
              strong(col, style = "font-size: 14px;")
            )
          ),
          column(4,
            selectInput(
              paste0("pk_param_type_", clean_col_id),
              label = "PK Parameter Type:",
              choices = c("Select parameter..." = "none",
                        "AUC0t (AUC from 0 to last time)" = "AUC0t",
                        "AUC0inf (AUC from 0 to infinity)" = "AUC0inf", 
                        "Cmax (Maximum concentration)" = "Cmax",
                        "Tmax (Time to maximum concentration)" = "Tmax",
                        "Half-life (Elimination half-life)" = "half_life",
                        "Clearance (Apparent clearance)" = "clearance",
                        "Volume (Apparent volume of distribution)" = "volume",
                        "AUC extrapolation %" = "AUC_extrap_percent",
                        "Custom parameter..." = "other"),
              selected = detected_pk,
              width = "100%"
            ),
            conditionalPanel(
              condition = paste0("input.pk_param_type_", clean_col_id, " == 'other'"),
              textInput(
                paste0("pk_custom_param_", clean_col_id),
                "Custom parameter name:",
                placeholder = "e.g., Tlag, Ke, MRT",
                width = "100%"
              )
            )
          ),
          column(3,
            # Units selection for each parameter
            conditionalPanel(
              condition = paste0("input.pk_param_type_", clean_col_id, " != 'none'"),
              div(style = "margin-top: 25px;",
                # AUC units
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'AUC0t' || input.pk_param_type_", clean_col_id, " == 'AUC0inf'"),
                  selectInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "AUC Units:",
                    choices = list(
                      "ng·h/mL" = "ng*h/mL",
                      "μg·h/mL" = "μg*h/mL", 
                      "mg·h/mL" = "mg*h/mL",
                      "ng·h/L" = "ng*h/L",
                      "μg·h/L" = "μg*h/L",
                      "mg·h/L" = "mg*h/L",
                      "Other" = "other"
                    ),
                    selected = "ng*h/mL",
                    width = "100%"
                  )
                ),
                # Concentration units (Cmax)
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'Cmax'"),
                  selectInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "Concentration Units:",
                    choices = list(
                      "ng/mL" = "ng/mL",
                      "μg/mL" = "μg/mL", 
                      "mg/mL" = "mg/mL",
                      "ng/L" = "ng/L",
                      "μg/L" = "μg/L",
                      "mg/L" = "mg/L",
                      "Other" = "other"
                    ),
                    selected = "ng/mL",
                    width = "100%"
                  )
                ),
                # Time units (Tmax, half-life)
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'Tmax' || input.pk_param_type_", clean_col_id, " == 'half_life'"),
                  selectInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "Time Units:",
                    choices = list(
                      "Hours" = "hours",
                      "Minutes" = "minutes",
                      "Days" = "days",
                      "Other" = "other"
                    ),
                    selected = "hours",
                    width = "100%"
                  )
                ),
                # Clearance units
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'clearance'"),
                  selectInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "Clearance Units:",
                    choices = list(
                      "L/h" = "L/h",
                      "mL/min" = "mL/min",
                      "L/h/kg" = "L/h/kg",
                      "mL/min/kg" = "mL/min/kg",
                      "Other" = "other"
                    ),
                    selected = "L/h",
                    width = "100%"
                  )
                ),
                # Volume units
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'volume'"),
                  selectInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "Volume Units:",
                    choices = list(
                      "L" = "L",
                      "mL" = "mL",
                      "L/kg" = "L/kg",
                      "mL/kg" = "mL/kg",
                      "Other" = "other"
                    ),
                    selected = "L",
                    width = "100%"
                  )
                ),
                # Percentage (no units needed)
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'AUC_extrap_percent'"),
                  div(style = "padding: 8px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
                    span("Percentage (%)", style = "font-size: 12px; color: #6c757d; font-style: italic;")
                  )
                ),
                # Custom parameter units
                conditionalPanel(
                  condition = paste0("input.pk_param_type_", clean_col_id, " == 'other'"),
                  textInput(
                    paste0("pk_param_units_", clean_col_id),
                    label = "Custom Units:",
                    placeholder = "e.g., mg/L, h",
                    width = "100%"
                  )
                )
              )
            )
          ),
          column(3,
            # Individual log-transformation checkbox for each parameter
            conditionalPanel(
              condition = paste0("input.pk_param_type_", clean_col_id, " != 'none' && input.pk_param_type_", clean_col_id, " != 'Tmax'"),
              div(style = "margin-top: 25px;",
                checkboxInput(
                  paste0("pk_param_log_", clean_col_id),
                  label = div(
                    strong("Log-transformed", style = "font-size: 12px;"),
                    br(),
                    span("Check if already logged", style = "font-size: 10px; color: #6c757d;")
                  ),
                  value = FALSE,
                  width = "100%"
                )
              )
            ),
            # Note for Tmax (typically not log-transformed)
            conditionalPanel(
              condition = paste0("input.pk_param_type_", clean_col_id, " == 'Tmax'"),
              div(style = "margin-top: 25px; padding: 8px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
                span("Tmax is typically", br(), "not log-transformed", 
                     style = "font-size: 10px; color: #6c757d; font-style: italic;")
              )
            )
          )
        )
      })
    )
  }
})

# Handle PK parameter mapping confirmation
observeEvent(input$confirm_pk_mapping, {
  req(values$uploaded_data_original)
  
  data <- values$uploaded_data_original
  all_columns <- names(data)
  
  # Collect general column mappings (Step 1)
  general_mappings <- list()
  pk_parameter_columns <- c()
  
  for (col in all_columns) {
    clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
    mapping_input <- input[[paste0("pk_general_mapping_", clean_col_id)]]
    
    if (!is.null(mapping_input) && mapping_input != "none" && mapping_input != "other") {
      if (mapping_input == "pk_parameter") {
        pk_parameter_columns <- c(pk_parameter_columns, col)
      } else {
        general_mappings[[mapping_input]] <- col
      }
    }
  }
  
  # Collect PK parameter mappings from Step 2
  pk_mappings <- list()
  pk_log_settings <- list()
  pk_units_settings <- list()
  
  for (col in pk_parameter_columns) {
    clean_col_id <- gsub("[^A-Za-z0-9]", "_", col)
    param_type_input <- input[[paste0("pk_param_type_", clean_col_id)]]
    
    if (!is.null(param_type_input) && param_type_input != "none") {
      parameter_name <- NULL
      
      if (param_type_input == "other") {
        custom_name <- input[[paste0("pk_custom_param_", clean_col_id)]]
        if (!is.null(custom_name) && custom_name != "") {
          parameter_name <- custom_name
          pk_mappings[[custom_name]] <- col
        }
      } else {
        parameter_name <- param_type_input
        pk_mappings[[param_type_input]] <- col
      }
      
      # Get log-transformation setting for this specific parameter
      if (!is.null(parameter_name)) {
        log_input <- input[[paste0("pk_param_log_", clean_col_id)]]
        pk_log_settings[[parameter_name]] <- ifelse(is.null(log_input), FALSE, log_input)
        
        # Get units setting for this specific parameter
        units_input <- input[[paste0("pk_param_units_", clean_col_id)]]
        if (!is.null(units_input)) {
          pk_units_settings[[parameter_name]] <- units_input
        }
      }
    }
  }
  
  # Validate required mappings
  if (!"subject" %in% names(general_mappings)) {
    showNotification("⚠️ Subject column must be identified", type = "warning")
    return()
  }
  
  if (!"treatment" %in% names(general_mappings)) {
    showNotification("⚠️ Treatment column must be identified", type = "warning")
    return()
  }
  
  if (length(pk_mappings) == 0) {
    showNotification("⚠️ At least one PK parameter must be identified", type = "warning")
    return()
  }
  
  # Store all mappings
  values$user_column_mappings <- general_mappings
  values$user_pk_mappings <- pk_mappings
  values$pk_log_settings <- pk_log_settings
  values$pk_units_settings <- pk_units_settings
  
  # Update the processed data with new column names
  processed_data <- values$uploaded_data_original
  
  # Map general columns
  for (data_type in names(general_mappings)) {
    original_col <- general_mappings[[data_type]]
    if (original_col %in% names(processed_data)) {
      names(processed_data)[names(processed_data) == original_col] <- data_type
    }
  }
  
  # Map PK parameter columns
  for (pk_param in names(pk_mappings)) {
    original_col <- pk_mappings[[pk_param]]
    if (original_col %in% names(processed_data)) {
      names(processed_data)[names(processed_data) == original_col] <- pk_param
    }
  }
  
  # Re-run validation and update state
  validation_result <- validate_pk_data_enhanced(processed_data)
  validation_result$mapped_pk_parameters <- pk_mappings
  validation_result$column_mappings <- general_mappings
  validation_result$pk_log_settings <- pk_log_settings
  validation_result$pk_units_settings <- pk_units_settings
  validation_result$user_specified <- TRUE
  validation_result$valid <- TRUE
  validation_result$errors <- character(0)
  
  values$uploaded_data <- processed_data
  values$validation_result <- validation_result
  values$columns_mapped <- TRUE
  
  # Update data summary
  values$data_summary <- create_data_summary_enhanced(processed_data)
  values$data_summary$pk_mappings_confirmed <- TRUE
  values$data_summary$data_type <- "pk_parameters"
  
  showNotification(
    paste("✅ Column mapping confirmed!", 
          length(general_mappings), "general columns,", 
          length(pk_mappings), "PK parameters mapped"), 
    type = "message", duration = 5
  )
})

# Help modal
observeEvent(input$show_help, {
  showModal(modalDialog(
    title = "Data Upload Help",
    size = "l",
    
    h4("Data Format Requirements"),
    p("Your bioequivalence data should be in a specific format for proper analysis:"),
    
    h5("Required Columns:"),
    tags$ul(
      tags$li(tags$strong("Subject:"), " Unique identifier for each study participant (numeric)"),
      tags$li(tags$strong("Treatment:"), " Treatment code - use 'R' for Reference and 'T' for Test"),
      tags$li(tags$strong("Subject:"), " Unique identifier for each study participant (numeric)"),
      tags$li(tags$strong("Treatment:"), " Treatment code - use 'R' for Reference and 'T' for Test"),
      tags$li(tags$strong("Time:"), " Sampling time in hours from dosing (numeric)"),
      tags$li(tags$strong("Concentration:"), " Drug concentration in ng/mL or appropriate units (numeric)")
    ),
    
    h5("Data Quality Tips:"),
    tags$ul(
      tags$li("Include pre-dose concentrations (Time = 0)"),
      tags$li("Ensure no missing subject IDs"),
      tags$li("Check for negative concentrations (may indicate assay issues)")
    ),
    
    h5("Common Issues:"),
    tags$ul(
      tags$li("Mixed treatment codes (e.g., 'Ref', 'Test' instead of 'R', 'T')"),
      tags$li("Missing time points for some subjects"),
      tags$li("Inconsistent time units (minutes vs hours)")
    ),
    
    footer = modalButton("Close")
  ))
})

# =============================================================================
# WORKFLOW STEP TRACKING FOR NEW REORGANIZED UI
# =============================================================================

# Track current workflow step for enhanced user guidance
current_workflow_step <- reactive({
  if (is.null(values$uploaded_data)) {
    return(1)  # Step 1: Upload data
  } else if (!is.null(values$uploaded_data) && is.null(values$validation_result)) {
    return(2)  # Step 2: Data preview available, validation pending
  } else if (!is.null(values$validation_result)) {
    data_type <- input$data_type %||% "concentration"
    if (data_type == "concentration") {
      return(3)  # Step 3: Unit specifications for concentration data
    } else {
      # For PK parameters, check if parameter identification is complete
      pk_mappings_complete <- !is.null(input$pk_mapping_confirmed) && input$pk_mapping_confirmed
      if (pk_mappings_complete) {
        return(4)  # Step 4: Unit specifications for PK data
      } else {
        return(3)  # Step 3: PK parameter identification
      }
    }
  }
  return(1)
})

# Enhanced validation message with step-specific guidance
output$workflow_guidance <- renderUI({
  current_step <- current_workflow_step()
  data_type <- input$data_type %||% "concentration"
  
  guidance_content <- switch(current_step,
    "1" = list(
      icon = "upload",
      color = "#3498db", 
      title = "Ready to Upload",
      message = "Select your data type and upload your bioequivalence study file."
    ),
    "2" = list(
      icon = "table",
      color = "#27ae60",
      title = "Data Successfully Loaded", 
      message = "Review your data preview and summary below, then proceed to the next step."
    ),
    "3" = if (data_type == "concentration") {
      list(
        icon = "ruler",
        color = "#9b59b6",
        title = "Specify Units",
        message = "Please specify the units for your concentration and time data."
      )
    } else {
      list(
        icon = "tags", 
        color = "#e74c3c",
        title = "Identify PK Parameters",
        message = "Map your data columns to the appropriate PK parameter types."
      )
    },
    "4" = list(
      icon = "ruler",
      color = "#9b59b6", 
      title = "Specify PK Parameter Units",
      message = "Please specify the units for your identified PK parameters."
    ),
    "5" = list(
      icon = "clipboard-check",
      color = "#17a2b8",
      title = "Final Validation",
      message = "Review the validation results and proceed to analysis setup."
    )
  )
  
  if (!is.null(guidance_content)) {
    div(
      style = paste0("background: linear-gradient(135deg, ", guidance_content$color, "15 0%, ", guidance_content$color, "05 100%); 
                     border-left: 4px solid ", guidance_content$color, "; 
                     padding: 15px; border-radius: 8px; margin: 15px 0;"),
      div(style = "display: flex; align-items: center;",
        icon(guidance_content$icon, style = paste0("color: ", guidance_content$color, "; font-size: 24px; margin-right: 15px;")),
        div(
          h5(guidance_content$title, style = paste0("color: ", guidance_content$color, "; margin: 0; font-weight: 600;")),
          p(guidance_content$message, style = "color: #2d3748; margin: 5px 0 0 0; font-size: 14px;")
        )
      )
    )
  }
})
