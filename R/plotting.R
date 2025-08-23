# BioEQ - Plotting and Visualization Functions
# Modern plotting functions for bioequivalence analysis with dual-output support (static & interactive)

# Required packages for plotting
required_packages <- c("ggplot2", "plotly", "dplyr", "tidyr", "gridExtra", "htmlwidgets")

# Check and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    message("Installing required package: ", pkg)
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#' Validate concentration-time data structure
#'
#' @param data Data frame to validate
#' @param silent Logical, if TRUE returns validation result instead of stopping (default FALSE)
#' @return If silent=TRUE, returns list with valid (logical) and message (character)
#' @export
validate_conc_time_data <- function(data, silent = FALSE) {
  required_cols <- c("Time", "Concentration", "Subject", "Formulation")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    msg <- paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    if (silent) {
      return(list(valid = FALSE, message = msg))
    } else {
      stop(msg)
    }
  }
  
  if (nrow(data) == 0) {
    msg <- "Data frame is empty"
    if (silent) {
      return(list(valid = FALSE, message = msg))
    } else {
      stop(msg)
    }
  }
  
  # Check for valid formulations
  formulations <- unique(data$Formulation)
  warnings <- c()
  
  if (length(formulations) < 2) {
    warning_msg <- paste("Only one formulation found in data:", paste(formulations, collapse = ", "))
    if (silent) {
      warnings <- c(warnings, warning_msg)
    } else {
      warning(warning_msg)
    }
  }
  
  # Check for negative concentrations
  if (any(data$Concentration < 0, na.rm = TRUE)) {
    warning_msg <- "Negative concentrations found in data"
    if (silent) {
      warnings <- c(warnings, warning_msg)
    } else {
      warning(warning_msg)
    }
  }
  
  # Check for negative times
  if (any(data$Time < 0, na.rm = TRUE)) {
    warning_msg <- "Negative time points found in data"
    if (silent) {
      warnings <- c(warnings, warning_msg)
    } else {
      warning(warning_msg)
    }
  }
  
  if (silent) {
    return(list(valid = TRUE, message = "Data validation passed", warnings = warnings))
  }
  
  invisible(TRUE)
}

# =============================================================================
# SHINY-OPTIMIZED PLOTTING FUNCTIONS
# =============================================================================

#' Standardize column names for plotting functions
#'
#' @param data Data frame to standardize
#' @param required_cols Named vector of required columns, where names are the 
#'                      standard names and values are possible alternative names
#' @return Data frame with standardized column names
#' @export
standardize_column_names <- function(data, required_cols = NULL) {
  if (is.null(required_cols)) {
    # Default mappings for bioequivalence data
    required_cols <- list(
      "Formulation" = c("Formulation", "Treatment", "Trt", "Group"),
      "Subject" = c("Subject", "ID", "SUBJID", "subject_id"),
      "Period" = c("Period", "PERIOD", "period"),
      "Sequence" = c("Sequence", "SEQ", "sequence")
    )
  }
  
  standardized_data <- data
  mapping_log <- c()
  
  for (standard_name in names(required_cols)) {
    possible_names <- required_cols[[standard_name]]
    
    # Find which column exists in the data
    existing_col <- intersect(names(data), possible_names)
    
    if (length(existing_col) > 0) {
      # Use the first match
      actual_col <- existing_col[1]
      
      if (actual_col != standard_name) {
        # Rename the column
        names(standardized_data)[names(standardized_data) == actual_col] <- standard_name
        mapping_log <- c(mapping_log, paste0(actual_col, " -> ", standard_name))
      }
    }
  }
  
  # Add mapping information as an attribute
  attr(standardized_data, "column_mapping") <- mapping_log
  
  return(standardized_data)
}

#' Configure plotly object for optimal Shiny display
#'
#' @param plotly_obj A plotly object
#' @param height Plot height in pixels (default 400)
#' @param show_mode_bar Logical, whether to show the plotly mode bar (default TRUE)
#' @return Configured plotly object
#' @export
configure_plotly_for_shiny <- function(plotly_obj, height = 400, show_mode_bar = TRUE) {
  if (!inherits(plotly_obj, "plotly")) {
    stop("Input must be a plotly object")
  }
  
  # Standard Shiny-friendly configuration
  plotly_obj %>%
    plotly::config(
      displayModeBar = show_mode_bar,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 
                               'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d',
                               'hoverClosestCartesian', 'hoverCompareCartesian'),
      responsive = TRUE,
      toImageButtonOptions = list(
        format = 'png',
        filename = 'bioeq_plot',
        height = height,
        width = 1000,
        scale = 2
      )
    ) %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 50, r = 50, t = 80, b = 80),
      font = list(size = 12),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
}

#' Shiny-safe plot wrapper for concentration-time profiles
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @param interactive Logical, whether to create interactive plotly plot
#' @param height Plot height in pixels for Shiny (default 400)
#' @return List with plot object and any error/warning messages
#' @export
shiny_plot_concentration_time <- function(data, log_scale = FALSE, individual = TRUE, 
                                         mean_profile = TRUE, interactive = TRUE, height = 400) {
  tryCatch({
    # Validate data with silent mode
    validation <- validate_conc_time_data(data, silent = TRUE)
    if (!validation$valid) {
      return(list(plot = NULL, error = validation$message, warnings = NULL))
    }
    
    # Create the plot
    plot_obj <- plot_concentration_time(data, log_scale, individual, mean_profile, interactive)
    
    # Configure for Shiny output
    if (interactive && inherits(plot_obj, "plotly")) {
      plot_obj <- configure_plotly_for_shiny(plot_obj, height)
    }
    
    return(list(plot = plot_obj, error = NULL, warnings = validation$warnings))
    
  }, error = function(e) {
    return(list(plot = NULL, error = paste("Plot generation failed:", e$message), warnings = NULL))
  })
}

#' Shiny-safe plot wrapper for bioequivalence confidence intervals
#'
#' @param be_results Results from perform_be_analysis
#' @param interactive Logical, whether to create interactive plotly plot
#' @param height Plot height in pixels for Shiny (default 400)
#' @return List with plot object and any error/warning messages
#' @export
shiny_plot_be_confidence_intervals <- function(be_results, interactive = TRUE, height = 400) {
  tryCatch({
    if (!inherits(be_results, "bioeq")) {
      return(list(plot = NULL, error = "Input must be a bioeq object from perform_be_analysis", warnings = NULL))
    }
    
    # Create the plot
    plot_obj <- plot_be_confidence_intervals(be_results, interactive)
    
    # Configure for Shiny output
    if (interactive && inherits(plot_obj, "plotly")) {
      plot_obj <- configure_plotly_for_shiny(plot_obj, height)
    }
    
    return(list(plot = plot_obj, error = NULL, warnings = NULL))
    
  }, error = function(e) {
    return(list(plot = NULL, error = paste("Plot generation failed:", e$message), warnings = NULL))
  })
}

#' Shiny-safe plot wrapper for PK parameter box plots
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameters Vector of PK parameters to plot (default: c("AUC0t", "Cmax"))
#' @param log_scale Logical, whether to use log scale (default TRUE)
#' @param show_individual_points Logical, whether to overlay individual points (default TRUE)
#' @param interactive Logical, whether to create interactive plotly plot (default TRUE)
#' @param height Plot height in pixels for Shiny (default 400)
#' @return List with plot object and any error/warning messages
#' @export
shiny_plot_pk_boxplots <- function(data, parameters = c("AUC0t", "Cmax"), 
                                  log_scale = TRUE, show_individual_points = TRUE, 
                                  interactive = TRUE, height = 400) {
  tryCatch({
    # Standardize column names first
    data <- standardize_column_names(data)
    
    # Validate parameters exist in data
    missing_params <- setdiff(parameters, names(data))
    if (length(missing_params) > 0) {
      return(list(plot = NULL, error = paste("Parameters not found in data:", paste(missing_params, collapse = ", ")), warnings = NULL))
    }
    
    # Check if we have a Formulation column after standardization
    if (!"Formulation" %in% names(data)) {
      return(list(plot = NULL, error = "No treatment/formulation column found in data. Expected one of: Formulation, Treatment, Trt, Group", warnings = NULL))
    }
    
    # Create the plot
    plot_obj <- plot_pk_boxplots(data, parameters, log_scale, show_individual_points, interactive)
    
    # Configure for Shiny output
    if (interactive && inherits(plot_obj, "plotly")) {
      plot_obj <- configure_plotly_for_shiny(plot_obj, height)
    }
    
    return(list(plot = plot_obj, error = NULL, warnings = NULL))
    
  }, error = function(e) {
    return(list(plot = NULL, error = paste("Plot generation failed:", e$message), warnings = NULL))
  })
}

#' Shiny-safe plot wrapper for forest plots
#'
#' @param be_results Results from perform_be_analysis
#' @param parameters Vector of parameters to include (default: all available)
#' @param be_limits Bioequivalence limits (default: c(0.8, 1.25))
#' @param interactive Logical, whether to create interactive plotly plot (default TRUE)
#' @param height Plot height in pixels for Shiny (default 400)
#' @return List with plot object and any error/warning messages
#' @export
shiny_plot_forest_be <- function(be_results, parameters = NULL, be_limits = c(0.8, 1.25), 
                                interactive = TRUE, height = 400) {
  tryCatch({
    if (!inherits(be_results, "bioeq")) {
      return(list(plot = NULL, error = "Input must be a bioeq object from perform_be_analysis", warnings = NULL))
    }
    
    # Create the plot
    plot_obj <- plot_forest_be(be_results, parameters, be_limits, interactive)
    
    # Configure for Shiny output
    if (interactive && inherits(plot_obj, "plotly")) {
      plot_obj <- configure_plotly_for_shiny(plot_obj, height)
    }
    
    return(list(plot = plot_obj, error = NULL, warnings = NULL))
    
  }, error = function(e) {
    return(list(plot = NULL, error = paste("Plot generation failed:", e$message), warnings = NULL))
  })
}

#' Create interactive plot for selected individual subjects only
#' 
#' @param data Data frame with concentration-time data
#' @param selected_subjects Vector of subject IDs to display
#' @param log_scale Logical, whether to use log scale for concentration (default FALSE)
#' @export
create_individual_concentration_plot <- function(data, selected_subjects = NULL, log_scale = FALSE) {
  tryCatch({
    # Validate data
    validation <- validate_conc_time_data(data, silent = TRUE)
    
    # If selected_subjects is NULL, use all subjects
    if (is.null(selected_subjects)) {
      selected_subjects <- unique(data$Subject)
    }
    
    # Filter data for selected subjects
    plot_data <- data[data$Subject %in% selected_subjects, ]
    
    if (nrow(plot_data) == 0) {
      stop("No data available for selected subjects")
    }
    
    # Start with empty plotly object
    p <- plot_ly()
    
    # Add individual profiles for each selected subject
    for (subj in unique(plot_data$Subject)) {
      for (form in unique(plot_data$Formulation)) {
        subj_data <- plot_data[plot_data$Subject == subj & plot_data$Formulation == form, ]
        if (nrow(subj_data) > 0) {
          color_val <- if (form == "Reference") "#E31A1C" else "#1F78B4"
          
          p <- p %>% add_trace(
            x = subj_data$Time, 
            y = subj_data$Concentration,
            type = "scatter", mode = "lines+markers",
            line = list(color = color_val, width = 2),
            marker = list(color = color_val, size = 6),
            opacity = 0.8,
            name = paste("Subject", subj, "-", form),
            hovertemplate = paste(
              "<b>Subject:</b>", subj, "<br>",
              "<b>Formulation:</b>", form, "<br>",
              "<b>Time:</b> %{x}<br>",
              "<b>Concentration:</b> %{y}<br>",
              "<extra></extra>"
            )
          )
        }
      }
    }
    
    # Configure plot layout
    y_title <- if (log_scale) "Concentration (log scale)" else "Concentration"
    y_type <- if (log_scale) "log" else "linear"
    
    p <- p %>% layout(
      title = list(
        text = paste("Individual Subject Profiles (n =", length(unique(plot_data$Subject)), "subjects)"),
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = list(text = "Time", font = list(size = 14)),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      yaxis = list(
        title = list(text = y_title, font = list(size = 14)),
        type = y_type,
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "#bdc3c7",
        borderwidth = 1
      ),
      margin = list(r = 150)
    )
    
    # Configure for Shiny
    p <- configure_plotly_for_shiny(p, height = 500, show_mode_bar = TRUE)
    
    return(p)
    
  }, error = function(e) {
    cat("Error in create_individual_concentration_plot:", e$message, "\n")
    stop(paste("Failed to create individual concentration plot:", e$message))
  })
}

# =============================================================================
# END SHINY-OPTIMIZED FUNCTIONS
# =============================================================================

# Load required packages for interactive plotting
if (!require("plotly", quietly = TRUE)) {
  message("Installing plotly package for interactive plots...")
  install.packages("plotly", dependencies = TRUE)
  library(plotly)
}

#' Export plot to specified format
#'
#' @param plot_object ggplot2 or plotly object
#' @param filename Output filename (with or without extension)
#' @param format Output format: "png", "pdf", "html", "auto"
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution for raster formats
#' @export
export_plot <- function(plot_object, filename, format = "auto", 
                       width = 10, height = 6, dpi = 300) {
  # Determine format from filename if auto
  if (format == "auto") {
    ext <- tools::file_ext(filename)
    format <- if (ext %in% c("png", "pdf", "html")) ext else "png"
  }
  
  # Ensure filename has correct extension
  if (!grepl(paste0("\\.", format, "$"), filename)) {
    filename <- paste0(tools::file_path_sans_ext(filename), ".", format)
  }
  
  # Create directory if it doesn't exist
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  if (inherits(plot_object, "plotly")) {
    # Handle plotly objects
    if (format == "html") {
      tryCatch({
        htmlwidgets::saveWidget(plot_object, filename, 
                               selfcontained = TRUE, 
                               libdir = NULL)
      }, error = function(e) {
        if (grepl("pandoc", e$message, ignore.case = TRUE)) {
          warning("Pandoc not available. Saving as non-self-contained HTML.")
          htmlwidgets::saveWidget(plot_object, filename, 
                                 selfcontained = FALSE, 
                                 libdir = paste0(tools::file_path_sans_ext(filename), "_files"))
        } else {
          stop(e)
        }
      })
    } else {
      # Convert plotly to static format via orca or kaleido
      tryCatch({
        plotly::save_image(plot_object, filename, 
                          width = width * dpi, height = height * dpi)
      }, error = function(e) {
        warning("Could not save plotly as static image. Install orca or kaleido. Saving as HTML instead.")
        html_file <- paste0(tools::file_path_sans_ext(filename), ".html")
        tryCatch({
          htmlwidgets::saveWidget(plot_object, html_file, 
                                 selfcontained = TRUE, libdir = NULL)
        }, error = function(e2) {
          if (grepl("pandoc", e2$message, ignore.case = TRUE)) {
            warning("Pandoc not available. Saving as non-self-contained HTML.")
            htmlwidgets::saveWidget(plot_object, html_file, 
                                   selfcontained = FALSE, 
                                   libdir = paste0(tools::file_path_sans_ext(html_file), "_files"))
          } else {
            stop(e2)
          }
        })
      })
    }
  } else if (inherits(plot_object, "ggplot")) {
    # Handle ggplot2 objects
    if (format == "html") {
      # Convert ggplot to plotly then save as HTML
      p_ly <- ggplotly(plot_object)
      tryCatch({
        htmlwidgets::saveWidget(p_ly, filename, 
                               selfcontained = TRUE, libdir = NULL)
      }, error = function(e) {
        if (grepl("pandoc", e$message, ignore.case = TRUE)) {
          warning("Pandoc not available. Saving as non-self-contained HTML.")
          htmlwidgets::saveWidget(p_ly, filename, 
                                 selfcontained = FALSE, 
                                 libdir = paste0(tools::file_path_sans_ext(filename), "_files"))
        } else {
          stop(e)
        }
      })
    } else {
      ggsave(filename, plot_object, width = width, height = height, dpi = dpi)
    }
  } else {
    stop("plot_object must be a ggplot2 or plotly object")
  }
  
  message("Plot saved: ", filename)
  return(invisible(filename))
}

#' Plot concentration-time profile
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @param interactive Logical, whether to create interactive plotly plot
#' @export
plot_concentration_time <- function(data, log_scale = FALSE, individual = TRUE, 
                                   mean_profile = TRUE, interactive = FALSE) {
  if (interactive) {
    return(plot_concentration_time_interactive(data, log_scale, individual, mean_profile))
  } else {
    return(plot_concentration_time_static(data, log_scale, individual, mean_profile))
  }
}

#' Plot concentration-time profile (static ggplot2 version)
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @export
plot_concentration_time_static <- function(data, log_scale = FALSE, individual = TRUE, mean_profile = TRUE) {
  # Use silent validation for better Shiny integration
  validation <- validate_conc_time_data(data, silent = FALSE)  # Keep original behavior for backward compatibility
  
  p <- ggplot(data, aes(x = Time, y = Concentration, color = Formulation))
  
  if (individual) {
    p <- p + geom_line(aes(group = interaction(Subject, Formulation)), alpha = 0.3)
    p <- p + geom_point(aes(group = interaction(Subject, Formulation)), alpha = 0.3, size = 0.8)
  }
  
  if (mean_profile) {
    mean_data <- data %>%
      group_by(Time, Formulation) %>%
      summarise(
        Mean_Conc = mean(Concentration, na.rm = TRUE),
        SE = sd(Concentration, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      )
    
    p <- p + geom_line(data = mean_data, aes(x = Time, y = Mean_Conc), linewidth = 1.2)
    p <- p + geom_point(data = mean_data, aes(x = Time, y = Mean_Conc), size = 2)
    p <- p + geom_errorbar(data = mean_data, 
                          aes(x = Time, y = Mean_Conc, 
                              ymin = Mean_Conc - SE, ymax = Mean_Conc + SE),
                          width = 0.1, alpha = 0.7)
  }
  
  if (log_scale) {
    p <- p + scale_y_log10()
    p <- p + labs(y = "Concentration (log scale)")
  } else {
    p <- p + labs(y = "Concentration")
  }
  
  p <- p + labs(x = "Time", title = "Concentration-Time Profiles") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_brewer(type = "qual", palette = "Set1")
  
  return(p)
}

#' Plot concentration-time profile (interactive plotly version)
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @export
plot_concentration_time_interactive <- function(data, log_scale = FALSE, individual = TRUE, mean_profile = TRUE) {
  # Use silent validation for better Shiny integration
  validation <- validate_conc_time_data(data, silent = FALSE)  # Keep original behavior for backward compatibility
  
  # Start with empty plotly object
  p <- plot_ly()
  
  if (individual) {
    # Add individual profiles with hover information
    for (subj in unique(data$Subject)) {
      for (form in unique(data$Formulation)) {
        subj_data <- data[data$Subject == subj & data$Formulation == form, ]
        if (nrow(subj_data) > 0) {
          color_val <- if (form == "Reference") "#E31A1C" else "#1F78B4"
          
          p <- p %>% add_trace(
            x = subj_data$Time, 
            y = subj_data$Concentration,
            type = "scatter", mode = "lines+markers",
            line = list(color = color_val, width = 1),
            marker = list(color = color_val, size = 4),
            opacity = 0.4,
            name = paste(form, "- Subject", subj),
            legendgroup = form,
            showlegend = subj == unique(data$Subject)[1],  # Show legend only for first subject
            hovertemplate = paste(
              "<b>Subject:</b>", subj, "<br>",
              "<b>Formulation:</b>", form, "<br>",
              "<b>Time:</b> %{x}<br>",
              "<b>Concentration:</b> %{y}<br>",
              "<extra></extra>"
            )
          )
        }
      }
    }
  }
  
  if (mean_profile) {
    # Calculate mean data
    mean_data <- data %>%
      group_by(Time, Formulation) %>%
      summarise(
        Mean_Conc = mean(Concentration, na.rm = TRUE),
        SE = sd(Concentration, na.rm = TRUE) / sqrt(n()),
        N = n(),
        .groups = 'drop'
      )
    
    # Add mean profiles
    for (form in unique(mean_data$Formulation)) {
      form_data <- mean_data[mean_data$Formulation == form, ]
      color_val <- if (form == "Reference") "#E31A1C" else "#1F78B4"
      
      # Add error bars
      p <- p %>% add_trace(
        x = form_data$Time, 
        y = form_data$Mean_Conc,
        error_y = list(
          type = "data",
          array = form_data$SE,
          color = color_val,
          thickness = 2,
          width = 3
        ),
        type = "scatter", mode = "lines+markers",
        line = list(color = color_val, width = 3),
        marker = list(color = color_val, size = 8),
        name = paste("Mean", form),
        legendgroup = paste("mean", form),
        hovertemplate = paste(
          "<b>Formulation:</b>", form, "<br>",
          "<b>Time:</b> %{x}<br>",
          "<b>Mean Concentration:</b> %{y:.3f}<br>",
          "<b>SE:</b> %{text}<br>",
          "<b>N:</b> %{text}<br>",
          "<extra></extra>"
        ),
        text = paste("SE:", round(form_data$SE, 3), "| N:", form_data$N)
      )
    }
  }
  
  # Configure layout
  y_title <- if (log_scale) "Concentration (log scale)" else "Concentration"
  
  p <- p %>% layout(
    title = list(
      text = "Concentration-Time Profiles",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Time",
      showgrid = TRUE,
      gridcolor = 'rgba(128,128,128,0.2)'
    ),
    yaxis = list(
      title = y_title,
      type = if (log_scale) "log" else "linear",
      showgrid = TRUE,
      gridcolor = 'rgba(128,128,128,0.2)'
    ),
    hovermode = "closest",
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    ),
    plot_bgcolor = 'rgba(0,0,0,0)',
    paper_bgcolor = 'rgba(0,0,0,0)'
  )
  
  return(p)
}

#' Plot bioequivalence analysis results
#'
#' @param be_results Results from perform_be_analysis
#' @export
plot_be_results <- function(be_results) {
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  # Extract PK parameters
  pk_data <- be_results$pk_parameters
  
  # Create plots for each parameter
  plots <- list()
  
  for (param in c("AUC0t", "AUC0inf", "Cmax", "Tmax")) {
    if (param %in% names(pk_data)) {
      plot_data <- pk_data %>%
        select(Subject, Period, Formulation, all_of(param)) %>%
        rename(Value = all_of(param))
      
      p <- ggplot(plot_data, aes(x = Formulation, y = Value, fill = Formulation)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
        labs(title = paste("Individual", param, "Values"),
             y = param, x = "Formulation") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_brewer(type = "qual", palette = "Set2")
      
      if (param != "Tmax") {
        p <- p + scale_y_log10() + labs(y = paste(param, "(log scale)"))
      }
      
      plots[[param]] <- p
    }
  }
  
  return(plots)
}

#' Plot confidence intervals for bioequivalence
#'
#' @param be_results Results from perform_be_analysis
#' @param interactive Logical, whether to create interactive plotly plot
#' @export
plot_be_confidence_intervals <- function(be_results, interactive = FALSE) {
  if (interactive) {
    return(plot_be_confidence_intervals_interactive(be_results))
  } else {
    return(plot_be_confidence_intervals_static(be_results))
  }
}

#' Plot confidence intervals for bioequivalence (static version)
#'
#' @param be_results Results from perform_be_analysis
#' @export
plot_be_confidence_intervals_static <- function(be_results) {
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  ci_data <- be_results$confidence_intervals
  
  # Check if there are any confidence intervals to plot
  if (is.null(ci_data) || length(ci_data) == 0) {
    stop("No confidence intervals available for plotting")
  }
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Parameter = names(ci_data),
    Lower = sapply(ci_data, function(x) x$ci_lower),
    Upper = sapply(ci_data, function(x) x$ci_upper),
    Estimate = sapply(ci_data, function(x) x$point_estimate),
    stringsAsFactors = FALSE
  )
  
  # Add bioequivalence limits
  plot_data$BE_Lower <- 80
  plot_data$BE_Upper <- 125
  
  p <- ggplot(plot_data, aes(x = Parameter, y = Estimate)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1) +
    geom_hline(yintercept = c(80, 125), linetype = "dashed", color = "red", alpha = 0.7) +
    geom_hline(yintercept = 100, linetype = "solid", color = "gray", alpha = 0.5) +
    labs(title = "90% Confidence Intervals for Bioequivalence",
         subtitle = "Red dashed lines show bioequivalence limits (80-125%)",
         y = "Ratio (%) with 90% CI", x = "PK Parameter") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(70, 140))
  
  return(p)
}

#' Plot confidence intervals for bioequivalence (interactive version)
#'
#' @param be_results Results from perform_be_analysis
#' @export
plot_be_confidence_intervals_interactive <- function(be_results) {
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  ci_data <- be_results$confidence_intervals
  
  # Check if there are any confidence intervals to plot
  if (is.null(ci_data) || length(ci_data) == 0) {
    stop("No confidence intervals available for plotting")
  }
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Parameter = names(ci_data),
    Lower = sapply(ci_data, function(x) x$ci_lower),
    Upper = sapply(ci_data, function(x) x$ci_upper),
    Estimate = sapply(ci_data, function(x) x$point_estimate),
    stringsAsFactors = FALSE
  )
  
  # Add bioequivalence limits
  plot_data$BE_Lower <- 80
  plot_data$BE_Upper <- 125
  plot_data$BE_Status <- ifelse(plot_data$Lower >= 80 & plot_data$Upper <= 125, 
                                "Bioequivalent", "Not Bioequivalent")
  
  # Create plotly plot
  p <- plot_ly(x = plot_data$Parameter, y = plot_data$Estimate) %>%
    
    # Add confidence interval error bars
    add_trace(
      type = "scatter", mode = "markers",
      marker = list(
        size = 12,
        color = ifelse(plot_data$BE_Status == "Bioequivalent", "blue", "red"),
        line = list(width = 2, color = "white")
      ),
      error_y = list(
        type = "data",
        symmetric = FALSE,
        array = (plot_data$Upper - plot_data$Estimate),
        arrayminus = (plot_data$Estimate - plot_data$Lower),
        thickness = 3,
        width = 8,
        color = ifelse(plot_data$BE_Status == "Bioequivalent", "blue", "red")
      ),
      name = "90% CI",
      hovertemplate = paste(
        "<b>Parameter:</b> %{x}<br>",
        "<b>Point Estimate:</b> %{y:.2f}%<br>",
        "<b>90% CI:</b> [%{text}]<br>",
        "<b>Status:</b> %{text}<br>",
        "<extra></extra>"
      ),
      text = paste(
        round(plot_data$Lower, 2), "%, ", round(plot_data$Upper, 2), "%]<br><b>Status:</b> ", 
        plot_data$BE_Status, sep = ""
      )
    ) %>%
    
    # Add bioequivalence limit lines using add_lines instead of add_hline
    add_lines(x = plot_data$Parameter, y = rep(80, nrow(plot_data)), 
             line = list(color = "red", dash = "dash", width = 2),
             name = "BE Lower Limit (80%)", showlegend = FALSE,
             hovertemplate = "BE Lower Limit: 80%<extra></extra>") %>%
    add_lines(x = plot_data$Parameter, y = rep(125, nrow(plot_data)), 
             line = list(color = "red", dash = "dash", width = 2),
             name = "BE Upper Limit (125%)", showlegend = FALSE,
             hovertemplate = "BE Upper Limit: 125%<extra></extra>") %>%
    add_lines(x = plot_data$Parameter, y = rep(100, nrow(plot_data)), 
             line = list(color = "gray", dash = "solid", width = 1),
             name = "Unity (100%)", showlegend = FALSE,
             hovertemplate = "Unity: 100%<extra></extra>") %>%
    
    layout(
      title = list(
        text = "90% Confidence Intervals for Bioequivalence",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "PK Parameter",
        tickangle = 45
      ),
      yaxis = list(
        title = "Ratio (%) with 90% CI",
        range = c(70, 140)
      ),
      hovermode = "closest",
      showlegend = FALSE,
      annotations = list(
        list(
          x = 0.02, y = 0.98,
          xref = "paper", yref = "paper",
          text = "Red dashed lines: BE limits (80-125%)",
          showarrow = FALSE,
          font = list(size = 10, color = "gray")
        )
      )
    )
  
  return(p)
}

#' Plot sample size curves
#'
#' @param cv_range Range of CV values to plot
#' @param power Target power (default 0.8)
#' @param theta0 True ratio (default 0.95)
#' @param alpha Significance level (default 0.05)
#' @export
plot_sample_size_curve <- function(cv_range = seq(0.1, 0.5, 0.01), 
                                  power = 0.8, theta0 = 0.95, alpha = 0.05) {
  
  sample_sizes <- sapply(cv_range, function(cv) {
    result <- calculate_sample_size(cv = cv, power = power, theta0 = theta0, alpha = alpha)
    # Handle different return structures for different designs
    if (!is.null(result$n_subjects)) {
      return(result$n_subjects)  # For crossover designs
    } else if (!is.null(result$n_per_group)) {
      return(result$n_per_group * 2)  # For parallel designs (total sample size)
    } else if (!is.null(result$n_total)) {
      return(result$n_total)  # Alternative total sample size field
    } else {
      return(NA)  # Fallback if structure is unexpected
    }
  })
  
  plot_data <- data.frame(
    CV = cv_range * 100,  # Convert to percentage
    SampleSize = sample_sizes
  )
  
  p <- ggplot(plot_data, aes(x = CV, y = SampleSize)) +
    geom_line(linewidth = 1, color = "blue") +
    geom_point(alpha = 0.6, color = "blue") +
    labs(title = paste("Sample Size Requirements for", power * 100, "% Power"),
         subtitle = paste("θ₀ =", theta0, ", α =", alpha),
         x = "Coefficient of Variation (%)", 
         y = "Total Sample Size") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(10, 50, 5)) +
    scale_y_continuous(breaks = seq(0, max(sample_sizes, na.rm = TRUE), 10))
  
  return(p)
}

#' Plot power curves
#'
#' @param n_per_group Sample size per group
#' @param cv Coefficient of variation
#' @param theta_range Range of true ratios to plot
#' @param alpha Significance level (default 0.05)
#' @export
plot_power_curve <- function(n_per_group, cv, theta_range = seq(0.8, 1.25, 0.01), alpha = 0.05) {
  
  powers <- sapply(theta_range, function(theta) {
    result <- calculate_power(n_per_group = n_per_group, cv = cv, theta0 = theta, alpha = alpha)
    return(result$power)
  })
  
  plot_data <- data.frame(
    TrueRatio = theta_range,
    Power = powers
  )
  
  p <- ggplot(plot_data, aes(x = TrueRatio, y = Power)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_vline(xintercept = c(0.8, 1.25), linetype = "dashed", color = "blue", alpha = 0.7) +
    labs(title = paste("Power Curve (n =", n_per_group, "per group, CV =", cv * 100, "%)"),
         subtitle = "Red line: 80% power, Blue lines: bioequivalence limits",
         x = "True Test/Reference Ratio", 
         y = "Power") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0.8, 1.25, 0.05)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent)
  
  return(p)
}

#' Create diagnostic plots for bioequivalence analysis
#'
#' @param be_results Results from perform_be_analysis
#' @export
plot_be_diagnostics <- function(be_results) {
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  pk_data <- be_results$pk_parameters
  plots <- list()
  
  # Residual plots for each parameter
  for (param in c("AUC0t", "AUC0inf", "Cmax")) {
    if (param %in% names(pk_data)) {
      # Create a simple linear model for residuals
      formula_str <- paste("log(", param, ") ~ Formulation + Subject + Period")
      model_data <- pk_data[!is.na(pk_data[[param]]) & pk_data[[param]] > 0, ]
      
      if (nrow(model_data) > 0) {
        model <- lm(as.formula(formula_str), data = model_data)
        
        diag_data <- data.frame(
          Fitted = fitted(model),
          Residuals = residuals(model),
          Parameter = param
        )
        
        p <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          labs(title = paste("Residual Plot -", param),
               x = "Fitted Values", y = "Residuals") +
          theme_minimal()
        
        plots[[paste0(param, "_residuals")]] <- p
      }
    }
  }
  
  return(plots)
}

#' Generate a comprehensive bioequivalence report plot
#'
#' @param be_results Results from perform_be_analysis
#' @export
plot_be_summary <- function(be_results) {
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  # Create multiple plots and arrange them
  ci_plot <- plot_be_confidence_intervals(be_results)
  be_plots <- plot_be_results(be_results)
  
  # Return list of plots for manual arrangement
  summary_plots <- list(
    confidence_intervals = ci_plot,
    parameter_distributions = be_plots
  )
  
  return(summary_plots)
}

#' Plot individual subject test vs reference comparison
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param facet_wrap Logical, whether to use facet_wrap for individual subjects
#' @param ncol Number of columns for faceting (default 4)
#' @param interactive Logical, whether to create interactive plotly plot
#' @export
plot_subject_comparison <- function(data, log_scale = FALSE, facet_wrap = TRUE, ncol = 4, interactive = FALSE) {
  if (interactive) {
    return(plot_subject_comparison_interactive(data, log_scale, facet_wrap, ncol))
  } else {
    return(plot_subject_comparison_static(data, log_scale, facet_wrap, ncol))
  }
}

#' Plot individual subject test vs reference comparison (static version)
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param facet_wrap Logical, whether to use facet_wrap for individual subjects
#' @param ncol Number of columns for faceting (default 4)
#' @export
plot_subject_comparison_static <- function(data, log_scale = FALSE, facet_wrap = TRUE, ncol = 4) {
  validate_conc_time_data(data)
  
  # Ensure we have both Test and Reference data
  if (!all(c("Test", "Reference") %in% data$Formulation)) {
    stop("Data must contain both 'Test' and 'Reference' formulations")
  }
  
  p <- ggplot(data, aes(x = Time, y = Concentration, color = Formulation)) +
    geom_line(aes(group = interaction(Subject, Formulation)), linewidth = 0.8) +
    geom_point(aes(group = interaction(Subject, Formulation)), size = 1.2) +
    scale_color_manual(values = c("Reference" = "#E31A1C", "Test" = "#1F78B4"),
                       labels = c("Reference", "Test")) +
    labs(x = "Time", 
         title = "Individual Subject Comparison: Test vs Reference",
         subtitle = "Concentration-Time Profiles by Subject") +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8, face = "bold"))
  
  if (facet_wrap) {
    p <- p + facet_wrap(~ paste("Subject", Subject), ncol = ncol, scales = "free_y")
  }
  
  if (log_scale) {
    p <- p + scale_y_log10() + labs(y = "Concentration (log scale)")
  } else {
    p <- p + labs(y = "Concentration")
  }
  
  return(p)
}

#' Plot individual subject test vs reference comparison (interactive version)
#'
#' @param data Data frame with Time, Concentration, Subject, and Formulation columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param facet_wrap Logical, whether to use facet_wrap for individual subjects
#' @param ncol Number of columns for faceting (default 4)
#' @export
plot_subject_comparison_interactive <- function(data, log_scale = FALSE, facet_wrap = TRUE, ncol = 4) {
  validate_conc_time_data(data)
  
  # Ensure we have both Test and Reference data
  if (!all(c("Test", "Reference") %in% data$Formulation)) {
    stop("Data must contain both 'Test' and 'Reference' formulations")
  }
  
  if (facet_wrap) {
    # For now, create a single plot with all subjects but with text annotations to identify subjects
    # This avoids complex subplot issues while maintaining functionality
    p <- plot_ly()
    
    for (subj in unique(data$Subject)) {
      for (form in unique(data$Formulation)) {
        subj_data <- data[data$Subject == subj & data$Formulation == form, ]
        if (nrow(subj_data) > 0) {
          color_val <- if (form == "Reference") "#E31A1C" else "#1F78B4"
          
          p <- p %>% add_trace(
            x = subj_data$Time, 
            y = subj_data$Concentration,
            type = "scatter", mode = "lines+markers",
            line = list(color = color_val, width = 1.5),
            marker = list(color = color_val, size = 5),
            name = paste(form, "- Subject", subj),
            legendgroup = form,
            showlegend = (subj == unique(data$Subject)[1] && form %in% c("Reference", "Test")),
            hovertemplate = paste(
              "<b>Subject:</b>", subj, "<br>",
              "<b>Formulation:</b>", form, "<br>",
              "<b>Time:</b> %{x}<br>",
              "<b>Concentration:</b> %{y}<br>",
              "<extra></extra>"
            )
          )
        }
      }
    }
    
    p <- p %>% layout(
      title = list(
        text = "Individual Subject Comparison: Test vs Reference",
        font = list(size = 16)
      ),
      xaxis = list(title = "Time"),
      yaxis = list(
        title = if (log_scale) "Concentration (log scale)" else "Concentration",
        type = if (log_scale) "log" else "linear"
      ),
      hovermode = "closest",
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1
      )
    )
    
  } else {
    # Single plot with all subjects
    p <- plot_ly()
    
    for (subj in unique(data$Subject)) {
      for (form in unique(data$Formulation)) {
        subj_data <- data[data$Subject == subj & data$Formulation == form, ]
        if (nrow(subj_data) > 0) {
          color_val <- if (form == "Reference") "#E31A1C" else "#1F78B4"
          
          p <- p %>% add_trace(
            x = subj_data$Time, 
            y = subj_data$Concentration,
            type = "scatter", mode = "lines+markers",
            line = list(color = color_val, width = 1),
            marker = list(color = color_val, size = 4),
            name = paste(form, "- Subject", subj),
            legendgroup = form,
            showlegend = subj == unique(data$Subject)[1],
            hovertemplate = paste(
              "<b>Subject:</b>", subj, "<br>",
              "<b>Formulation:</b>", form, "<br>",
              "<b>Time:</b> %{x}<br>",
              "<b>Concentration:</b> %{y}<br>",
              "<extra></extra>"
            )
          )
        }
      }
    }
    
    p <- p %>% layout(
      title = list(
        text = "Individual Subject Comparison: Test vs Reference",
        font = list(size = 16)
      ),
      xaxis = list(title = "Time"),
      yaxis = list(
        title = if (log_scale) "Concentration (log scale)" else "Concentration",
        type = if (log_scale) "log" else "linear"
      ),
      hovermode = "closest",
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1
      )
    )
  }
  
  return(p)
}

#' Plot bioavailability ratios for each subject
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameter PK parameter to plot ("AUC0t", "AUC0inf", "Cmax")
#' @export
plot_subject_ratios <- function(data, parameter = "AUC0t") {
  if (!parameter %in% names(data)) {
    stop("Parameter '", parameter, "' not found in data")
  }
  
  # Calculate ratios (Test/Reference) for each subject
  wide_data <- data %>%
    select(Subject, Formulation, all_of(parameter)) %>%
    reshape2::dcast(Subject ~ Formulation, value.var = parameter) %>%
    mutate(Ratio = Test / Reference,
           Ratio_Percent = Ratio * 100)
  
  # Remove subjects with missing data
  wide_data <- wide_data[complete.cases(wide_data), ]
  
  if (nrow(wide_data) == 0) {
    stop("No complete data pairs found for ratio calculation")
  }
  
  # Create the plot
  p <- ggplot(wide_data, aes(x = factor(Subject), y = Ratio_Percent)) +
    geom_hline(yintercept = 100, linetype = "solid", color = "gray50", alpha = 0.7) +
    geom_hline(yintercept = c(80, 125), linetype = "dashed", color = "red", alpha = 0.7) +
    geom_point(size = 3, color = "#1F78B4", alpha = 0.8) +
    geom_segment(aes(xend = factor(Subject), y = 100, yend = Ratio_Percent), 
                 color = "#1F78B4", alpha = 0.6, linewidth = 0.8) +
    labs(x = "Subject", 
         y = paste("Test/Reference Ratio (%) for", parameter),
         title = paste("Individual Subject Ratios:", parameter),
         subtitle = "Red dashed lines show bioequivalence limits (80-125%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(min(70, min(wide_data$Ratio_Percent) * 0.95), 
                            max(130, max(wide_data$Ratio_Percent) * 1.05)))
  
  return(p)
}

#' Plot test vs reference scatter plot with identity line
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters  
#' @param parameter PK parameter to plot ("AUC0t", "AUC0inf", "Cmax")
#' @param log_scale Logical, whether to use log scale
#' @export
plot_test_vs_reference <- function(data, parameter = "AUC0t", log_scale = TRUE) {
  if (!parameter %in% names(data)) {
    stop("Parameter '", parameter, "' not found in data")
  }
  
  # Create wide format for scatter plot
  wide_data <- data %>%
    select(Subject, Formulation, all_of(parameter)) %>%
    reshape2::dcast(Subject ~ Formulation, value.var = parameter)
  
  # Remove subjects with missing data
  wide_data <- wide_data[complete.cases(wide_data), ]
  
  if (nrow(wide_data) == 0) {
    stop("No complete data pairs found for scatter plot")
  }
  
  # Calculate the range for axes
  if (log_scale) {
    all_values <- c(wide_data$Test, wide_data$Reference)
    axis_min <- min(all_values, na.rm = TRUE) * 0.8
    axis_max <- max(all_values, na.rm = TRUE) * 1.2
  } else {
    all_values <- c(wide_data$Test, wide_data$Reference)
    axis_min <- min(all_values, na.rm = TRUE) * 0.9
    axis_max <- max(all_values, na.rm = TRUE) * 1.1
  }
  
  p <- ggplot(wide_data, aes(x = Reference, y = Test)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
    geom_point(size = 3, alpha = 0.7, color = "#1F78B4") +
    geom_text(aes(label = Subject), vjust = -0.5, hjust = 0.5, size = 3, alpha = 0.8) +
    labs(title = paste("Test vs Reference:", parameter),
         subtitle = "Red dashed line represents perfect agreement (Test = Reference)",
         x = paste("Reference", parameter),
         y = paste("Test", parameter)) +
    theme_minimal() +
    theme(aspect.ratio = 1)
  
  if (log_scale) {
    p <- p + 
      scale_x_log10(limits = c(axis_min, axis_max)) +
      scale_y_log10(limits = c(axis_min, axis_max)) +
      labs(x = paste("Reference", parameter, "(log scale)"),
           y = paste("Test", parameter, "(log scale)"))
  } else {
    p <- p + 
      coord_cartesian(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max))
  }
  
  return(p)
}

#' Plot individual subject T/R ratios with cumulative trend and BE limits
#'
#' Creates a comprehensive plot showing individual subject T/R ratios as scatter points,
#' connected by lines, with a cumulative trend line showing the overall T/R ratio
#' as each subject is analyzed sequentially, plus bioequivalence limit lines.
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameter PK parameter to plot ("AUC0t", "AUC0inf", "Cmax")
#' @param subject_order Optional vector specifying the order of subjects for analysis.
#'                      If NULL, subjects are ordered by their Subject ID (1, 2, 3, ...)
#' @param be_limits Bioequivalence limits (default c(0.8, 1.25))
#' @param interactive Logical, whether to create interactive plot (default FALSE)
#' @export
plot_subject_tr_ratios_with_trend <- function(data, parameter = "AUC0t", 
                                             subject_order = NULL,
                                             be_limits = c(0.8, 1.25),
                                             interactive = FALSE) {
  
  if (!parameter %in% names(data)) {
    stop("Parameter '", parameter, "' not found in data")
  }
  
  # Calculate ratios (Test/Reference) for each subject
  wide_data <- data %>%
    select(Subject, Formulation, all_of(parameter)) %>%
    reshape2::dcast(Subject ~ Formulation, value.var = parameter) %>%
    mutate(Ratio = Test / Reference,
           Ratio_Percent = Ratio * 100)
  
  # Remove subjects with missing data
  wide_data <- wide_data[complete.cases(wide_data), ]
  
  if (nrow(wide_data) == 0) {
    stop("No complete data pairs found for ratio calculation")
  }
  
  # Order subjects according to specified order or default (Subject ID)
  if (is.null(subject_order)) {
    wide_data <- wide_data[order(wide_data$Subject), ]
  } else {
    # Validate subject_order
    missing_subjects <- setdiff(subject_order, wide_data$Subject)
    if (length(missing_subjects) > 0) {
      warning("Subjects not found in data: ", paste(missing_subjects, collapse = ", "))
    }
    valid_order <- intersect(subject_order, wide_data$Subject)
    remaining_subjects <- setdiff(wide_data$Subject, valid_order)
    final_order <- c(valid_order, sort(remaining_subjects))
    wide_data <- wide_data[match(final_order, wide_data$Subject), ]
  }
  
  # Add analysis order
  wide_data$Analysis_Order <- 1:nrow(wide_data)
  
  # Calculate cumulative T/R ratios (geometric means)
  wide_data$Cumulative_Test <- cumsum(log(wide_data$Test))
  wide_data$Cumulative_Ref <- cumsum(log(wide_data$Reference))
  wide_data$Cumulative_Ratio <- exp((wide_data$Cumulative_Test - wide_data$Cumulative_Ref) / wide_data$Analysis_Order) * 100
  
  # Convert BE limits to percentage
  be_lower <- be_limits[1] * 100
  be_upper <- be_limits[2] * 100
  
  # Calculate dynamic y-axis limits based on actual data range
  all_y_values <- c(wide_data$Ratio_Percent, wide_data$Cumulative_Ratio, be_lower, be_upper, 100)
  y_min <- min(all_y_values, na.rm = TRUE)
  y_max <- max(all_y_values, na.rm = TRUE)
  y_range <- y_max - y_min
  
  # Add 10% padding on each side, with minimum range of 20%
  y_padding <- max(y_range * 0.1, 10)
  y_limit_min <- y_min - y_padding
  y_limit_max <- y_max + y_padding
  
  if (interactive) {
    # Create interactive plotly version
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("plotly package is required for interactive plots. Install with: install.packages('plotly')")
    }
    
    p <- plotly::plot_ly() %>%
      # Add BE limit lines first (background)
      plotly::add_lines(
        x = c(0.5, nrow(wide_data) + 0.5), 
        y = c(be_lower, be_lower),
        line = list(color = "red", dash = "dash", width = 2),
        name = paste0("BE Lower Limit (", be_lower, "%)"),
        hovertemplate = paste0("BE Lower Limit: ", be_lower, "%<extra></extra>")
      ) %>%
      plotly::add_lines(
        x = c(0.5, nrow(wide_data) + 0.5), 
        y = c(be_upper, be_upper),
        line = list(color = "red", dash = "dash", width = 2),
        name = paste0("BE Upper Limit (", be_upper, "%)"),
        hovertemplate = paste0("BE Upper Limit: ", be_upper, "%<extra></extra>")
      ) %>%
      # Add unity line
      plotly::add_lines(
        x = c(0.5, nrow(wide_data) + 0.5), 
        y = c(100, 100),
        line = list(color = "gray", dash = "solid", width = 1),
        name = "Unity (100%)",
        hovertemplate = "Unity: 100%<extra></extra>"
      ) %>%
      # Add individual subject ratios as scatter plot with connecting lines
      plotly::add_trace(
        data = wide_data,
        x = ~Analysis_Order,
        y = ~Ratio_Percent,
        type = "scatter",
        mode = "markers+lines",
        marker = list(size = 8, color = "#1F78B4", opacity = 0.8),
        line = list(color = "#1F78B4", width = 2, dash = "dot"),
        name = "Individual T/R Ratios",
        hovertemplate = paste0(
          "<b>Subject %{text}</b><br>",
          "Analysis Order: %{x}<br>",
          "T/R Ratio: %{y:.1f}%<br>",
          "<extra></extra>"
        ),
        text = wide_data$Subject
      ) %>%
      # Add cumulative trend line
      plotly::add_trace(
        data = wide_data,
        x = ~Analysis_Order,
        y = ~Cumulative_Ratio,
        type = "scatter",
        mode = "lines",
        line = list(color = "#E31A1C", width = 3),
        name = "Cumulative T/R Ratio",
        hovertemplate = paste0(
          "<b>Cumulative Analysis</b><br>",
          "Through Subject: %{text}<br>",
          "Subjects Included: %{x}<br>",
          "Overall T/R Ratio: %{y:.1f}%<br>",
          "<extra></extra>"
        ),
        text = wide_data$Subject
      ) %>%
      plotly::layout(
        title = list(
          text = paste0("Individual Subject T/R Ratios with Cumulative Trend<br>", 
                       "<sub>Parameter: ", parameter, "</sub>"),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Analysis Order (Subject Number)",
          tickmode = "array",
          tickvals = wide_data$Analysis_Order,
          ticktext = wide_data$Subject
        ),
        yaxis = list(
          title = "Test/Reference Ratio (%)",
          range = c(y_limit_min, y_limit_max)
        ),
        hovermode = "closest",
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1
        )
      )
      
  } else {
    # Create static ggplot2 version
    p <- ggplot(wide_data, aes(x = Analysis_Order)) +
      # Add BE limit lines
      geom_hline(yintercept = be_lower, linetype = "dashed", color = "red", 
                 alpha = 0.7, linewidth = 1) +
      geom_hline(yintercept = be_upper, linetype = "dashed", color = "red", 
                 alpha = 0.7, linewidth = 1) +
      # Add unity line
      geom_hline(yintercept = 100, linetype = "solid", color = "gray50", 
                 alpha = 0.5, linewidth = 0.8) +
      # Add individual subject ratios with connecting line
      geom_line(aes(y = Ratio_Percent), color = "#1F78B4", linewidth = 1, 
                linetype = "dotted", alpha = 0.7) +
      geom_point(aes(y = Ratio_Percent), size = 3, color = "#1F78B4", 
                 alpha = 0.8) +
      # Add cumulative trend line
      geom_line(aes(y = Cumulative_Ratio), color = "#E31A1C", linewidth = 2) +
      # Styling
      labs(
        x = "Analysis Order (Subject Number)", 
        y = "Test/Reference Ratio (%)",
        title = paste("Individual Subject T/R Ratios with Cumulative Trend"),
        subtitle = paste0("Parameter: ", parameter, 
                         " | Red dashed lines: BE limits (", be_lower, "-", be_upper, "%)",
                         " | Blue dots: Individual ratios | Red line: Cumulative ratio")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      ) +
      # Custom x-axis labels showing subject numbers
      scale_x_continuous(
        breaks = wide_data$Analysis_Order,
        labels = wide_data$Subject
      ) +
      # Y-axis limits - dynamically adjusted to show all data
      coord_cartesian(ylim = c(y_limit_min, y_limit_max))
  }
  
  return(p)
}

# =============================================================================
# ENHANCED INTERACTIVE PLOTTING FUNCTIONS FOR BIOEQ WEB APPLICATION
# =============================================================================

#' Create interactive box plots for PK parameters by treatment
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameters Vector of PK parameters to plot (default: c("AUC0t", "Cmax"))
#' @param log_scale Logical, whether to use log scale (default TRUE)
#' @param show_individual_points Logical, whether to overlay individual points (default TRUE)
#' @param interactive Logical, whether to create interactive plotly plot (default TRUE)
#' @export
plot_pk_boxplots <- function(data, parameters = c("AUC0t", "Cmax"), 
                           log_scale = TRUE, show_individual_points = TRUE, 
                           interactive = TRUE) {
  
  if (interactive) {
    return(plot_pk_boxplots_interactive(data, parameters, log_scale, show_individual_points))
  } else {
    return(plot_pk_boxplots_static(data, parameters, log_scale, show_individual_points))
  }
}

#' Create interactive box plots for PK parameters (interactive version)
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameters Vector of PK parameters to plot
#' @param log_scale Logical, whether to use log scale
#' @param show_individual_points Logical, whether to overlay individual points
#' @export
plot_pk_boxplots_interactive <- function(data, parameters = c("AUC0t", "Cmax"), 
                                       log_scale = TRUE, show_individual_points = TRUE) {
  
  # Standardize column names first
  data <- standardize_column_names(data)
  
  # Check if we have required columns
  if (!"Formulation" %in% names(data)) {
    stop("No treatment/formulation column found in data. Expected one of: Formulation, Treatment, Trt, Group")
  }
  
  # Validate parameters exist in data
  missing_params <- setdiff(parameters, names(data))
  if (length(missing_params) > 0) {
    stop("Parameters not found in data: ", paste(missing_params, collapse = ", "))
  }
  
  # Reshape data for plotting
  plot_data <- data %>%
    select(Subject, Formulation, all_of(parameters)) %>%
    tidyr::pivot_longer(cols = all_of(parameters), 
                       names_to = "Parameter", 
                       values_to = "Value") %>%
    filter(!is.na(Value) & Value > 0)
  
  if (nrow(plot_data) == 0) {
    stop("No valid data found for plotting")
  }
  
  # Create subplot layout
  subplot_list <- list()
  
  for (param in parameters) {
    param_data <- plot_data[plot_data$Parameter == param, ]
    
    # Create box plot
    p <- plot_ly(param_data, x = ~Formulation, y = ~Value, 
                color = ~Formulation, colors = c("#E31A1C", "#1F78B4"),
                type = "box", 
                name = param,
                hovertemplate = paste(
                  "<b>%{fullData.name}</b><br>",
                  "Formulation: %{x}<br>",
                  "Value: %{y:.3f}<br>",
                  "<extra></extra>"
                )) %>%
      layout(
        title = list(text = param, font = list(size = 14)),
        xaxis = list(title = "Formulation"),
        yaxis = list(
          title = paste(param, if (log_scale) "(log scale)" else ""),
          type = if (log_scale) "log" else "linear"
        )
      )
    
    # Add individual points if requested
    if (show_individual_points) {
      p <- p %>% add_trace(
        x = param_data$Formulation, 
        y = param_data$Value,
        type = "scatter", 
        mode = "markers",
        marker = list(
          size = 6,
          color = ifelse(param_data$Formulation == "Reference", "#E31A1C", "#1F78B4"),
          opacity = 0.7,
          line = list(width = 1, color = "white")
        ),
        text = param_data$Subject,
        hovertemplate = paste(
          "<b>", param, "</b><br>",
          "Subject: %{text}<br>",
          "Formulation: %{x}<br>",
          "Value: %{y:.3f}<br>",
          "<extra></extra>"
        ),
        name = paste(param, "Individual"),
        showlegend = FALSE
      )
    }
    
    subplot_list[[param]] <- p
  }
  
  # Create subplot layout
  if (length(parameters) == 1) {
    final_plot <- subplot_list[[1]]
  } else {
    final_plot <- plotly::subplot(subplot_list, nrows = ceiling(length(parameters)/2), 
                                 shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
      layout(
        title = list(
          text = "PK Parameter Distributions by Treatment",
          font = list(size = 16)
        ),
        showlegend = TRUE
      )
  }
  
  return(final_plot)
}

#' Create static box plots for PK parameters (static version)
#'
#' @param data PK parameter data with Subject, Formulation, and PK parameters
#' @param parameters Vector of PK parameters to plot
#' @param log_scale Logical, whether to use log scale
#' @param show_individual_points Logical, whether to overlay individual points
#' @export
plot_pk_boxplots_static <- function(data, parameters = c("AUC0t", "Cmax"), 
                                  log_scale = TRUE, show_individual_points = TRUE) {
  
  # Standardize column names first
  data <- standardize_column_names(data)
  
  # Check if we have required columns
  if (!"Formulation" %in% names(data)) {
    stop("No treatment/formulation column found in data. Expected one of: Formulation, Treatment, Trt, Group")
  }
  
  # Validate parameters exist in data
  missing_params <- setdiff(parameters, names(data))
  if (length(missing_params) > 0) {
    stop("Parameters not found in data: ", paste(missing_params, collapse = ", "))
  }
  
  # Reshape data for plotting
  plot_data <- data %>%
    select(Subject, Formulation, all_of(parameters)) %>%
    tidyr::pivot_longer(cols = all_of(parameters), 
                       names_to = "Parameter", 
                       values_to = "Value") %>%
    filter(!is.na(Value) & Value > 0)
  
  if (nrow(plot_data) == 0) {
    stop("No valid data found for plotting")
  }
  
  p <- ggplot(plot_data, aes(x = Formulation, y = Value, fill = Formulation)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_fill_manual(values = c("Reference" = "#E31A1C", "Test" = "#1F78B4")) +
    facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
    labs(
      title = "PK Parameter Distributions by Treatment",
      x = "Formulation",
      y = if (log_scale) "Value (log scale)" else "Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    )
  
  if (show_individual_points) {
    p <- p + geom_point(aes(color = Formulation), 
                       position = position_jitter(width = 0.2), 
                       size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Reference" = "#E31A1C", "Test" = "#1F78B4"))
  }
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

#' Create interactive forest plot for bioequivalence assessment
#'
#' @param be_results Results from perform_be_analysis
#' @param parameters Vector of parameters to include (default: all available)
#' @param be_limits Bioequivalence limits (default: c(0.8, 1.25))
#' @param interactive Logical, whether to create interactive plotly plot (default TRUE)
#' @export
plot_forest_be <- function(be_results, parameters = NULL, be_limits = c(0.8, 1.25), 
                          interactive = TRUE) {
  
  if (interactive) {
    return(plot_forest_be_interactive(be_results, parameters, be_limits))
  } else {
    return(plot_forest_be_static(be_results, parameters, be_limits))
  }
}

#' Create interactive forest plot for bioequivalence assessment (interactive version)
#'
#' @param be_results Results from perform_be_analysis
#' @param parameters Vector of parameters to include
#' @param be_limits Bioequivalence limits
#' @export
plot_forest_be_interactive <- function(be_results, parameters = NULL, be_limits = c(0.8, 1.25)) {
  
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  ci_data <- be_results$confidence_intervals
  
  if (is.null(parameters)) {
    parameters <- names(ci_data)
  }
  
  # Filter for requested parameters
  ci_data <- ci_data[parameters]
  
  if (length(ci_data) == 0) {
    stop("No valid parameters found in bioequivalence results")
  }
  
  # Prepare data for forest plot
  plot_data <- data.frame(
    Parameter = names(ci_data),
    Lower = sapply(ci_data, function(x) x$ci_lower),
    Upper = sapply(ci_data, function(x) x$ci_upper),
    Estimate = sapply(ci_data, function(x) x$point_estimate),
    stringsAsFactors = FALSE
  )
  
  # Add bioequivalence assessment
  plot_data$BE_Status <- ifelse(
    plot_data$Lower >= (be_limits[1] * 100) & plot_data$Upper <= (be_limits[2] * 100),
    "Bioequivalent", "Not Bioequivalent"
  )
  
  # Reverse order for better visualization (top to bottom)
  plot_data$y_pos <- nrow(plot_data):1
  
  # Create forest plot
  p <- plot_ly() %>%
    # Add confidence interval lines
    add_segments(
      data = plot_data,
      x = ~Lower, xend = ~Upper,
      y = ~y_pos, yend = ~y_pos,
      line = list(
        width = 4,
        color = ifelse(plot_data$BE_Status == "Bioequivalent", "#2E8B57", "#DC143C")
      ),
      hovertemplate = paste(
        "<b>%{customdata}</b><br>",
        "90% CI: %{x:.1f}% - %{customdata2:.1f}%<br>",
        "Point Estimate: %{customdata3:.1f}%<br>",
        "Status: %{customdata4}<br>",
        "<extra></extra>"
      ),
      customdata = plot_data$Parameter,
      customdata2 = plot_data$Upper,
      customdata3 = plot_data$Estimate,
      customdata4 = plot_data$BE_Status,
      name = "90% Confidence Intervals",
      showlegend = FALSE
    ) %>%
    # Add point estimates
    add_markers(
      data = plot_data,
      x = ~Estimate, y = ~y_pos,
      marker = list(
        size = 10,
        color = ifelse(plot_data$BE_Status == "Bioequivalent", "#2E8B57", "#DC143C"),
        symbol = "diamond",
        line = list(width = 2, color = "white")
      ),
      hovertemplate = paste(
        "<b>%{customdata}</b><br>",
        "Point Estimate: %{x:.1f}%<br>",
        "90% CI: %{customdata2:.1f}% - %{customdata3:.1f}%<br>",
        "Status: %{customdata4}<br>",
        "<extra></extra>"
      ),
      customdata = plot_data$Parameter,
      customdata2 = plot_data$Lower,
      customdata3 = plot_data$Upper,
      customdata4 = plot_data$BE_Status,
      name = "Point Estimates",
      showlegend = FALSE
    ) %>%
    # Add bioequivalence limit lines
    add_lines(x = rep(c(plot_data$Parameter[1], plot_data$Parameter[length(plot_data$Parameter)]), 1),
             y = rep(be_limits[1] * 100, 2), 
             line = list(color = "red", dash = "dash", width = 2),
             name = "BE Lower Limit",
             showlegend = FALSE,
             hovertemplate = paste0("BE Lower Limit: ", be_limits[1] * 100, "%<extra></extra>")) %>%
    add_lines(x = rep(c(plot_data$Parameter[1], plot_data$Parameter[length(plot_data$Parameter)]), 1),
             y = rep(be_limits[2] * 100, 2), 
             line = list(color = "red", dash = "dash", width = 2),
             name = "BE Upper Limit",
             showlegend = FALSE,
             hovertemplate = paste0("BE Upper Limit: ", be_limits[2] * 100, "%<extra></extra>")) %>%
    add_lines(x = rep(c(plot_data$Parameter[1], plot_data$Parameter[length(plot_data$Parameter)]), 1),
             y = rep(100, 2), 
             line = list(color = "gray", dash = "solid", width = 1),
             name = "Unity",
             showlegend = FALSE,
             hovertemplate = "Unity: 100%<extra></extra>") %>%
    layout(
      title = list(
        text = "Forest Plot - Bioequivalence Assessment",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Test/Reference Ratio (%) with 90% CI",
        range = c(70, 140),
        showgrid = TRUE,
        gridcolor = 'rgba(128,128,128,0.2)'
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = plot_data$y_pos,
        ticktext = plot_data$Parameter,
        showgrid = FALSE
      ),
      hovermode = "closest",
      annotations = list(
        list(
          x = be_limits[1] * 100, y = max(plot_data$y_pos) + 0.5,
          text = paste0(be_limits[1] * 100, "%"), showarrow = FALSE,
          font = list(color = "red", size = 12)
        ),
        list(
          x = be_limits[2] * 100, y = max(plot_data$y_pos) + 0.5,
          text = paste0(be_limits[2] * 100, "%"), showarrow = FALSE,
          font = list(color = "red", size = 12)
        ),
        list(
          x = 100, y = max(plot_data$y_pos) + 0.5,
          text = "100%", showarrow = FALSE,
          font = list(color = "gray", size = 12)
        )
      ),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
  
  return(p)
}

#' Create static forest plot for bioequivalence assessment (static version)
#'
#' @param be_results Results from perform_be_analysis
#' @param parameters Vector of parameters to include
#' @param be_limits Bioequivalence limits
#' @export
plot_forest_be_static <- function(be_results, parameters = NULL, be_limits = c(0.8, 1.25)) {
  
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  ci_data <- be_results$confidence_intervals
  
  if (is.null(parameters)) {
    parameters <- names(ci_data)
  }
  
  # Filter for requested parameters
  ci_data <- ci_data[parameters]
  
  if (length(ci_data) == 0) {
    stop("No valid parameters found in bioequivalence results")
  }
  
  # Prepare data for forest plot
  plot_data <- data.frame(
    Parameter = factor(names(ci_data), levels = rev(names(ci_data))),
    Lower = sapply(ci_data, function(x) x$ci_lower),
    Upper = sapply(ci_data, function(x) x$ci_upper),
    Estimate = sapply(ci_data, function(x) x$point_estimate),
    stringsAsFactors = FALSE
  )
  
  # Add bioequivalence assessment
  plot_data$BE_Status <- ifelse(
    plot_data$Lower >= (be_limits[1] * 100) & plot_data$Upper <= (be_limits[2] * 100),
    "Bioequivalent", "Not Bioequivalent"
  )
  
  p <- ggplot(plot_data, aes(x = Estimate, y = Parameter, color = BE_Status)) +
    geom_vline(xintercept = c(be_limits[1] * 100, be_limits[2] * 100), 
               linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = 100, linetype = "solid", color = "gray") +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, linewidth = 1.5) +
    geom_point(size = 4, shape = 18) +
    scale_color_manual(values = c("Bioequivalent" = "#2E8B57", "Not Bioequivalent" = "#DC143C")) +
    labs(
      title = "Forest Plot - Bioequivalence Assessment",
      x = "Test/Reference Ratio (%) with 90% CI",
      y = "",
      color = "BE Status"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(color = "gray"),
      panel.grid.major.y = element_blank()
    ) +
    coord_cartesian(xlim = c(70, 140))
  
  return(p)
}

#' Create interactive ANOVA diagnostic plots
#'
#' @param be_results Results from perform_be_analysis containing ANOVA results
#' @param parameters Vector of parameters to create diagnostics for (default: all available)
#' @param interactive Logical, whether to create interactive plotly plot (default TRUE)
#' @export
plot_anova_diagnostics <- function(be_results, parameters = NULL, interactive = TRUE) {
  
  if (interactive) {
    return(plot_anova_diagnostics_interactive(be_results, parameters))
  } else {
    return(plot_anova_diagnostics_static(be_results, parameters))
  }
}

#' Create interactive ANOVA diagnostic plots (interactive version)
#'
#' @param be_results Results from perform_be_analysis containing ANOVA results
#' @param parameters Vector of parameters to create diagnostics for
#' @export
plot_anova_diagnostics_interactive <- function(be_results, parameters = NULL) {
  
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  if (!is.null(be_results$anova_results)) {
    anova_data <- be_results$anova_results
  } else {
    stop("No ANOVA results found in bioequivalence results")
  }
  
  if (is.null(parameters)) {
    parameters <- names(anova_data)
  }
  
  # Filter for requested parameters
  anova_data <- anova_data[parameters]
  
  if (length(anova_data) == 0) {
    stop("No valid parameters found in ANOVA results")
  }
  
  diagnostic_plots <- list()
  
  for (param in names(anova_data)) {
    param_results <- anova_data[[param]]
    
    if (!is.null(param_results$residuals) && !is.null(param_results$fitted)) {
      residuals <- param_results$residuals
      fitted <- param_results$fitted
      
      # 1. Residuals vs Fitted
      residual_plot <- plot_ly(
        x = fitted, y = residuals,
        type = "scatter", mode = "markers",
        marker = list(size = 6, color = "#1F78B4", opacity = 0.7),
        hovertemplate = paste(
          "<b>Residuals vs Fitted</b><br>",
          "Fitted: %{x:.3f}<br>",
          "Residual: %{y:.3f}<br>",
          "<extra></extra>"
        ),
        name = "Residuals"
      ) %>%
        add_hline(y = 0, line = list(color = "red", dash = "dash")) %>%
        layout(
          title = list(text = paste("Residuals vs Fitted -", param), font = list(size = 14)),
          xaxis = list(title = "Fitted Values"),
          yaxis = list(title = "Residuals")
        )
      
      # 2. Q-Q Plot
      if (length(residuals) > 2) {
        # Calculate theoretical quantiles
        n <- length(residuals)
        theoretical <- qnorm((1:n - 0.5) / n)
        sample_quantiles <- sort(residuals)
        
        qq_plot <- plot_ly(
          x = theoretical, y = sample_quantiles,
          type = "scatter", mode = "markers",
          marker = list(size = 6, color = "#E31A1C", opacity = 0.7),
          hovertemplate = paste(
            "<b>Q-Q Plot</b><br>",
            "Theoretical: %{x:.3f}<br>",
            "Sample: %{y:.3f}<br>",
            "<extra></extra>"
          ),
          name = "Sample vs Theoretical"
        ) %>%
          add_trace(
            x = range(theoretical), y = range(theoretical),
            type = "scatter", mode = "lines",
            line = list(color = "red", dash = "dash"),
            name = "Theoretical Line",
            hovertemplate = "Perfect Normal Distribution<extra></extra>"
          ) %>%
          layout(
            title = list(text = paste("Q-Q Plot -", param), font = list(size = 14)),
            xaxis = list(title = "Theoretical Quantiles"),
            yaxis = list(title = "Sample Quantiles")
          )
        
        # 3. Scale-Location Plot (sqrt(|residuals|) vs fitted)
        sqrt_abs_residuals <- sqrt(abs(residuals))
        scale_location_plot <- plot_ly(
          x = fitted, y = sqrt_abs_residuals,
          type = "scatter", mode = "markers",
          marker = list(size = 6, color = "#2E8B57", opacity = 0.7),
          hovertemplate = paste(
            "<b>Scale-Location Plot</b><br>",
            "Fitted: %{x:.3f}<br>",
            "√|Residuals|: %{y:.3f}<br>",
            "<extra></extra>"
          ),
          name = "√|Residuals|"
        ) %>%
          layout(
            title = list(text = paste("Scale-Location Plot -", param), font = list(size = 14)),
            xaxis = list(title = "Fitted Values"),
            yaxis = list(title = "√|Residuals|")
          )
        
        # Store plots for this parameter
        diagnostic_plots[[param]] <- list(
          residual_plot = residual_plot,
          qq_plot = qq_plot,
          scale_location_plot = scale_location_plot
        )
      }
    }
  }
  
  if (length(diagnostic_plots) == 0) {
    stop("No diagnostic plots could be created - insufficient ANOVA data")
  }
  
  # Create subplot layout
  if (length(parameters) == 1) {
    param <- parameters[1]
    if (!is.null(diagnostic_plots[[param]])) {
      subplot_result <- plotly::subplot(
        diagnostic_plots[[param]]$residual_plot,
        diagnostic_plots[[param]]$qq_plot,
        diagnostic_plots[[param]]$scale_location_plot,
        nrows = 2, ncols = 2,
        subplot_titles = c("Residuals vs Fitted", "Q-Q Plot", "Scale-Location")
      ) %>%
        layout(
          title = list(
            text = paste("ANOVA Diagnostic Plots -", param),
            font = list(size = 16)
          ),
          showlegend = FALSE
        )
    }
  } else {
    # For multiple parameters, create a grid layout
    all_plots <- list()
    plot_titles <- c()
    
    for (param in names(diagnostic_plots)) {
      if (!is.null(diagnostic_plots[[param]])) {
        all_plots <- c(all_plots, diagnostic_plots[[param]]$residual_plot, 
                       diagnostic_plots[[param]]$qq_plot, 
                       diagnostic_plots[[param]]$scale_location_plot)
        plot_titles <- c(plot_titles, 
                         paste("Residuals vs Fitted -", param), 
                         paste("Q-Q Plot -", param), 
                         paste("Scale-Location Plot -", param))
      }
    }
    
    # Combine all plots into a single subplot
    subplot_result <- plotly::subplot(
      all_plots, nrows = length(parameters), 
      titleX = TRUE, titleY = TRUE, 
      margin = 0.05
    ) %>%
      layout(
        title = list(
          text = "ANOVA Diagnostic Plots",
          font = list(size = 16)
        ),
        showlegend = FALSE
      )
  }
  
  return(subplot_result)
}

#' Create static ANOVA diagnostic plots (static version)
#'
#' @param be_results Results from perform_be_analysis containing ANOVA results
#' @param parameters Vector of parameters to create diagnostics for (default: all available)
#' @export
plot_anova_diagnostics_static <- function(be_results, parameters = NULL) {
  
  if (!inherits(be_results, "bioeq")) {
    stop("Input must be a bioeq object from perform_be_analysis")
  }
  
  if (!is.null(be_results$anova_results)) {
    anova_data <- be_results$anova_results
  } else {
    stop("No ANOVA results found in bioequivalence results")
  }
  
  if (is.null(parameters)) {
    parameters <- names(anova_data)
  }
  
  # Filter for requested parameters
  anova_data <- anova_data[parameters]
  
  if (length(anova_data) == 0) {
    stop("No valid parameters found in ANOVA results")
  }
  
  diagnostic_plots <- list()
  
  for (param in names(anova_data)) {
    param_results <- anova_data[[param]]
    
    if (!is.null(param_results$residuals) && !is.null(param_results$fitted)) {
      residuals <- param_results$residuals
      fitted <- param_results$fitted
      
      # 1. Residuals vs Fitted
      p1 <- ggplot(data.frame(Fitted = fitted, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        geom_smooth(method = "loess", se = FALSE, color = "blue") +
        labs(title = paste("Residuals vs Fitted -", param),
             x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      
      # 2. Q-Q Plot
      if (length(residuals) > 2) {
        # Calculate theoretical quantiles
        n <- length(residuals)
        theoretical <- qnorm((1:n - 0.5) / n)
        sample_quantiles <- sort(residuals)
        
        p2 <- ggplot(data.frame(Theoretical = theoretical, Sample = sample_quantiles), aes(x = Theoretical, y = Sample)) +
          geom_point(alpha = 0.6, color = "#E31A1C") +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
          labs(title = paste("Q-Q Plot -", param),
               x = "Theoretical Quantiles", y = "Sample Quantiles") +
          theme_minimal()
      } else {
        p2 <- NULL
      }
      
      # 3. Scale-Location Plot (sqrt(|residuals|) vs fitted)
      sqrt_abs_residuals <- sqrt(abs(residuals))
      p3 <- ggplot(data.frame(Fitted = fitted, `√|Residuals|` = sqrt_abs_residuals), aes(x = Fitted, y = `√|Residuals|`)) +
        geom_point(alpha = 0.6, color = "#2E8B57") +
        labs(title = paste("Scale-Location Plot -", param),
             x = "Fitted Values", y = "√|Residuals|") +
        theme_minimal()
      
      # Combine plots for this parameter
      if (!is.null(p2)) {
        diagnostic_plots[[param]] <- gridExtra::grid.arrange(p1, p2, p3, nrow = 2)
      } else {
        diagnostic_plots[[param]] <- p1
      }
    }
  }
  
  # Arrange all diagnostic plots in a grid
  gridExtra::grid.arrange(grobs = diagnostic_plots, ncol = 2)
}
