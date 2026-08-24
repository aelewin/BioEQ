# BioEQ - Plotting and Visualization Functions
# Modern plotting functions for bioequivalence analysis with dual-output support (static & interactive)

# Required packages for plotting
# NOTE: gridExtra was removed from this list in the 2026-08 dependency audit —
# it was attached at startup but no gridExtra function (grid.arrange/
# arrangeGrob/tableGrob) is called anywhere in BioEQ.
required_packages <- c("ggplot2", "plotly", "dplyr", "tidyr", "htmlwidgets")

# Check and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    message("Installing required package: ", pkg)
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Consistent color palette for treatments and replicate periods
# T1 = dark blue, T2 = light blue, R1 = dark red, R2 = light red/salmon
BIOEQ_COLORS <- c(
  "T1"        = "#1F78B4",  # Dark blue  — Test replicate 1
  "T2"        = "#A6CEE3",  # Light blue — Test replicate 2
  "R1"        = "#E31A1C",  # Dark red   — Reference replicate 1
  "R2"        = "#FB9A99",  # Light red  — Reference replicate 2
  "Test"      = "#1F78B4",  # Fallback for 2x2x2
  "Reference" = "#E31A1C"   # Fallback for 2x2x2
)

#' Validate concentration-time data structure
#'
#' @param data Data frame to validate
#' @param silent Logical, if TRUE returns validation result instead of stopping (default FALSE)
#' @return If silent=TRUE, returns list with valid (logical) and message (character)
#' @export
validate_conc_time_data <- function(data, silent = FALSE) {
  required_cols <- c("Time", "Concentration", "Subject", "Treatment")
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
  formulations <- unique(data$Treatment)
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
      "Treatment" = c("Treatment", "Treatment", "Trt", "Group"),
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
#' @param data Data frame with Time, Concentration, Subject, and Treatment columns
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
#' @param data PK parameter data with Subject, Treatment, and PK parameters
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
    
    # Check if we have a Treatment column after standardization
    if (!"Treatment" %in% names(data)) {
      return(list(plot = NULL, error = "No treatment/formulation column found in data. Expected one of: Treatment, Treatment, Trt, Group", warnings = NULL))
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
    
    # Does data have per-replicate labels?
    has_tp_indiv <- "TreatmentPeriod" %in% names(plot_data) && !all(is.na(plot_data$TreatmentPeriod))

    # Add individual profiles for each selected subject
    for (subj in unique(plot_data$Subject)) {
      if (has_tp_indiv) {
        for (tp in sort(unique(plot_data$TreatmentPeriod[!is.na(plot_data$TreatmentPeriod)]))) {
          subj_data <- plot_data[
            !is.na(plot_data$TreatmentPeriod) &
            plot_data$Subject == subj &
            plot_data$TreatmentPeriod == tp, ]
          if (nrow(subj_data) > 0) {
            color_val <- unname(BIOEQ_COLORS[tp])
            if (is.na(color_val)) color_val <- "#888888"

            p <- p %>% add_trace(
              x = subj_data$Time,
              y = subj_data$Concentration,
              type = "scatter", mode = "lines+markers",
              line   = list(color = color_val, width = 2),
              marker = list(color = color_val, size  = 6),
              opacity = 0.8,
              name = paste("Subject", subj, "-", tp),
              legendgroup = tp,
              hovertemplate = paste0(
                "<b>Subject:</b> ",      subj, "<br>",
                "<b>Period:</b> ",       tp,   "<br>",
                "<b>Time:</b> %{x}<br>",
                "<b>Concentration:</b> %{y}<br>",
                "<extra></extra>"
              )
            )
          }
        }
      } else {
        for (form in unique(plot_data$Treatment)) {
          subj_data <- plot_data[plot_data$Subject == subj & plot_data$Treatment == form, ]
          if (nrow(subj_data) > 0) {
            color_val <- unname(BIOEQ_COLORS[form])
            if (is.na(color_val)) color_val <- "#1F78B4"

            p <- p %>% add_trace(
              x = subj_data$Time,
              y = subj_data$Concentration,
              type = "scatter", mode = "lines+markers",
              line   = list(color = color_val, width = 2),
              marker = list(color = color_val, size  = 6),
              opacity = 0.8,
              name = paste("Subject", subj, "-", form),
              legendgroup = form,
              hovertemplate = paste0(
                "<b>Subject:</b> ",      subj, "<br>",
                "<b>Treatment:</b> ",    form, "<br>",
                "<b>Time:</b> %{x}<br>",
                "<b>Concentration:</b> %{y}<br>",
                "<extra></extra>"
              )
            )
          }
        }
      }
    }
    
    # Configure plot layout
    # Get user-specified units from global options if available
    concentration_label <- getOption("bioeq.concentration.label", "Concentration")
    time_label <- getOption("bioeq.time.label", "Time")
    
    y_title <- if (log_scale) paste0(gsub(" \\(.*\\)", "", concentration_label), " (log scale)") else concentration_label
    y_type <- if (log_scale) "log" else "linear"
    
    p <- p %>% layout(
      title = list(
        text = paste("Individual Subject Profiles (n =", length(unique(plot_data$Subject)), "subjects)"),
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = list(text = time_label, font = list(size = 14)),
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

#' Plot concentration-time profile
#'
#' @param data Data frame with Time, Concentration, Subject, and Treatment columns
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
#' @param data Data frame with Time, Concentration, Subject, and Treatment columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @export
plot_concentration_time_static <- function(data, log_scale = FALSE, individual = TRUE, mean_profile = TRUE) {
  # Use silent validation for better Shiny integration
  validation <- validate_conc_time_data(data, silent = FALSE)  # Keep original behavior for backward compatibility
  
  p <- ggplot(data, aes(x = Time, y = Concentration, color = Treatment))
  
  if (individual) {
    p <- p + geom_line(aes(group = interaction(Subject, Treatment)), alpha = 0.3)
    p <- p + geom_point(aes(group = interaction(Subject, Treatment)), alpha = 0.3, size = 0.8)
  }
  
  if (mean_profile) {
    # Group by NOMINAL time (see .derive_nominal_time()), not the exact
    # recorded Time - actual-vs-planned sampling deviations otherwise
    # fragment the mean into spurious near-duplicate points. Individual
    # profiles above intentionally keep the exact actual Time.
    nominal_map <- .derive_nominal_time(data$Time)
    data$NominalTime <- unname(nominal_map[as.character(data$Time)])

    mean_data <- data %>%
      group_by(NominalTime, Treatment) %>%
      summarise(
        Mean_Conc = mean(Concentration, na.rm = TRUE),
        .groups = 'drop'
      )

    p <- p + geom_line(data = mean_data, aes(x = NominalTime, y = Mean_Conc), linewidth = 1.2)
    p <- p + geom_point(data = mean_data, aes(x = NominalTime, y = Mean_Conc), size = 2)
  }

  # Get user-specified units from global options if available
  concentration_label <- getOption("bioeq.concentration.label", "Concentration (ng/mL)")
  time_label <- getOption("bioeq.time.label", "Time (h)")

  if (log_scale) {
    p <- p + scale_y_log10()
    p <- p + labs(y = paste0(gsub(" \\(.*\\)", "", concentration_label), " (natural log scale)"))
  } else {
    p <- p + labs(y = concentration_label)
  }
  
  p <- p + labs(x = time_label, title = "Concentration-Time Profiles") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(t = 10, r = 10, b = 40, l = 10, unit = "pt"),
      axis.title.x = element_text(margin = margin(t = 15))
    ) +
    scale_color_brewer(type = "qual", palette = "Set1")
  
  return(p)
}

#' Plot concentration-time profile (interactive plotly version)
#'
#' @param data Data frame with Time, Concentration, Subject, and Treatment columns
#' @param log_scale Logical, whether to use log scale for concentration
#' @param individual Logical, whether to show individual profiles
#' @param mean_profile Logical, whether to show mean profiles
#' @export
plot_concentration_time_interactive <- function(data, log_scale = FALSE, individual = TRUE, mean_profile = TRUE) {
  # Use silent validation for better Shiny integration
  validation <- validate_conc_time_data(data, silent = FALSE)

  # Does data have per-replicate period labels (T1/T2/R1/R2)?
  has_tp <- "TreatmentPeriod" %in% names(data) && !all(is.na(data$TreatmentPeriod))

  # Start with empty plotly object
  p <- plot_ly()

  if (individual) {
    if (has_tp) {
      # Replicate design: one trace per (subject, TreatmentPeriod), colored distinctly
      all_subs <- unique(data$Subject)
      all_tps  <- sort(unique(data$TreatmentPeriod[!is.na(data$TreatmentPeriod)]))

      for (subj in all_subs) {
        for (tp in all_tps) {
          subj_data <- data[
            !is.na(data$TreatmentPeriod) &
            data$Subject == subj &
            data$TreatmentPeriod == tp, ]
          if (nrow(subj_data) > 0) {
            color_val <- unname(BIOEQ_COLORS[tp])
            if (is.na(color_val)) color_val <- "#888888"

            p <- p %>% add_trace(
              x = subj_data$Time,
              y = subj_data$Concentration,
              type = "scatter", mode = "lines+markers",
              line   = list(color = color_val, width = 1),
              marker = list(color = color_val, size  = 4),
              opacity = 0.4,
              name = tp,
              legendgroup = tp,
              showlegend = (subj == all_subs[1]),
              hovertemplate = paste0(
                "<b>Subject:</b> ", subj, "<br>",
                "<b>Period:</b> ",  tp, "<br>",
                "<b>Time:</b> %{x}<br>",
                "<b>Concentration:</b> %{y}<br>",
                "<extra></extra>"
              )
            )
          }
        }
      }
    } else {
      # 2x2x2: original Treatment-based coloring
      for (subj in unique(data$Subject)) {
        for (form in unique(data$Treatment)) {
          subj_data <- data[data$Subject == subj & data$Treatment == form, ]
          if (nrow(subj_data) > 0) {
            color_val <- unname(BIOEQ_COLORS[form])
            if (is.na(color_val)) color_val <- "#1F78B4"

            p <- p %>% add_trace(
              x = subj_data$Time,
              y = subj_data$Concentration,
              type = "scatter", mode = "lines+markers",
              line   = list(color = color_val, width = 1),
              marker = list(color = color_val, size  = 4),
              opacity = 0.4,
              name = paste(form, "- Subject", subj),
              legendgroup = form,
              showlegend = (subj == unique(data$Subject)[1]),
              hovertemplate = paste0(
                "<b>Subject:</b> ", subj, "<br>",
                "<b>Treatment:</b> ", form, "<br>",
                "<b>Time:</b> %{x}<br>",
                "<b>Concentration:</b> %{y}<br>",
                "<extra></extra>"
              )
            )
          }
        }
      }
    }
  }

  if (mean_profile) {
    # Mean profiles always by Treatment (T/R overall — all replicates pooled).
    # Group by NOMINAL time (see .derive_nominal_time()), not the exact
    # recorded Time - actual-vs-planned sampling deviations otherwise
    # fragment the mean into spurious near-duplicate points. Individual
    # traces above intentionally keep the exact actual Time.
    nominal_map <- .derive_nominal_time(data$Time)
    data$NominalTime <- unname(nominal_map[as.character(data$Time)])

    mean_data <- data %>%
      group_by(NominalTime, Treatment) %>%
      summarise(
        Mean_Conc = mean(Concentration, na.rm = TRUE),
        N         = n(),
        .groups   = 'drop'
      )

    for (form in unique(mean_data$Treatment)) {
      form_data <- mean_data[mean_data$Treatment == form, ]
      color_val <- unname(BIOEQ_COLORS[form])
      if (is.na(color_val)) color_val <- "#1F78B4"

      p <- p %>% add_trace(
        x = form_data$NominalTime,
        y = form_data$Mean_Conc,
        type = "scatter", mode = "lines+markers",
        line   = list(color = color_val, width = 3),
        marker = list(color = color_val, size  = 8),
        name = paste("Mean", form),
        legendgroup = paste("mean", form),
        hovertemplate = paste0(
          "<b>Treatment:</b> ", form, "<br>",
          "<b>Time:</b> %{x}<br>",
          "<b>Mean Concentration:</b> %{y:.3f}<br>",
          "<b>N:</b> %{text}<br>",
          "<extra></extra>"
        ),
        text = paste("N:", form_data$N)
      )
    }
  }
  
  # Configure layout
  # Get user-specified units from global options if available
  concentration_label <- getOption("bioeq.concentration.label", "Concentration")
  time_label <- getOption("bioeq.time.label", "Time")
  
  y_title <- if (log_scale) paste0(gsub(" \\(.*\\)", "", concentration_label), " (natural log scale)") else concentration_label

  p <- p %>% layout(
    title = list(
      text = "Concentration-Time Profiles",
      font = list(size = 16)
    ),
    xaxis = list(
      title = time_label,
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
      y = -0.15
    ),
    margin = list(b = 80, l = 60, r = 40, t = 60),
    plot_bgcolor = 'rgba(0,0,0,0)',
    paper_bgcolor = 'rgba(0,0,0,0)'
  )
  
  return(p)
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
  
  # Calculate dynamic y-axis limits based on data
  data_min <- min(plot_data$Lower, na.rm = TRUE)
  data_max <- max(plot_data$Upper, na.rm = TRUE)
  
  # Ensure limits include BE limits (80-125%) and add some padding
  y_min <- min(data_min * 0.9, 70)  # 10% padding below data or 70%, whichever is lower
  y_max <- max(data_max * 1.1, 140)  # 10% padding above data or 140%, whichever is higher
  
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
    coord_cartesian(ylim = c(y_min, y_max))
  
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
  
  # Calculate dynamic y-axis limits based on data
  data_min <- min(plot_data$Lower, na.rm = TRUE)
  data_max <- max(plot_data$Upper, na.rm = TRUE)
  
  # Ensure limits include BE limits (80-125%) and add some padding
  y_min <- min(data_min * 0.9, 70)  # 10% padding below data or 70%, whichever is lower
  y_max <- max(data_max * 1.1, 140)  # 10% padding above data or 140%, whichever is higher
  
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
        range = c(y_min, y_max)
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

# =============================================================================
# ENHANCED INTERACTIVE PLOTTING FUNCTIONS FOR BIOEQ WEB APPLICATION
# =============================================================================

#' Create interactive box plots for PK parameters by treatment
#'
#' @param data PK parameter data with Subject, Treatment, and PK parameters
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
#' @param data PK parameter data with Subject, Treatment, and PK parameters
#' @param parameters Vector of PK parameters to plot
#' @param log_scale Logical, whether to use log scale
#' @param show_individual_points Logical, whether to overlay individual points
#' @export
plot_pk_boxplots_interactive <- function(data, parameters = c("AUC0t", "Cmax"), 
                                       log_scale = TRUE, show_individual_points = TRUE) {
  
  # Standardize column names first
  data <- standardize_column_names(data)
  
  # Check if we have required columns
  if (!"Treatment" %in% names(data)) {
    stop("No treatment/formulation column found in data. Expected one of: Treatment, Treatment, Trt, Group")
  }
  
  # Validate parameters exist in data
  missing_params <- setdiff(parameters, names(data))
  if (length(missing_params) > 0) {
    stop("Parameters not found in data: ", paste(missing_params, collapse = ", "))
  }
  
  # Reshape data for plotting
  plot_data <- data %>%
    select(Subject, Treatment, all_of(parameters)) %>%
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
    p <- plot_ly(param_data, x = ~Treatment, y = ~Value, 
                color = ~Treatment, colors = c("#E31A1C", "#1F78B4"),
                type = "box", 
                name = param,
                hovertemplate = paste(
                  "<b>%{fullData.name}</b><br>",
                  "Treatment: %{x}<br>",
                  "Value: %{y:.3f}<br>",
                  "<extra></extra>"
                )) %>%
      layout(
        title = list(text = param, font = list(size = 14)),
        xaxis = list(title = "Treatment"),
        yaxis = list(
          title = paste(param, if (log_scale) "(log scale)" else ""),
          type = if (log_scale) "log" else "linear"
        )
      )
    
    # Add individual points if requested
    if (show_individual_points) {
      p <- p %>% add_trace(
        x = param_data$Treatment, 
        y = param_data$Value,
        type = "scatter", 
        mode = "markers",
        marker = list(
          size = 6,
          color = ifelse(param_data$Treatment == "Reference", "#E31A1C", "#1F78B4"),
          opacity = 0.7,
          line = list(width = 1, color = "white")
        ),
        text = param_data$Subject,
        hovertemplate = paste(
          "<b>", param, "</b><br>",
          "Subject: %{text}<br>",
          "Treatment: %{x}<br>",
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
#' @param data PK parameter data with Subject, Treatment, and PK parameters
#' @param parameters Vector of PK parameters to plot
#' @param log_scale Logical, whether to use log scale
#' @param show_individual_points Logical, whether to overlay individual points
#' @export
plot_pk_boxplots_static <- function(data, parameters = c("AUC0t", "Cmax"), 
                                  log_scale = TRUE, show_individual_points = TRUE) {
  
  # Standardize column names first
  data <- standardize_column_names(data)
  
  # Check if we have required columns
  if (!"Treatment" %in% names(data)) {
    stop("No treatment/formulation column found in data. Expected one of: Treatment, Treatment, Trt, Group")
  }
  
  # Validate parameters exist in data
  missing_params <- setdiff(parameters, names(data))
  if (length(missing_params) > 0) {
    stop("Parameters not found in data: ", paste(missing_params, collapse = ", "))
  }
  
  # Reshape data for plotting
  plot_data <- data %>%
    select(Subject, Treatment, all_of(parameters)) %>%
    tidyr::pivot_longer(cols = all_of(parameters), 
                       names_to = "Parameter", 
                       values_to = "Value") %>%
    filter(!is.na(Value) & Value > 0)
  
  if (nrow(plot_data) == 0) {
    stop("No valid data found for plotting")
  }
  
  p <- ggplot(plot_data, aes(x = Treatment, y = Value, fill = Treatment)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_fill_manual(values = c("Reference" = "#E31A1C", "Test" = "#1F78B4")) +
    facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
    labs(
      title = "PK Parameter Distributions by Treatment",
      x = "Treatment",
      y = if (log_scale) "Value (log scale)" else "Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(t = 10, r = 10, b = 30, l = 10, unit = "pt"),
      strip.text = element_text(size = 12, face = "bold")
    )
  
  if (show_individual_points) {
    p <- p + geom_point(aes(color = Treatment), 
                       position = position_jitter(width = 0.2), 
                       size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Reference" = "#E31A1C", "Test" = "#1F78B4"))
  }
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

#' Simple T/R ratio plot for individual subjects
#'
#' @param data Data frame with subject, treatment, and PK parameters
#' @param parameter PK parameter to plot (e.g., "AUC0t", "Cmax")
#' @param subject_order Vector of subject IDs in desired order
#' @export
create_simple_tr_plot <- function(data, parameter, subject_order = NULL) {
  
  # Find the actual parameter name (handle ln prefix)
  param_name <- parameter
  if (!parameter %in% names(data)) {
    ln_param <- paste0("ln", parameter)
    if (ln_param %in% names(data)) {
      param_name <- ln_param
      cat("Using log-transformed parameter:", ln_param, "\n")
    } else {
      cat("Available columns:", paste(names(data), collapse = ", "), "\n")
      stop("Parameter not found: ", parameter, " or ", ln_param)
    }
  }
  
  # Use correct column names based on actual data structure
  # The cumulative data uses: subject, treatment (not Subject, Treatment)
  # First, get unique PK parameter values per subject-treatment (ignore concentration-time data)
  pk_data <- data %>%
    filter(!is.na(.data[[param_name]])) %>%
    select(subject, treatment, all_of(param_name)) %>%
    # Get unique combinations (removes duplicate rows from concentration-time data)
    distinct(subject, treatment, .keep_all = TRUE)
  
  cat("PK data after selecting unique combinations:", nrow(pk_data), "rows\n")
  
  # Convert to wide format: one row per subject with Test and Reference columns
  tr_data <- pk_data %>%
    tidyr::pivot_wider(
      names_from = treatment, 
      values_from = all_of(param_name),
      values_fn = first  # Take first value if duplicates still exist
    ) %>%
    filter(!is.na(Test), !is.na(Reference))
  
  cat("T/R data after reshaping:", nrow(tr_data), "subjects\n")
  
  # Handle log-transformed data
  if (grepl("^ln", param_name)) {
    tr_data <- tr_data %>%
      mutate(
        Test_orig = exp(Test),
        Ref_orig = exp(Reference),
        TR_Ratio = (Test_orig / Ref_orig) * 100
      )
    cat("Used log-transformed data, converted to original scale\n")
  } else {
    tr_data <- tr_data %>%
      mutate(TR_Ratio = (Test / Reference) * 100)
    cat("Used original scale data\n")
  }
  
  # Apply subject ordering (preserve numerical ordering) - INCLUDE ALL SUBJECTS
  tr_data <- tr_data %>%
    mutate(subject_numeric = as.numeric(as.character(subject)))
  
  if (!is.null(subject_order)) {
    # Convert subject_order to numeric for proper ordering
    subject_order_numeric <- as.numeric(as.character(subject_order))
    
    # Order subjects: first those in subject_order, then remaining subjects numerically
    subjects_in_order <- tr_data$subject_numeric[tr_data$subject_numeric %in% subject_order_numeric]
    subjects_not_in_order <- sort(tr_data$subject_numeric[!tr_data$subject_numeric %in% subject_order_numeric])
    
    # Create final order
    final_order <- c(
      subject_order_numeric[subject_order_numeric %in% tr_data$subject_numeric],
      subjects_not_in_order
    )
    
    # Apply the ordering
    tr_data <- tr_data %>%
      arrange(match(subject_numeric, final_order))
    
    cat("Final subject order:", paste(final_order, collapse = ", "), "\n")
    cat("Subjects after ordering:", paste(tr_data$subject_numeric, collapse = ", "), "\n")
  } else {
    # Default: order ALL subjects numerically
    tr_data <- tr_data %>%
      arrange(subject_numeric)
    
    cat("Subjects ordered numerically:", paste(tr_data$subject_numeric, collapse = ", "), "\n")
  }
  
  # Add plotting position
  tr_data$plot_position <- 1:nrow(tr_data)
  
  cat("Final plot data:", nrow(tr_data), "subjects\n")
  
  if (nrow(tr_data) == 0) {
    stop("No data available for plotting after filtering")
  }
  
  # Add hover text with proper formatting
  tr_data$hover_text <- paste0("Subject: ", tr_data$subject_numeric, 
                              "\nT/R Ratio: ", sprintf("%.2f", tr_data$TR_Ratio), "%")
  
  # Create simple ggplot with proper numerical subject ordering
  p <- ggplot(tr_data, aes(x = plot_position, y = TR_Ratio, text = hover_text)) +
    geom_line(aes(group = 1), color = "blue", alpha = 0.6, linewidth = 0.5) +  # Thin line
    geom_point(size = 3, color = "blue") +
    geom_hline(yintercept = 100, linetype = "solid", color = "black", alpha = 0.5) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_hline(yintercept = 125, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_x_continuous(
      name = "Subject",
      breaks = tr_data$plot_position,
      labels = as.character(tr_data$subject_numeric),
      expand = c(0.02, 0)
    ) +
    labs(
      title = paste("T/R Ratios for", parameter),
      x = "Subject",
      y = "T/R Ratio (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0.5)
    )
  
  cat("Plot created successfully\n")
  return(p)
}


#' Create Lambda Z Regression Diagnostic Plots
#'
#' Generates a page of small plots (one per concentration profile) showing the
#' observed data points on a log scale, the terminal phase points used for
#' lambda z estimation highlighted, and the regression line through them.
#'
#' @param conc_data Data frame with Subject, Treatment, Period, Time, Concentration columns
#' @param nca_subject_data Data frame of per-subject NCA results containing
#'   lambda_z_terminal_times, lambda_z_terminal_concs, lambda_z_slope,
#'   lambda_z_intercept (comma-separated strings for times/concs)
#' @param subjects Optional character vector of subjects to include (NULL = all)
#' @param ncol Number of columns in the facet grid (default 4)
#' @return A ggplot2 object
#' @export
create_lambda_z_regression_plots <- function(conc_data, nca_subject_data, 
                                              subjects = NULL, ncol = 4) {
  
  # Standardize column names
  names(conc_data) <- gsub("^subject$", "Subject", names(conc_data), ignore.case = TRUE)
  names(conc_data) <- gsub("^treatment$", "Treatment", names(conc_data), ignore.case = TRUE)
  names(conc_data) <- gsub("^period$", "Period", names(conc_data), ignore.case = TRUE)
  names(conc_data) <- gsub("^time$", "Time", names(conc_data), ignore.case = TRUE)
  names(conc_data) <- gsub("^concentration$", "Concentration", names(conc_data), ignore.case = TRUE)
  
  # Filter subjects if specified
  if (!is.null(subjects)) {
    conc_data <- conc_data[conc_data$Subject %in% subjects, ]
    nca_subject_data <- nca_subject_data[nca_subject_data$Subject %in% subjects, ]
  }
  
  if (nrow(nca_subject_data) == 0 || nrow(conc_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5) +
             theme_void())
  }
  
  # Build per-profile data for plotting
  plot_data_list <- list()
  regression_line_list <- list()
  terminal_point_list <- list()
  r_squared_list <- list()
  
  for (i in seq_len(nrow(nca_subject_data))) {
    row <- nca_subject_data[i, ]
    subj <- as.character(row$Subject)
    trt <- as.character(row$Treatment)
    period <- if ("Period" %in% names(row) && !is.na(row$Period)) as.character(row$Period) else NA
    
    # Build profile label
    profile_label <- paste0("Subj ", subj, " - ", trt)
    if (!is.na(period)) {
      profile_label <- paste0(profile_label, " (P", period, ")")
    }
    
    # Get raw concentration data for this profile
    mask <- conc_data$Subject == subj & conc_data$Treatment == trt
    if (!is.na(period) && "Period" %in% names(conc_data)) {
      mask <- mask & conc_data$Period == period
    }
    profile_conc <- conc_data[mask, ]
    
    if (nrow(profile_conc) == 0) next
    
    # All observed points (exclude zero concentrations for log scale)
    obs <- profile_conc[profile_conc$Concentration > 0, c("Time", "Concentration")]
    if (nrow(obs) == 0) next
    
    obs$Profile <- profile_label
    obs$PointType <- "Observed"
    plot_data_list[[length(plot_data_list) + 1]] <- obs
    
    # Terminal phase points and regression line
    has_fit <- !is.na(row$lambda_z_slope) && 
               !is.na(row$lambda_z_intercept) &&
               !is.na(row$lambda_z_terminal_times) &&
               nchar(as.character(row$lambda_z_terminal_times)) > 0
    
    if (has_fit) {
      term_times <- as.numeric(strsplit(as.character(row$lambda_z_terminal_times), ",")[[1]])
      term_concs <- as.numeric(strsplit(as.character(row$lambda_z_terminal_concs), ",")[[1]])
      slope <- as.numeric(row$lambda_z_slope)
      intercept <- as.numeric(row$lambda_z_intercept)
      
      # Terminal points
      if (length(term_times) > 0 && all(is.finite(term_concs)) && all(term_concs > 0)) {
        term_df <- data.frame(
          Time = term_times,
          Concentration = term_concs,
          Profile = profile_label,
          PointType = "Terminal Phase"
        )
        terminal_point_list[[length(terminal_point_list) + 1]] <- term_df
      }
      
      # Collect R² for annotation
      r_sq <- if ("lambda_z_r_squared" %in% names(row) && !is.na(row$lambda_z_r_squared)) {
        as.numeric(row$lambda_z_r_squared)
      } else NA
      if (!is.na(r_sq)) {
        r_squared_list[[length(r_squared_list) + 1]] <- data.frame(
          Profile = profile_label,
          r_squared = r_sq,
          stringsAsFactors = FALSE
        )
      }
      
      # Regression line (extend slightly beyond terminal range for visibility)
      if (is.finite(slope) && is.finite(intercept)) {
        line_times <- seq(min(term_times), max(term_times), length.out = 50)
        line_concs <- exp(intercept + slope * line_times)
        
        reg_df <- data.frame(
          Time = line_times,
          Concentration = line_concs,
          Profile = profile_label
        )
        regression_line_list[[length(regression_line_list) + 1]] <- reg_df
      }
    }
  }
  
  if (length(plot_data_list) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No profiles with valid data", size = 5) +
             theme_void())
  }
  
  # Combine data
  all_obs <- do.call(rbind, plot_data_list)
  all_terminal <- if (length(terminal_point_list) > 0) do.call(rbind, terminal_point_list) else NULL
  all_regression <- if (length(regression_line_list) > 0) do.call(rbind, regression_line_list) else NULL
  
  # Build plot
  p <- ggplot(all_obs, aes(x = Time, y = Concentration)) +
    # Observed points (open circles)
    geom_point(color = "grey50", shape = 1, size = 1.5) +
    geom_line(color = "grey70", linewidth = 0.3, alpha = 0.5)
  
  # Add regression line (behind terminal points)
  if (!is.null(all_regression)) {
    p <- p + geom_line(data = all_regression, aes(x = Time, y = Concentration),
                       color = "#2166AC", linewidth = 0.8, linetype = "solid")
  }
  
  # Add terminal phase points (filled, on top)
  if (!is.null(all_terminal)) {
    p <- p + geom_point(data = all_terminal, aes(x = Time, y = Concentration),
                        color = "#D6604D", fill = "#D6604D", shape = 16, size = 2.5)
  }
  
  # Build R² annotation data frame positioned at top-right of each panel
  all_r_squared <- if (length(r_squared_list) > 0) do.call(rbind, r_squared_list) else NULL
  if (!is.null(all_r_squared)) {
    # Compute per-panel max x and max y for positioning
    panel_ranges <- do.call(rbind, lapply(split(all_obs, all_obs$Profile), function(d) {
      data.frame(Profile = d$Profile[1],
                 x_max = max(d$Time, na.rm = TRUE),
                 y_max = max(d$Concentration, na.rm = TRUE),
                 stringsAsFactors = FALSE)
    }))
    all_r_squared <- merge(all_r_squared, panel_ranges, by = "Profile")
    all_r_squared$label <- sprintf("R\u00b2 = %.4f", all_r_squared$r_squared)
    
    p <- p + geom_label(data = all_r_squared,
                        aes(x = x_max, y = y_max, label = label),
                        hjust = 1, vjust = 1,
                        size = 2.5, fontface = "bold",
                        fill = "white", alpha = 0.85,
                        label.padding = unit(0.2, "lines"),
                        label.size = 0.3, color = "#2166AC",
                        inherit.aes = FALSE)
  }
  
  p <- p +
    scale_y_log10() +
    facet_wrap(~ factor(Profile, levels = unique(Profile)), scales = "free", ncol = ncol) +
    labs(x = "Time", y = "Concentration (log scale)") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5)
    )

  return(p)
}

# =============================================================================
# PLOT EXPORT HELPERS (Exports & Reports "Plot Exports (PDF)" card)
# =============================================================================

#' Prepare uploaded concentration-time data for the static export plots below:
#' standardizes column names, maps Treatment T/R -> Test/Reference (needed for
#' BIOEQ_COLORS lookups), and derives a TreatmentPeriod (T1/T2/R1/R2) label
#' for replicate designs (>2 periods). This mirrors, line-for-line, the
#' preprocessing block inside shiny/server/plots_server.R's generate_all_plots()
#' reactive - kept as a separate function here (rather than refactoring that
#' reactive) so the export handlers can reuse it without touching the live
#' interactive Plots tab.
#' @keywords internal
.prepare_plot_data_for_export <- function(conc_data) {
  col_mapping <- list(
    "Time" = c("time", "Time", "TIME"),
    "Concentration" = c("concentration", "Concentration", "CONCENTRATION", "conc", "Conc"),
    "Subject" = c("subject", "Subject", "SUBJECT", "subj", "Subj", "ID", "id"),
    "Treatment" = c("treatment", "Treatment", "TREATMENT", "formulation", "FORMULATION", "trt", "Trt")
  )
  data <- conc_data
  for (std_name in names(col_mapping)) {
    found <- intersect(col_mapping[[std_name]], names(conc_data))
    if (length(found) > 0) names(data)[names(data) == found[1]] <- std_name
  }

  if ("Treatment" %in% names(data)) {
    data$Treatment <- ifelse(data$Treatment == "T", "Test",
                       ifelse(data$Treatment == "R", "Reference", data$Treatment))
  }

  if ("Period" %in% names(data) && length(unique(data$Period)) > 2) {
    if ("Sequence" %in% names(data)) {
      tp_map <- data %>%
        dplyr::distinct(Sequence, Treatment, Period) %>%
        dplyr::arrange(Sequence, Treatment, as.numeric(Period)) %>%
        dplyr::group_by(Sequence, Treatment) %>%
        dplyr::mutate(rep_num = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(TreatmentPeriod = paste0(ifelse(Treatment == "Test", "T", "R"), rep_num)) %>%
        dplyr::select(Sequence, Treatment, Period, TreatmentPeriod)
      data <- dplyr::left_join(data, tp_map, by = c("Sequence", "Treatment", "Period"))
    } else {
      tp_map <- data %>%
        dplyr::distinct(Treatment, Period) %>%
        dplyr::arrange(Treatment, as.numeric(Period)) %>%
        dplyr::group_by(Treatment) %>%
        dplyr::mutate(rep_num = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(TreatmentPeriod = paste0(ifelse(Treatment == "Test", "T", "R"), rep_num)) %>%
        dplyr::select(Treatment, Period, TreatmentPeriod)
      data <- dplyr::left_join(data, tp_map, by = c("Treatment", "Period"))
    }
  }

  data
}

#' Derive a "nominal time" for mean-profile grouping from a vector of actual
#' recorded sampling times. Real BE data often has a handful of points where
#' the recorded time is the ACTUAL draw time rather than the planned/nominal
#' one (e.g. 1.267 instead of 1.25, 4.351 instead of 4.334) - grouping a mean
#' profile by exact Time then fragments each nominal timepoint into several
#' near-singleton groups, producing a jagged/spiky mean line. There is no
#' separate planned-time column in BioEQ's schema, so this reconstructs the
#' nominal schedule from the data itself: distinct times within `tol` of each
#' other (default 0.05h = 3 min - comfortably above realistic actual-time
#' deviations, comfortably below the tightest real nominal spacing seen in
#' practice, e.g. 15 min) are merged into one cluster, and each cluster's
#' representative nominal time is whichever exact value occurs most often
#' across the dataset (the value the majority of subjects actually hit).
#' @param time_values Numeric vector of recorded times (with duplicates)
#' @param tol Merge tolerance in the same units as `time_values` (hours)
#' @return named numeric vector: as.character(actual time) -> nominal time
#' @keywords internal
.derive_nominal_time <- function(time_values, tol = 0.05) {
  time_values <- time_values[!is.na(time_values)]
  freq <- table(time_values)
  u <- sort(unique(time_values))
  if (length(u) <= 1) return(setNames(u, as.character(u)))

  cluster_id <- cumsum(c(1L, diff(u) > tol))
  nominal_by_cluster <- vapply(split(u, cluster_id), function(vals) {
    counts <- freq[as.character(vals)]
    as.numeric(names(counts)[which.max(counts)])
  }, numeric(1))

  lookup <- nominal_by_cluster[as.character(cluster_id)]
  setNames(unname(lookup), as.character(u))
}

#' Build one or two static mean concentration-time profile plots for export.
#' Always returns a pooled Test-vs-Reference mean plot. If `data` carries a
#' TreatmentPeriod column (added by .prepare_plot_data_for_export() for
#' replicate designs), also returns a second plot split by T1/T2/R1/R2.
#' Points are grouped by NOMINAL time (see .derive_nominal_time()), not the
#' exact recorded Time, so that actual-vs-planned sampling deviations don't
#' fragment the mean into spurious near-duplicate points. Individual subject
#' profiles (plot_individual_profiles_paginated()) intentionally keep the
#' exact actual Time - only the mean is nominal-time-binned. No error
#' bars/CI are drawn - just the mean line, to keep the plot readable.
#' @param data Concentration-time data, already run through
#'   .prepare_plot_data_for_export()
#' @param log_scale Logical, whether to use natural log scale for concentration
#' @param nominal_tol Merge tolerance (hours) for deriving nominal time - see
#'   .derive_nominal_time()
#' @return list of 1-2 ggplot objects, each a full standalone page
#' @export
plot_mean_profiles_export <- function(data, log_scale = FALSE, nominal_tol = 0.05) {
  concentration_label <- getOption("bioeq.concentration.label", "Concentration (ng/mL)")
  time_label <- getOption("bioeq.time.label", "Time (h)")
  y_lab <- if (log_scale) paste0(gsub(" \\(.*\\)", "", concentration_label), " (natural log scale)") else concentration_label

  nominal_map <- .derive_nominal_time(data$Time, tol = nominal_tol)
  data$NominalTime <- unname(nominal_map[as.character(data$Time)])

  build_one <- function(group_col, title_suffix) {
    mean_data <- data %>%
      dplyr::group_by(.data[[group_col]], NominalTime) %>%
      dplyr::summarise(
        Mean_Conc = mean(Concentration, na.rm = TRUE),
        .groups = "drop"
      )
    names(mean_data)[names(mean_data) == group_col] <- "Group"
    color_vals <- BIOEQ_COLORS[intersect(names(BIOEQ_COLORS), unique(mean_data$Group))]

    p <- ggplot(mean_data, aes(x = NominalTime, y = Mean_Conc, color = Group)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = color_vals, name = NULL) +
      labs(x = time_label, y = y_lab,
           title = paste("Mean Concentration-Time Profile", title_suffix)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10, unit = "pt"))
    if (log_scale) p <- p + scale_y_log10()
    p
  }

  plots <- list(build_one("Treatment", "(Test vs. Reference, all periods pooled)"))
  if ("TreatmentPeriod" %in% names(data) && !all(is.na(data$TreatmentPeriod))) {
    plots[[length(plots) + 1]] <- build_one("TreatmentPeriod", "(T1 / T2 / R1 / R2)")
  }
  plots
}

#' Build one full-page ggplot per subject (not faceted/grouped with other
#' subjects onto a shared page). Colors by TreatmentPeriod (T1/T2/R1/R2) when
#' present (replicate design), else by Treatment (Test/Reference). Individual
#' profiles intentionally keep the exact actual Time (see
#' plot_mean_profiles_export(), which bins the mean to nominal time instead).
#' @param data Concentration-time data, already run through
#'   .prepare_plot_data_for_export()
#' @param log_scale Logical, whether to use natural log scale for concentration
#' @return list of ggplot objects, one per subject, one subject per page
#' @export
plot_individual_profiles_paginated <- function(data, log_scale = FALSE) {
  concentration_label <- getOption("bioeq.concentration.label", "Concentration (ng/mL)")
  time_label <- getOption("bioeq.time.label", "Time (h)")
  y_lab <- if (log_scale) paste0(gsub(" \\(.*\\)", "", concentration_label), " (natural log scale)") else concentration_label

  has_tp <- "TreatmentPeriod" %in% names(data) && !all(is.na(data$TreatmentPeriod))
  color_col <- if (has_tp) "TreatmentPeriod" else "Treatment"

  subjects <- unique(data$Subject)
  subj_sort_key <- suppressWarnings(as.numeric(as.character(subjects)))
  subjects <- if (!anyNA(subj_sort_key)) subjects[order(subj_sort_key)] else sort(subjects)

  lapply(subjects, function(subj) {
    subj_data <- data[data$Subject == subj, ]
    color_vals <- BIOEQ_COLORS[intersect(names(BIOEQ_COLORS), unique(subj_data[[color_col]]))]

    p <- ggplot(subj_data, aes(x = Time, y = Concentration, color = .data[[color_col]])) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      scale_color_manual(values = color_vals, name = NULL) +
      labs(x = time_label, y = y_lab,
           title = paste("Individual Subject Profile - Subject", subj)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.margin = margin(t = 10, r = 10, b = 40, l = 10, unit = "pt"))
    if (log_scale) p <- p + scale_y_log10()
    p
  })
}
