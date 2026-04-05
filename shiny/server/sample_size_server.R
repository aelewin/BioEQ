# Sample Size Estimation Server
# PowerTOST-based sample size calculator for bioequivalence studies

# ── Update design choices based on selected BE method ──
observeEvent(input$ss_method, {
  method <- input$ss_method
  
  if (method == "ABE") {
    design_choices <- list(
      "2\u00d72\u00d72 Crossover (TR|RT)" = "2x2x2",
      "Parallel" = "parallel",
      "2\u00d72\u00d73 Replicate" = "2x2x3",
      "2\u00d72\u00d74 Full Replicate" = "2x2x4",
      "2\u00d73\u00d73 Partial Replicate" = "2x3x3",
      "Paired" = "paired"
    )
    selected_design <- "2x2x2"
  } else if (method %in% c("ABEL", "RSABE")) {
    # Scaled methods require replicate designs
    design_choices <- list(
      "2\u00d72\u00d73 Replicate" = "2x2x3",
      "2\u00d72\u00d74 Full Replicate" = "2x2x4",
      "2\u00d73\u00d73 Partial Replicate" = "2x3x3"
    )
    selected_design <- "2x3x3"
  } else if (method == "NTID") {
    design_choices <- list(
      "2\u00d72\u00d74 Full Replicate" = "2x2x4",
      "2\u00d72\u00d73 Replicate" = "2x2x3",
      "2\u00d73\u00d73 Partial Replicate" = "2x3x3"
    )
    selected_design <- "2x2x4"
  }
  
  updateSelectInput(session, "ss_design", choices = design_choices, selected = selected_design)
  
  # Update theta0 defaults per method
  if (method == "ABE") {
    updateNumericInput(session, "ss_theta0", value = 0.95)
    updateNumericInput(session, "ss_theta1", value = 0.80)
    updateNumericInput(session, "ss_theta2", value = 1.25)
  } else if (method %in% c("ABEL", "RSABE")) {
    updateNumericInput(session, "ss_theta0", value = 0.90)
    updateNumericInput(session, "ss_theta1", value = 0.80)
    updateNumericInput(session, "ss_theta2", value = 1.25)
  } else if (method == "NTID") {
    updateNumericInput(session, "ss_theta0", value = 0.975)
    updateNumericInput(session, "ss_theta1", value = 0.80)
    updateNumericInput(session, "ss_theta2", value = 1.25)
  }
})

# ── Reactive: store last successful result ──
ss_result <- reactiveVal(NULL)

# ── Calculate sample size ──
observeEvent(input$calculate_ss, {
  req(input$ss_cv, input$ss_theta0, input$ss_target_power, input$ss_alpha)
  
  method <- input$ss_method
  design <- input$ss_design
  cv <- input$ss_cv
  theta0 <- input$ss_theta0
  theta1 <- input$ss_theta1
  theta2 <- input$ss_theta2
  target_power <- input$ss_target_power
  alpha <- input$ss_alpha
  
  # Input validation
  if (cv <= 0 || cv > 2) {
    showNotification("CV must be between 0.01 and 2.0", type = "error")
    return()
  }
  if (target_power <= 0 || target_power >= 1) {
    showNotification("Target power must be between 0 and 1", type = "error")
    return()
  }
  if (theta1 >= theta2) {
    showNotification("Lower BE limit must be less than upper BE limit", type = "error")
    return()
  }
  
  tryCatch({
    result <- NULL
    console_output <- NULL
    
    if (method == "ABE") {
      console_output <- capture.output({
        result <- PowerTOST::sampleN.TOST(
          alpha = alpha,
          targetpower = target_power,
          logscale = TRUE,
          theta0 = theta0,
          theta1 = theta1,
          theta2 = theta2,
          CV = cv,
          design = design,
          print = TRUE,
          details = FALSE
        )
      })
    } else if (method == "ABEL") {
      regulator <- input$ss_regulator %||% "EMA"
      console_output <- capture.output({
        result <- PowerTOST::sampleN.scABEL(
          alpha = alpha,
          targetpower = target_power,
          theta0 = theta0,
          CV = cv,
          design = design,
          regulator = regulator,
          print = TRUE,
          details = FALSE
        )
      })
    } else if (method == "RSABE") {
      console_output <- capture.output({
        result <- PowerTOST::sampleN.RSABE(
          alpha = alpha,
          targetpower = target_power,
          theta0 = theta0,
          CV = cv,
          design = design,
          print = TRUE,
          details = FALSE
        )
      })
    } else if (method == "NTID") {
      console_output <- capture.output({
        result <- PowerTOST::sampleN.NTID(
          alpha = alpha,
          targetpower = target_power,
          theta0 = theta0,
          CV = cv,
          design = design,
          print = TRUE,
          details = FALSE
        )
      })
    }
    
    # Store result for power curve
    ss_result(list(
      data = result,
      console = console_output,
      method = method,
      design = design,
      cv = cv,
      theta0 = theta0,
      theta1 = theta1,
      theta2 = theta2,
      alpha = alpha,
      target_power = target_power,
      regulator = if (method == "ABEL") input$ss_regulator else NULL
    ))
    
    showNotification("Sample size calculated successfully!", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(
      paste("Calculation error:", e$message), 
      type = "error", 
      duration = 8
    )
  })
})

# ── Render sample size result ──
output$ss_result_display <- renderUI({
  res <- ss_result()
  
  if (is.null(res)) {
    return(
      div(
        style = "text-align: center; padding: 40px; color: #a0aec0;",
        icon("calculator", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Configure parameters and click Calculate", style = "color: #6c757d;"),
        p("Results will appear here.", style = "font-size: 14px;")
      )
    )
  }
  
  result_df <- res$data
  console_text <- paste(res$console, collapse = "\n")
  
  # Extract key values
  n_total <- result_df[["Sample size"]]
  achieved_power <- result_df[["Achieved power"]]
  
  # Method label
  method_labels <- c(
    "ABE" = "Average Bioequivalence",
    "ABEL" = "Scaled ABE — Expanding Limits",
    "RSABE" = "Reference-Scaled ABE (FDA)",
    "NTID" = "Narrow Therapeutic Index Drugs"
  )
  
  tagList(
    # Summary card
    div(
      style = "background: linear-gradient(135deg, #d5f4e6, #e8f8f0); border-radius: 12px; 
               padding: 20px; margin-bottom: 20px; border: 1px solid #27ae60;",
      fluidRow(
        column(4,
          div(style = "text-align: center;",
            h2(n_total, style = "color: #27ae60; margin: 0; font-size: 42px; font-weight: 700;"),
            p("Total Subjects", style = "color: #2d6a4f; font-weight: 600; margin: 0;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h2(sprintf("%.1f%%", achieved_power * 100), 
               style = "color: #3182ce; margin: 0; font-size: 42px; font-weight: 700;"),
            p("Achieved Power", style = "color: #1e3a5f; font-weight: 600; margin: 0;")
          )
        ),
        column(4,
          div(style = "text-align: center;",
            h2(sprintf("%.0f%%", res$cv * 100), 
               style = "color: #805ad5; margin: 0; font-size: 42px; font-weight: 700;"),
            p("Within-Subject CV", style = "color: #553c9a; font-weight: 600; margin: 0;")
          )
        )
      )
    ),
    
    # Details
    div(
      style = "background: #f8f9fa; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
      h5(icon("info-circle"), " Calculation Details", style = "color: #2d3748; margin-top: 0;"),
      fluidRow(
        column(6,
          tags$table(
            style = "width: 100%; font-size: 14px;",
            tags$tr(tags$td("Method:", style = "color: #6c757d; padding: 3px 0;"), 
                    tags$td(tags$strong(method_labels[res$method]), style = "padding: 3px 0;")),
            tags$tr(tags$td("Design:", style = "color: #6c757d; padding: 3px 0;"), 
                    tags$td(tags$strong(res$design), style = "padding: 3px 0;")),
            tags$tr(tags$td("T/R Ratio (\u03b80):", style = "color: #6c757d; padding: 3px 0;"), 
                    tags$td(tags$strong(sprintf("%.4f", res$theta0)), style = "padding: 3px 0;"))
          )
        ),
        column(6,
          tags$table(
            style = "width: 100%; font-size: 14px;",
            tags$tr(tags$td("BE Limits:", style = "color: #6c757d; padding: 3px 0;"), 
                    tags$td(tags$strong(sprintf("%.2f \u2013 %.2f", res$theta1, res$theta2)), style = "padding: 3px 0;")),
            tags$tr(tags$td("Alpha:", style = "color: #6c757d; padding: 3px 0;"), 
                    tags$td(tags$strong(sprintf("%.2f", res$alpha)), style = "padding: 3px 0;")),
            if (!is.null(res$regulator)) 
              tags$tr(tags$td("Regulator:", style = "color: #6c757d; padding: 3px 0;"), 
                      tags$td(tags$strong(res$regulator), style = "padding: 3px 0;"))
          )
        )
      )
    ),
    
    # Console output (collapsible)
    tags$details(
      style = "margin-top: 10px;",
      tags$summary(
        style = "cursor: pointer; color: #3498db; font-weight: 600;",
        icon("terminal"), " PowerTOST Console Output"
      ),
      tags$pre(
        style = "background: #1e293b; color: #e2e8f0; border-radius: 8px; padding: 15px; 
                 margin-top: 10px; font-size: 13px; max-height: 300px; overflow-y: auto;",
        console_text
      )
    )
  )
})

# ── Power Curve Plot ──
output$ss_power_curve <- renderPlot({
  res <- ss_result()
  
  if (is.null(res)) {
    # Empty placeholder
    par(mar = c(4, 4, 2, 1))
    plot.new()
    text(0.5, 0.5, "Run a calculation to see the power curve", 
         col = "#a0aec0", cex = 1.3, font = 3)
    return()
  }
  
  method <- res$method
  design <- res$design
  cv <- res$cv
  theta0 <- res$theta0
  theta1 <- res$theta1
  theta2 <- res$theta2
  alpha <- res$alpha
  target_power <- res$target_power
  n_calculated <- res$data[["Sample size"]]
  
  # Generate range of sample sizes to evaluate
  n_min <- max(4, floor(n_calculated * 0.3))
  n_max <- ceiling(n_calculated * 2.5)
  n_seq <- seq(n_min, n_max, by = 2)
  
  # Ensure calculated N is in the sequence
  if (!(n_calculated %in% n_seq)) {
    n_seq <- sort(unique(c(n_seq, n_calculated)))
  }
  
  # Calculate power for each sample size
  power_values <- sapply(n_seq, function(n) {
    tryCatch({
      if (method == "ABE") {
        p <- PowerTOST::power.TOST(
          alpha = alpha, n = n, logscale = TRUE,
          theta0 = theta0, theta1 = theta1, theta2 = theta2,
          CV = cv, design = design
        )
      } else if (method == "ABEL") {
        regulator <- res$regulator %||% "EMA"
        p <- PowerTOST::power.scABEL(
          alpha = alpha, n = n,
          theta0 = theta0, CV = cv,
          design = design, regulator = regulator
        )
      } else if (method == "RSABE") {
        p <- PowerTOST::power.RSABE(
          alpha = alpha, n = n,
          theta0 = theta0, CV = cv,
          design = design
        )
      } else if (method == "NTID") {
        p <- PowerTOST::power.NTID(
          alpha = alpha, n = n,
          theta0 = theta0, CV = cv,
          design = design
        )
      }
      return(p)
    }, error = function(e) NA_real_)
  })
  
  # Remove NAs
  valid <- !is.na(power_values)
  n_seq <- n_seq[valid]
  power_values <- power_values[valid]
  
  if (length(n_seq) == 0) return()
  
  # Plot
  par(mar = c(4.5, 4.5, 2, 1), family = "sans")
  
  plot(n_seq, power_values * 100, type = "l",
       xlab = "Total Sample Size (N)", ylab = "Power (%)",
       main = "",
       col = "#3182ce", lwd = 3,
       xlim = range(n_seq),
       ylim = c(0, 100),
       las = 1, bty = "l",
       cex.lab = 1.2, cex.axis = 1.0)
  
  # Grid
  abline(h = seq(0, 100, 10), col = "#e2e8f0", lty = 1, lwd = 0.5)
  abline(v = pretty(n_seq), col = "#e2e8f0", lty = 1, lwd = 0.5)
  
  # Re-draw line on top of grid
  lines(n_seq, power_values * 100, col = "#3182ce", lwd = 3)
  
  # Target power line
  abline(h = target_power * 100, col = "#e53e3e", lty = 2, lwd = 2)
  
  # Calculated N line
  abline(v = n_calculated, col = "#38a169", lty = 2, lwd = 2)
  
  # Point at calculated N
  achieved <- res$data[["Achieved power"]] * 100
  points(n_calculated, achieved, pch = 19, col = "#38a169", cex = 2)
  
  # Labels
  text(n_calculated, achieved + 4,
       labels = sprintf("N = %d\n%.1f%%", n_calculated, achieved),
       col = "#38a169", font = 2, cex = 0.95, pos = 4)
  
  text(max(n_seq), target_power * 100 + 3,
       labels = sprintf("Target: %.0f%%", target_power * 100),
       col = "#e53e3e", font = 2, cex = 0.85, pos = 2)
  
  # 80% and 90% reference lines if not already the target
  common_powers <- c(0.80, 0.90)
  for (cp in common_powers) {
    if (abs(cp - target_power) > 0.01) {
      abline(h = cp * 100, col = "#a0aec0", lty = 3, lwd = 1)
      text(min(n_seq), cp * 100 + 2,
           labels = sprintf("%.0f%%", cp * 100),
           col = "#a0aec0", cex = 0.75, pos = 4)
    }
  }
  
}, res = 120)

# ── CV Back-Calculator ──
observeEvent(input$calculate_cv, {
  req(input$cv_calc_lower, input$cv_calc_upper, input$cv_calc_n)
  
  lower <- input$cv_calc_lower / 100  # Convert from percentage
  upper <- input$cv_calc_upper / 100
  n <- input$cv_calc_n
  design <- input$cv_calc_design
  
  # Point estimate (optional)
  pe <- if (!is.na(input$cv_calc_pe) && input$cv_calc_pe > 0) {
    input$cv_calc_pe / 100
  } else {
    NULL
  }
  
  if (lower >= upper) {
    showNotification("Lower CI limit must be less than upper CI limit", type = "error")
    return()
  }
  
  tryCatch({
    cv_est <- PowerTOST::CVfromCI(
      pe = if (!is.null(pe)) pe else NULL,
      lower = lower,
      upper = upper,
      n = n,
      design = design,
      alpha = 0.05
    )
    
    output$cv_result_display <- renderUI({
      div(
        style = "background: linear-gradient(135deg, #d6eaf8, #e8f4fd); border-radius: 10px; 
                 padding: 15px; margin-top: 15px; border: 1px solid #3498db;",
        div(style = "text-align: center;",
          h3(sprintf("%.2f%%", cv_est * 100), 
             style = "color: #2980b9; margin: 5px 0; font-weight: 700; font-size: 32px;"),
          p("Estimated Within-Subject CV", 
            style = "color: #2c3e50; font-weight: 600; margin: 0;"),
          p(sprintf("(CV = %.4f)", cv_est),
            style = "color: #6c757d; font-size: 12px; margin-top: 5px;")
        ),
        hr(style = "margin: 10px 0; border-color: #bee3f8;"),
        div(style = "text-align: center;",
          actionButton(
            "use_cv_estimate",
            label = "Use this CV",
            icon = icon("arrow-up"),
            class = "btn-outline-primary btn-sm",
            style = "font-weight: 600;"
          )
        )
      )
    })
    
    showNotification(sprintf("Estimated CV: %.4f (%.1f%%)", cv_est, cv_est * 100), 
                     type = "message", duration = 5)
    
  }, error = function(e) {
    showNotification(paste("CV calculation error:", e$message), type = "error", duration = 8)
  })
})

# ── Transfer estimated CV to main inputs ──
observeEvent(input$use_cv_estimate, {
  req(input$cv_calc_lower, input$cv_calc_upper, input$cv_calc_n)
  
  lower <- input$cv_calc_lower / 100
  upper <- input$cv_calc_upper / 100
  n <- input$cv_calc_n
  design <- input$cv_calc_design
  pe <- if (!is.na(input$cv_calc_pe) && input$cv_calc_pe > 0) input$cv_calc_pe / 100 else NULL
  
  tryCatch({
    cv_est <- PowerTOST::CVfromCI(
      pe = pe, lower = lower, upper = upper,
      n = n, design = design, alpha = 0.05
    )
    updateNumericInput(session, "ss_cv", value = round(cv_est, 4))
    showNotification("CV value transferred to sample size inputs", type = "message")
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})


