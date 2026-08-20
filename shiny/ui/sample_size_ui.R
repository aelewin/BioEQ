# Sample Size Estimation UI
# PowerTOST-based sample size calculator for bioequivalence studies

tagList(
  div(
    class = "sample-size-header",
    style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
    h3(icon("calculator"), " Sample Size Estimation",
       style = "margin: 0; font-weight: 700;"),
    p("PowerTOST-based sample size calculator for BE studies.",
      style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
  ),
  
  fluidRow(
    # ── Left panel: Inputs ──
    column(
      width = 4,
      
      # Main parameters box
      box(
        title = "Study Parameters",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        icon = icon("sliders-h"),
        
        selectInput(
          "ss_method",
          label = "BE Method",
          choices = list(
            "Average BE (ABE)" = "ABE",
            "Scaled ABE — EMA/HC (ABEL)" = "ABEL",
            "Reference-Scaled ABE — FDA (RSABE)" = "RSABE",
            "Narrow Therapeutic Index (NTID)" = "NTID"
          ),
          selected = "ABE"
        ),
        
        selectInput(
          "ss_design",
          label = "Study Design",
          choices = list(
            "2\u00d72\u00d72 Crossover (TR|RT)" = "2x2x2",
            "Parallel" = "parallel"
          ),
          selected = "2x2x2"
        ),
        
        # Regulator (only for scaled methods)
        conditionalPanel(
          condition = "input.ss_method == 'ABEL'",
          selectInput(
            "ss_regulator",
            label = "Regulatory Agency",
            choices = list(
              "EMA (European)" = "EMA",
              "Health Canada" = "HC",
              "GCC (Gulf)" = "GCC"
            ),
            selected = "EMA"
          )
        ),
        
        hr(),
        
        numericInput(
          "ss_cv",
          label = "Within-Subject CV",
          value = 0.30,
          min = 0.01,
          max = 2.0,
          step = 0.01
        ),
        div(style = "color: #6c757d; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          "Enter as fraction (e.g. 0.30 = 30%)"
        ),
        
        numericInput(
          "ss_theta0",
          label = "Assumed T/R Ratio (\u0394)",
          value = 0.95,
          min = 0.50,
          max = 1.50,
          step = 0.01
        ),
        
        numericInput(
          "ss_target_power",
          label = "Target Power (1 \u2212 \u03b2)",
          value = 0.80,
          min = 0.50,
          max = 0.99,
          step = 0.05
        ),

        numericInput(
          "ss_dropout_pct",
          label = "Anticipated Dropout Rate (%)",
          value = 0,
          min = 0,
          max = 50,
          step = 1
        ),

        # Collapsible advanced options
        tags$details(
          tags$summary(
            style = "cursor: pointer; color: #3498db; font-weight: 600; margin-bottom: 10px;",
            icon("cog"), " Advanced Options"
          ),
          
          numericInput(
            "ss_alpha",
            label = "Significance Level (\u03b1)",
            value = 0.05,
            min = 0.01,
            max = 0.10,
            step = 0.01
          ),
          
          numericInput(
            "ss_theta1",
            label = "Lower BE Limit (\u03b81)",
            value = 0.80,
            min = 0.50,
            max = 1.00,
            step = 0.01
          ),
          
          numericInput(
            "ss_theta2",
            label = "Upper BE Limit (\u03b82)",
            value = 1.25,
            min = 1.00,
            max = 2.00,
            step = 0.01
          )
        ),
        
        br(),
        
        actionButton(
          "calculate_ss",
          label = "Calculate Sample Size",
          icon = icon("calculator"),
          class = "btn-success btn-block",
          style = "font-weight: 600; padding: 10px;"
        )
      ),
      
      # CV Back-Calculator box
      box(
        title = "CV Back-Calculator",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        icon = icon("exchange-alt"),
        
        p("Estimate CV from a published confidence interval.", 
          style = "color: #6c757d; font-size: 13px;"),
        
        numericInput(
          "cv_calc_lower",
          label = "Lower 90% CI Limit (%)",
          value = 85.0,
          min = 50,
          max = 125,
          step = 0.1
        ),
        
        numericInput(
          "cv_calc_upper",
          label = "Upper 90% CI Limit (%)",
          value = 115.0,
          min = 80,
          max = 200,
          step = 0.1
        ),
        
        numericInput(
          "cv_calc_pe",
          label = "Point Estimate (%, optional)",
          value = NA,
          min = 50,
          max = 200,
          step = 0.1
        ),
        
        numericInput(
          "cv_calc_n",
          label = "Total Sample Size (N)",
          value = 24,
          min = 4,
          max = 500,
          step = 1
        ),
        
        selectInput(
          "cv_calc_design",
          label = "Study Design",
          choices = list(
            "2\u00d72\u00d72 Crossover" = "2x2x2",
            "Parallel" = "parallel",
            "2\u00d72\u00d73 Replicate" = "2x2x3",
            "2\u00d72\u00d74 Replicate" = "2x2x4",
            "2\u00d73\u00d73 Partial Replicate" = "2x3x3"
          ),
          selected = "2x2x2"
        ),
        
        actionButton(
          "calculate_cv",
          label = "Calculate CV",
          icon = icon("sync"),
          class = "btn-info btn-block",
          style = "font-weight: 600;"
        ),
        
        uiOutput("cv_result_display")
      )
    ),
    
    # ── Right panel: Results ──
    column(
      width = 8,
      
      # Results box
      box(
        title = "Sample Size Result",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        icon = icon("table"),

        uiOutput("ss_result_display"),
        conditionalPanel(
          condition = "output.ss_has_result",
          div(style = "margin-top: 12px; text-align: right;",
            downloadButton("ss_download_report",
              label = "Download Report (HTML)",
              icon = icon("file-code"),
              class = "btn-outline-success btn-sm")
          )
        )
      ),
      
      # Power Curve box
      box(
        title = "Power Curve",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        icon = icon("chart-line"),
        collapsible = TRUE,
        collapsed = FALSE,
        
        plotOutput("ss_power_curve", height = "400px"),
        
        div(style = "color: #6c757d; font-size: 12px; margin-top: 8px; text-align: center;",
          "Power vs. total sample size. Dashed lines show target power and calculated N."
        )
      )
    )
  )
)
