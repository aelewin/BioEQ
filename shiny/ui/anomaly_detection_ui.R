# Anomaly Detection UI Module
# Shiny module providing the multi-tab interface for fraud / anomaly detection.

anomaly_detection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "anomaly-header",
      style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
      h3(icon("triangle-exclamation"), " Anomaly Detection",
         style = "margin: 0; font-weight: 700;"),
      p("Pairwise comparison, trend analysis, and distributional checks to flag potentially anomalous BE data.",
        style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
    ),

    tabsetPanel(
      id = ns("ad_tabs"), type = "tabs",

      # ----------------------------------------------------------------------
      # Tab 1: Data Selection
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("database"), " Data Selection"),
        br(),
        fluidRow(
          box(
            title = "Source Dataset", status = "primary", solidHeader = TRUE,
            width = 5,
            radioButtons(
              ns("data_source"),
              label = "Use which dataset?",
              choices = list(
                "Currently uploaded BE dataset" = "shared",
                "Upload a separate dataset for fraud screening" = "upload"
              ),
              selected = "shared"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
              fileInput(ns("ad_file"), "Concentration-time file",
                        accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
              helpText("Required columns: Subject, Time, Concentration. ",
                       "Optional: Treatment, Period, Sequence.")
            ),
            hr(),
            uiOutput(ns("data_status"))
          ),
          box(
            title = "Profile Filtering", status = "info", solidHeader = TRUE,
            width = 7,
            uiOutput(ns("treatment_filter_ui")),
            checkboxInput(ns("exclude_blq"),
                          "Exclude below-LLOQ values from comparisons",
                          value = TRUE),
            numericInput(ns("lloq_value"),
                         "LLOQ value (concentrations \u2264 are excluded)",
                         value = 0, min = 0, step = 0.1),
            hr(),
            uiOutput(ns("profile_summary"))
          )
        ),
        fluidRow(
          box(
            title = "Profile Preview", status = "warning", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput(ns("profile_preview"))
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab 2: Pairwise Comparison
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("table-cells"), " Pairwise Comparison"),
        br(),
        fluidRow(
          box(
            title = "Comparison Selection", status = "primary", solidHeader = TRUE,
            width = 4,
            tags$div(
              style = "display: flex; align-items: center; gap: 6px;",
              tags$label("Comparison type", style = "font-weight: 600; margin: 0;"),
              actionLink(ns("pair_help"),
                         label = NULL,
                         icon  = icon("circle-question"),
                         style = "color: #2c5282; font-size: 16px;",
                         title = "Show details for the selected comparison")
            ),
            radioButtons(
              ns("pair_analysis"),
              label = NULL,
              choices = list(
                "Overlapping duplicates"   = "overlap",
                "Scaled duplicates"        = "scaled",
                "Time-shifted duplicates"  = "lag",
                "Dynamic pattern match"    = "dynamics"
              ),
              selected = "overlap"
            ),
            hr(),
            sliderInput(ns("top_n_pairs"),
                        "Show top N pairs:",
                        min = 10, max = 200, value = 50, step = 10),
            actionButton(ns("run_pairwise"), "Run Comparison",
                         icon = icon("play"), class = "btn-primary btn-block")
          ),
          box(
            title = "Ranked Pair Table", status = "warning", solidHeader = TRUE,
            width = 8,
            p("Click a row to overlay the two profiles below.",
              style = "color: #4a5568; font-size: 13px;"),
            DT::dataTableOutput(ns("pair_table"))
          )
        ),
        fluidRow(
          box(
            title = "Profile Overlay (selected pair)", status = "success",
            solidHeader = TRUE, width = 12,
            plotly::plotlyOutput(ns("pair_overlay"), height = "420px")
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab: Trend Analysis (cumulative BE evolution)
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("chart-line"), " Trend Analysis"),
        br(),
        fluidRow(
          box(
            title = "Cumulative BE Evolution", status = "primary",
            solidHeader = TRUE, width = 4,
            selectInput(ns("trend_parameter"), "PK parameter",
                        choices = c("Cmax", "AUC0t"), selected = "Cmax"),
            radioButtons(ns("trend_order_mode"), "Subject ordering",
                         choices = list(
                           "Subject ID (ascending)" = "id",
                           "Custom order" = "custom"
                         ),
                         selected = "id"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("trend_order_mode")),
              textInput(ns("trend_custom_order"),
                        "Comma-separated subject IDs",
                        value = "",
                        placeholder = "e.g. 1, 5, 3, 2, 8...")
            ),
            sliderInput(ns("trend_be_lower"), "BE lower limit (%)",
                        min = 70, max = 90, value = 80, step = 1),
            sliderInput(ns("trend_be_upper"), "BE upper limit (%)",
                        min = 110, max = 143, value = 125, step = 1),
            actionButton(ns("run_trend"), "Run Cumulative Analysis",
                         icon = icon("play"), class = "btn-primary btn-block")
          ),
          box(
            title = "Cumulative T/R Ratio Plot", status = "info",
            solidHeader = TRUE, width = 8,
            plotly::plotlyOutput(ns("trend_plot"), height = "500px"),
            helpText("Blue line: cumulative point estimate. Black lines: 90% CI. Red dashed: BE limits.")
          )
        ),

        # ------------------------------------------------------------------
        # Subgroup ABE
        # ------------------------------------------------------------------
        fluidRow(
          box(
            title = "Subgroup / Exclusion ABE", status = "primary",
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                radioButtons(ns("subgroup_mode"), "Mode",
                             choices = list(
                               "Exclude subjects" = "exclude",
                               "Compare custom subgroups"               = "subgroups"
                             ),
                             selected = "exclude"),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'exclude'", ns("subgroup_mode")),
                  uiOutput(ns("sg_exclude_ui")),
                  helpText("ABE will be run on all subjects not excluded.")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'subgroups'", ns("subgroup_mode")),
                  numericInput(ns("sg_n_groups"), "Number of subgroups",
                               value = 2, min = 1, max = 6, step = 1),
                  uiOutput(ns("sg_groups_ui"))
                ),
                hr(),
                selectInput(ns("sg_parameter"), "PK parameter",
                            choices = c("Cmax", "AUC0t"), selected = "Cmax"),
                sliderInput(ns("sg_be_lower"), "BE lower limit (%)",
                            min = 70, max = 90, value = 80, step = 1),
                sliderInput(ns("sg_be_upper"), "BE upper limit (%)",
                            min = 110, max = 143, value = 125, step = 1),
                actionButton(ns("run_subgroup_abe"), "Run ABE",
                             icon = icon("play"), class = "btn-primary btn-block")
              ),
              column(8,
                DT::dataTableOutput(ns("sg_results_table"))
              )
            )
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab: Distribution Checks
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("chart-column"), " Distribution Checks"),
        br(),
        fluidRow(
          box(
            title = "Within-Subject CV", status = "primary", solidHeader = TRUE,
            width = 6,
            selectInput(ns("cv_metric"), "Metric",
                        choices = c("Cmax", "AUC0t"), selected = "Cmax"),
            plotly::plotlyOutput(ns("cv_plot"), height = "380px"),
            helpText("Implausibly low within-subject CV is a fraud signature.")
          ),
          box(
            title = "Tmax Distribution", status = "info", solidHeader = TRUE,
            width = 6,
            plotly::plotlyOutput(ns("tmax_plot"), height = "380px"),
            helpText("Unnaturally tight Tmax clustering can indicate duplication or label swap.")
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab 6: ISPR Comparison (scaffold)
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("rotate-right"), " ISPR Comparison"),
        br(),
        fluidRow(
          box(
            title = "Incurred Subject Period Re-analysis", status = "primary",
            solidHeader = TRUE, width = 12,
            h4("Coming Soon"),
            p("Architectural support for re-analysed subset comparison, allowing the full pairwise battery to be re-run on a re-analysed subset and overlaid against original score distributions."),
            tags$ul(
              tags$li("Upload original + re-analysed concentration vectors"),
              tags$li("Score-distribution overlay with empirical thresholds"),
              tags$li("Confirmatory check for patterns 1b (composite) and 1c (time-shifted)")
            )
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab: Export
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("download"), " Export"),
        br(),
        fluidRow(
          box(
            title = "Download Pairwise Results", status = "primary",
            solidHeader = TRUE, width = 12,
            p("Export the full pairwise comparison table for the most recently run comparison, including all metric scores and the analysis-specific suspicion score."),
            p(tags$em("Note: scores are diagnostic indicators only. High scores warrant review of the underlying profiles \u2014 they are not, on their own, evidence of duplication or fabrication. Simple PK profiles can legitimately resemble one another."),
              style = "color: #4a5568; font-size: 13px;"),
            downloadButton(ns("download_results"),
                           "Download Full Pairwise Table (CSV)",
                           class = "btn-primary")
          )
        )
      )
    )
  )
}
