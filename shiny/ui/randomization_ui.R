# Randomization UI Module
# Generates, verifies, and exports BE study randomization schedules.

randomization_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "randomization-header",
      style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
      h3(icon("shuffle"), " Randomization",
         style = "margin: 0; font-weight: 700;"),
      p("Permuted-block randomization for BE designs. Reproducible from seed; auditable; verifiable.",
        style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
    ),

    tabsetPanel(
      id = ns("rnd_tabs"), type = "tabs",

      # ----------------------------------------------------------------------
      # Tab 1: Generate
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("dice"), " Generate Schedule"),
        br(),
        fluidRow(
          box(
            title = "Design Parameters", status = "primary", solidHeader = TRUE,
            width = 4,
            actionButton(ns("autofill"),
                         label = tagList(icon("wand-magic-sparkles"),
                                         " Autofill from Sample Size module"),
                         class = "btn-default btn-block",
                         style = "margin-bottom: 8px; font-weight: 600;"),
            uiOutput(ns("autofill_status")),
            hr(),
            selectInput(ns("design"), "Study design:",
                        choices = list(
                          "Parallel (T vs R)"                 = "parallel_2",
                          "2x2 Crossover (TR | RT)"           = "crossover_2x2",
                          "2x2x3 Replicate (TRT | RTR)"       = "replicate_2x2x3",
                          "2x2x4 Full Replicate (TRTR|RTRT)"  = "replicate_2x2x4",
                          "2x3x3 Partial Replicate"           = "partial_2x3x3"
                        ),
                        selected = "crossover_2x2"),
            numericInput(ns("n_total"), "Total sample size (N):",
                         value = 24, min = 2, step = 2),
            checkboxInput(ns("use_blocks"),
                          "Use group (block) randomization",
                          value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_blocks")),
              numericInput(ns("block_size"), "Group size:",
                           value = 4, min = 2, step = 2),
              helpText("Group size must be a positive multiple of the number of sequences. ",
                       "Default if left blank is 2 × number of sequences.")
            ),
            numericInput(ns("seed"), "RNG seed:",
                         value = 20260101, step = 1),
            actionButton(ns("rand_seed"), "Random seed",
                         icon = icon("dice-five"),
                         class = "btn-default btn-sm"),
            hr(),
            checkboxInput(ns("use_strata"), "Stratify randomization", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_strata")),
              textInput(ns("stratum1_name"),   "Stratum 1 name:",   value = "Site"),
              textInput(ns("stratum1_levels"), "Stratum 1 levels (comma-separated):",
                        value = "S1, S2"),
              textInput(ns("stratum2_name"),   "Stratum 2 name (optional):", value = ""),
              textInput(ns("stratum2_levels"), "Stratum 2 levels (comma-separated, optional):",
                        value = "")
            ),
            hr(),
            textInput(ns("subject_prefix"), "Subject ID prefix:", value = "S"),
            br(),
            actionButton(ns("generate"), "Generate Schedule",
                         icon = icon("play"),
                         class = "btn-primary btn-block")
          ),
          box(
            title = "Schedule (per subject)", status = "warning", solidHeader = TRUE,
            width = 8,
            uiOutput(ns("schedule_summary")),
            div(class = "no-top-pad",
                DT::dataTableOutput(ns("schedule_table"))),
            br(),
            downloadButton(ns("dl_csv"),  "Download CSV",
                           icon = icon("file-csv"),
                           class = "btn-default"),
            downloadButton(ns("dl_long"), "Download long-form CSV (per period)",
                           icon = icon("file-csv"),
                           class = "btn-default")
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab 2: Verify
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("circle-check"), " Verify Schedule"),
        br(),
        fluidRow(
          box(
            title = "Verification Inputs", status = "primary", solidHeader = TRUE,
            width = 5,
            p("Enter the parameters from a study's randomization audit record.",
              "BioEQ will regenerate the schedule deterministically and compare to the",
              "schedule you provide."),
            selectInput(ns("v_design"), "Study design:",
                        choices = list(
                          "Parallel (T vs R)"                 = "parallel_2",
                          "2x2 Crossover (TR | RT)"           = "crossover_2x2",
                          "2x2x3 Replicate (TRT | RTR)"       = "replicate_2x2x3",
                          "2x2x4 Full Replicate (TRTR|RTRT)"  = "replicate_2x2x4",
                          "2x3x3 Partial Replicate"           = "partial_2x3x3"
                        ),
                        selected = "crossover_2x2"),
            numericInput(ns("v_n_total"), "Total sample size (N):", value = 24, min = 2, step = 2),
            checkboxInput(ns("v_use_blocks"), "Group (block) randomization was used", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("v_use_blocks")),
              numericInput(ns("v_block_size"), "Group size:", value = 4, min = 2, step = 2)
            ),
            numericInput(ns("v_seed"), "RNG seed:", value = 20260101, step = 1),
            checkboxInput(ns("v_use_strata"), "Stratification was used", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("v_use_strata")),
              textInput(ns("v_stratum1_name"),   "Stratum 1 name:",   value = "Site"),
              textInput(ns("v_stratum1_levels"), "Stratum 1 levels (comma-separated):",
                        value = "S1, S2"),
              textInput(ns("v_stratum2_name"),   "Stratum 2 name (optional):", value = ""),
              textInput(ns("v_stratum2_levels"), "Stratum 2 levels (optional):", value = "")
            ),
            textInput(ns("v_subject_prefix"), "Subject ID prefix:", value = "S"),
            hr(),
            fileInput(ns("v_file"), "Provided schedule (CSV/TSV/XLSX, optional):",
                      accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
            helpText("If omitted, BioEQ will simply show the regenerated schedule."),
            actionButton(ns("verify"), "Verify",
                         icon = icon("magnifying-glass"),
                         class = "btn-primary btn-block")
          ),
          box(
            title = "Verification Result", status = "warning", solidHeader = TRUE,
            width = 7,
            uiOutput(ns("verify_status")),
            br(),
            DT::dataTableOutput(ns("verify_table"))
          )
        )
      ),

      # ----------------------------------------------------------------------
      # Tab 3: Audit / Report
      # ----------------------------------------------------------------------
      tabPanel(
        tags$span(icon("file-shield"), " Report"),
        br(),
        fluidRow(
          box(
            title = NULL, status = "primary", solidHeader = FALSE,
            width = 12,
            verbatimTextOutput(ns("audit_text")),
            downloadButton(ns("dl_audit"),  "Download audit text (.txt)",
                           class = "btn-default"),
            downloadButton(ns("dl_report"), "Download randomization report (.html)",
                           icon = icon("file-lines"),
                           class = "btn-primary")
          )
        )
      )
    )
  )
}
