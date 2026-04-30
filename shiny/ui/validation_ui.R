# Validation Module UI
#
# Group-centric selection: each row in the main table is a "validation group"
# representing a coherent set of reference datasets that target one analysis
# capability (e.g. parallel-group BE, 2x2 crossover BE, replicate ABEL, oral
# NCA, IV NCA). Selecting a group runs every dataset bundled inside it.
#
# Sections:
#   1. Header + "Run Complete Validation Set" / "Run Selected Groups" buttons
#   2. Groups selection table with citation links and download access
#   3. Selected-group detail panel (citation, link, # datasets, BioEQ download)
#   4. Methodology explanation card with dataset -> purpose mapping tables
#   5. Results panels (per dataset) and CSV/HTML/PDF download

validation_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      .validation-status-PASS         { color: #27ae60; font-weight: 700; }
      .validation-status-FAIL         { color: #c0392b; font-weight: 700; }
      .validation-status-NO_REFERENCE { color: #d35400; font-weight: 600; }
      .validation-status-DATA_MISSING { color: #7f8c8d; font-weight: 600; font-style: italic; }
      .validation-status-ERROR        { color: #c0392b; font-weight: 700; font-style: italic; }
      .validation-status-NOT_RUN      { color: #7f8c8d; }
      .validation-summary-box {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        padding: 12px 16px;
        margin-bottom: 16px;
      }
      .validation-dataset-card { border-left: 4px solid #cbd5e0; margin-bottom: 12px; }
      .validation-dataset-card.PASS         { border-left-color: #27ae60; }
      .validation-dataset-card.FAIL         { border-left-color: #c0392b; }
      .validation-dataset-card.NO_REFERENCE { border-left-color: #d35400; }
      .validation-dataset-card.DATA_MISSING { border-left-color: #95a5a6; }
      .validation-dataset-card.ERROR        { border-left-color: #c0392b; }
      .validation-group-detail {
        background: #f8fafc;
        border: 1px solid #cbd5e0;
        border-radius: 6px;
        padding: 14px 18px;
        margin-top: 8px;
      }
      .validation-group-detail h4 { margin-top: 0; }
      .validation-source-bioeq {
        display: inline-block;
        padding: 2px 8px;
        background: #2563eb;
        color: white;
        border-radius: 4px;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      .validation-source-public {
        display: inline-block;
        padding: 2px 8px;
        background: #16a34a;
        color: white;
        border-radius: 4px;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      table.bioeq-mapping {
        border-collapse: collapse;
        width: 100%;
        font-size: 13px;
        margin: 8px 0 16px 0;
      }
      table.bioeq-mapping th, table.bioeq-mapping td {
        border: 1px solid #cbd5e0;
        padding: 6px 10px;
        text-align: left;
        vertical-align: top;
      }
      table.bioeq-mapping th { background: #edf2f7; }
      table.bioeq-mapping tr:nth-child(even) td { background: #fafafa; }
    "))),

    # ---- Header / Run All ----------------------------------------------
    fluidRow(
      box(
        title = tagList(icon("shield-alt"), " Validation"),
        status = "success", solidHeader = TRUE, width = 12,
        fluidRow(
          column(width = 8,
            p("Verify that BioEQ produces correct results by comparing its output against ",
              "reference data sets targeting analysis capabilities of the application ",
              "(parallel BE, 2x2 crossover BE, replicate ABEL, oral / IV NCA)."),
            p("Re-run after installation, package updates, R upgrades, or BioEQ code changes.")
          ),
          column(width = 4,
            div(style = "text-align: right; padding-top: 10px;",
              actionButton("validation_run_all",
                           label = tagList(icon("play-circle"), " Run Validation Data Sets"),
                           class = "btn-success btn-lg",
                           style = "width: 100%;")
            )
          )
        )
      )
    ),

    # ---- Groups table --------------------------------------------------
    fluidRow(
      box(
        title = tagList(icon("layer-group"), " Reference Data Sets"),
        status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        DT::DTOutput("validation_groups_table"),
        br(),
        helpText(
          "Click a row to view group details and citation / download links. ",
          tags$span(class = "validation-source-public", "PUBLIC"),
          " sources link out to the original reference data; ",
          tags$span(class = "validation-source-bioeq", "BioEQ"),
          " reference sets are bundled with this application and downloadable below."
        ),
        # Selected-group detail panel
        uiOutput("validation_group_detail")
      )
    ),

    # ---- Methodology + dataset->purpose mapping tables -----------------
    fluidRow(
      box(
        title = tagList(icon("book"), " Validation Coverage Map"),
        status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,

        h4("What every BioEQ analysis is benchmarked against"),
        p("Each validation group targets a specific module of BioEQ. The two ",
          "tables below map the datasets to (a) the BioEQ functions they ",
          "exercise, and (b) the equivalent procedures in the reference ",
          "software stack (SAS PROC GLM/MIXED/TTEST, Phoenix WinNonlin, and the ",
          "CRAN ", code("replicateBE"), " / ", code("PowerTOST"), " packages)."),

        h4("Table 1 - NCA validation groups"),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr>",
          "<th>BioEQ function</th>",
          "<th>Embedded R package</th>",
          "<th>SAS function equivalent</th>",
          "<th>Phoenix WinNonlin equivalent</th>",
          "</tr></thead><tbody>",

          "<tr><td><code>perform_enhanced_nca_analysis()</code><br>",
          "<code>calculate_pk_parameters()</code> (oral / extravascular branch)</td>",
          "<td><code>PKNCA::pk.nca()</code>, <code>NonCompart::tblNCA()</code></td>",
          "<td><code>PROC NLMIXED</code> / NCA macros (e.g. %nca, %lambdaz)</td>",
          "<td>NCA Model 200 (extravascular, plasma)</td></tr>",

          "<tr><td><code>perform_enhanced_nca_analysis()</code> IV branch<br>",
          "<code>calculate_pk_parameters()</code> with IV-specific C0 logic</td>",
          "<td><code>PKNCA::pk.nca()</code> with <code>route='intravascular'</code></td>",
          "<td><code>PROC NLMIXED</code> / IV NCA macros</td>",
          "<td>NCA Model 201 (IV bolus, plasma)</td></tr>",

          "</tbody></table>"
        ),

        h4("Table 2 - BE validation groups"),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr>",
          "<th>BioEQ function</th>",
          "<th>Embedded R package</th>",
          "<th>SAS function equivalent</th>",
          "<th>Phoenix WinNonlin equivalent</th>",
          "</tr></thead><tbody>",

          "<tr><td><code>perform_be_analysis()</code> Parallel branch<br>",
          "<code>simple_anova.R</code> / <code>statistics.R</code></td>",
          "<td><code>stats::t.test()</code> (Welch + classical), <code>stats::lm()</code></td>",
          "<td><code>PROC TTEST</code> (Welch) / <code>PROC GLM</code></td>",
          "<td>Bioequivalence Wizard - Parallel design</td></tr>",

          "<tr><td><code>perform_be_analysis()</code> 2x2 branch<br>",
          "<code>be_analysis.R</code>, <code>statistics.R</code>, <code>carryover_detection.R</code></td>",
          "<td><code>stats::lm()</code>, <code>PowerTOST</code></td>",
          "<td><code>PROC GLM</code> with <code>RANDOM subject(sequence)</code></td>",
          "<td>Bioequivalence Wizard - 2x2 Crossover (Classical)</td></tr>",

          "<tr><td><code>perform_be_analysis()</code> Replicate branch<br>",
          "<code>rsabe_analysis.R</code>, <code>statistics.R</code></td>",
          "<td><code>replicateBE::method.A()</code> / <code>method.B()</code>, <code>lme4</code>, <code>lmerTest</code>, <code>pbkrtest</code></td>",
          "<td><code>PROC GLM</code> (Method A) + <code>PROC MIXED</code> with <code>DDFM=KENWARDROGER</code> (Method B)</td>",
          "<td>Bioequivalence Wizard - Reference-scaled / Replicate</td></tr>",

          "</tbody></table>"
        ),

        h4("Re-run frequency"),
        p("Re-run at minimum after: (a) initial installation, (b) any update ",
          "to the BioEQ source code, (c) any update to a key R package ",
          "(especially ", code("nlme"), ", ", code("lme4"), ", ",
          code("lmerTest"), ", ", code("replicateBE"), ", ", code("PowerTOST"),
          "), and (d) any R or OS upgrade. A scheduled quarterly re-run is ",
          "recommended for production deployments.")
      )
    ),

    # ---- Results panel -------------------------------------------------
    fluidRow(
      box(
        title = tagList(icon("clipboard-check"), " Validation Results"),
        status = "warning", solidHeader = TRUE, width = 12,
        uiOutput("validation_run_summary"),
        hr(),
        uiOutput("validation_results_panels"),
        br(),
        conditionalPanel(
          condition = "output.validation_has_results",
          fluidRow(
            column(width = 4,
              downloadButton("validation_download_csv",
                             label = "Download CSV", class = "btn-default", style = "width: 100%;")
            ),
            column(width = 4,
              downloadButton("validation_download_html",
                             label = "Download HTML report", class = "btn-default", style = "width: 100%;")
            ),
            column(width = 4,
              downloadButton("validation_download_pdf",
                             label = "Download PDF report", class = "btn-default", style = "width: 100%;")
            )
          )
        )
      )
    )
  )
}
