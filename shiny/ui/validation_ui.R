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

    # ---- Header --------------------------------------------------------
    div(
      class = "validation-header",
      style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
      h3(icon("shield-alt"), " Validation",
         style = "margin: 0; font-weight: 700;"),
      p("BioEQ's two engines are validated against two different reference standards: the NCA engine against Phoenix WinNonlin, and the ANOVA/BE engine against SAS.",
        style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
    ),

    # ---- Run All -------------------------------------------------------
    fluidRow(
      box(
        width = 12,
        div(style = "text-align: right;",
          actionButton("validation_run_all",
                       label = tagList(icon("play-circle"), " Run Validation Data Sets"),
                       class = "btn-success btn-lg",
                       style = "width: 100%;")
        )
      )
    ),

    # ---- Methodology + dataset->purpose mapping tables -----------------
    fluidRow(
      box(
        title = "Validation Coverage Map",
        status = "info", solidHeader = TRUE, width = 12,
        collapsible = TRUE, collapsed = TRUE,

        h4("What every BioEQ analysis is benchmarked against"),
        p("NCA is benchmarked against ", strong("Phoenix WinNonlin"), "; ANOVA/BE against ", strong("SAS"),
          ". Tables below show the calculation, its SAS/WinNonlin equivalent, and which datasets ",
          "have a published SAS/WinNonlin result to compare against."),

        h4("Table 1 - NCA"),
        p(class = "text-muted", style = "font-size: 12px;",
          "Computed by the ", code("PKNCA"), " CRAN package. TTT point selection has no PKNCA ",
          "equivalent — BioEQ selects the points, PKNCA fits the regression."),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr><th>Calculation</th><th>BioEQ package/call</th><th>WinNonlin equivalent</th><th>Validating dataset</th></tr></thead><tbody>",
          "<tr><td>AUC0t — Linear trapezoidal</td><td><code>PKNCA::pk.nca()</code>, <code>auc.method=\"linear\"</code></td><td>Linear Trapezoidal</td><td><a href='https://www.pkpd168.com/_files/ugd/2cabb8_e9c279faac004d668927945a4921bb54.pdf' target='_blank' rel='noopener noreferrer'>Lee &amp; Lee (2009), bear/WinNonlin validation report</a><br><small class='text-muted'>Subject 1, Period 1 only</small></td></tr>",
          "<tr><td>AUC0t — Linear-up/Log-down</td><td><code>PKNCA::pk.nca()</code>, <code>auc.method=\"lin up/log down\"</code></td><td>Linear Up/Log Down Trapezoidal</td><td>Planned</td></tr>",
          "<tr><td>λz — TTT</td><td>BioEQ rule (t≥ 2·Tmax) flags points, PKNCA fits</td><td>No native equivalent</td><td>Planned</td></tr>",
          "<tr><td>λz — ARS</td><td><code>PKNCA::pk.nca()</code> best-fit search (max adj. R²)</td><td>Best Fit</td><td>Planned</td></tr>",
          "<tr><td>λz — Manual</td><td>BioEQ flags fixed points, PKNCA fits</td><td>Manual</td><td><a href='https://www.pkpd168.com/_files/ugd/2cabb8_e9c279faac004d668927945a4921bb54.pdf' target='_blank' rel='noopener noreferrer'>Lee &amp; Lee (2009), bear/WinNonlin validation report</a><br><small class='text-muted'>Subject 1, Period 1 only</small></td></tr>",
          "</tbody></table>"
        ),

        h4("Table 2 - Parallel & 2×2 Crossover ANOVA"),
        p(class = "text-muted", style = "font-size: 12px;", "Used for Average BE (ABE) on parallel and standard 2×2 crossover designs."),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr><th>BE Test</th><th>ANOVA option</th><th>BioEQ package/call</th><th>SAS equivalent</th><th>Validating dataset</th></tr></thead><tbody>",
          "<tr><td>Parallel</td><td>N/A — two-sample t-test</td><td><code>stats::t.test()</code></td><td><code>PROC TTEST</code></td><td><a href='https://doi.org/10.1208/s12248-014-9704-6' target='_blank' rel='noopener noreferrer'>Fuglsang, Schütz &amp; Labes (2015), AAPS J 17(2):400-404</a></td></tr>",
          "<tr><td>ABE (2×2)</td><td>Fixed</td><td><code>stats::lm()</code></td><td><code>PROC GLM</code></td><td><a href='https://doi.org/10.1208/s12248-014-9661-0' target='_blank' rel='noopener noreferrer'>Schütz, Labes &amp; Fuglsang (2014), AAPS J 16(6):1292-1297</a></td></tr>",
          "<tr><td>ABE (2×2)</td><td>Mixed — nlme (REML)</td><td><code>nlme::lme()</code></td><td><code>PROC MIXED</code> (default <code>DDFM=CONTAIN</code>)</td><td>Planned</td></tr>",
          "<tr><td>ABE (2×2)</td><td>Mixed — Satterthwaite DF</td><td><code>lme4::lmer()</code> + <code>lmerTest</code></td><td><code>PROC MIXED DDFM=SATTERTHWAITE</code></td><td>Planned</td></tr>",
          "<tr><td>ABE (2×2)</td><td>Mixed — Kenward-Roger DF</td><td><code>lme4::lmer()</code> + <code>lmerTest</code> + <code>pbkrtest</code></td><td><code>PROC MIXED DDFM=KENWARDROGER</code></td><td>Planned</td></tr>",
          "</tbody></table>"
        ),

        h4("Table 3 - Replicate-Design ANOVA (replicateBE)"),
        p(class = "text-muted", style = "font-size: 12px;",
          "All ANOVAs on replicate designs are performed through ", code("replicateBE"),
          ", regardless of BE evaluation method (ABEL or RSABE)."),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr><th>ANOVA option</th><th>BioEQ package/call</th><th>SAS equivalent</th><th>Used by</th><th>Validating dataset</th></tr></thead><tbody>",
          "<tr><td>Fixed (Method A)</td><td><code>replicateBE::method.A()</code></td><td><code>PROC GLM</code></td><td>ABEL, RSABE (auto: partial-replicate)</td><td><a href='https://doi.org/10.1208/s12248-020-0427-6' target='_blank' rel='noopener noreferrer'>Schütz, Labes, Tomashevskiy, González-de la Parra, Shitova &amp; Fuglsang (2020), AAPS J 22(2):44</a></td></tr>",
          "<tr><td>Mixed — nlme (Method B, option 2, default)</td><td><code>replicateBE::method.B(option=2)</code></td><td><code>PROC MIXED</code> (<code>DDFM=CONTAIN</code>)</td><td>ABEL, RSABE (auto: full-replicate)</td><td><a href='https://doi.org/10.1208/s12248-020-0427-6' target='_blank' rel='noopener noreferrer'>Schütz, Labes, Tomashevskiy, González-de la Parra, Shitova &amp; Fuglsang (2020), AAPS J 22(2):44</a></td></tr>",
          "<tr><td>Mixed — Satterthwaite DF (Method B, option 1)</td><td><code>replicateBE::method.B(option=1)</code></td><td><code>PROC MIXED DDFM=SATTERTHWAITE</code></td><td>ABEL only</td><td>Planned</td></tr>",
          "<tr><td>Mixed — Kenward-Roger DF (Method B, option 3)</td><td><code>replicateBE::method.B(option=3)</code></td><td><code>PROC MIXED DDFM=KENWARDROGER</code></td><td>ABEL only</td><td>Planned</td></tr>",
          "</tbody></table>"
        ),

        h4("Table 4 - RSABE Scaling Decision (FDA guidance)"),
        p(class = "text-muted", style = "font-size: 12px;",
          "Independent of Table 3 — only invoked when a parameter's s²wR (Reference within-subject ",
          "variance) meets the switching threshold (sₑᵣ ≥ 0.294); otherwise RSABE reports Table 3's ",
          "result directly, unscaled."),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr><th>Calculation</th><th>BioEQ function</th><th>SAS equivalent</th><th>Validating dataset</th><th>Reference</th></tr></thead><tbody>",
          "<tr><td>s²wR / switching decision</td><td><code>fit_rsabe_seq_variance()</code> — <code>lm(D_ij ~ Sequence)</code></td><td><code>PROC GLM</code> (partial) / <code>PROC MIXED</code> (full)</td><td>Planned</td><td><a href='https://www.fda.gov/media/163638/download' target='_blank' rel='noopener noreferrer'>FDA, Statistical Approaches to Establishing Bioequivalence (May 2026), Appendix G</a></td></tr>",
          "<tr><td>FDA Linearized (Howe UCB)</td><td><code>rsabe_linearized_test()</code> — uses <code>fit_rsabe_seq_mean()</code>'s <code>lm(I_ij ~ Sequence)</code></td><td><code>PROC GLM</code> / <code>PROC MIXED</code> + Howe's Approximation I</td><td>Planned</td><td><a href='https://www.fda.gov/media/163638/download' target='_blank' rel='noopener noreferrer'>FDA, Statistical Approaches to Establishing Bioequivalence (May 2026), Appendix G</a></td></tr>",
          "<tr><td>Non-Central TOST (exact)</td><td><code>rsabe_nctost_test()</code></td><td>none</td><td>Planned</td><td><a href='https://doi.org/10.1208/s12248-016-9873-6' target='_blank' rel='noopener noreferrer'>Tóthfalusi &amp; Endrényi (2016), AAPS J 18(2):476–489</a> — calculation not yet independently verified</td></tr>",
          "</tbody></table>"
        ),

        h4("Table 5 - Data Handling (not part of NCA)"),
        p(class = "text-muted", style = "font-size: 12px;",
          "Missing data handling runs before NCA; carryover detection runs after NCA, on its output."),
        HTML(
          "<table class='bioeq-mapping'>",
          "<thead><tr><th>Calculation</th><th>BioEQ function</th><th>Equivalence</th><th>Validating dataset</th></tr></thead><tbody>",
          "<tr><td>Missing data handling (pre-NCA)</td><td><code>handle_missing_data()</code> (<code>R/missing_data_handling.R</code>) — position-aware: BLQ→0, middle/terminal complete-case, interpolation, or LOCF</td><td>Not a SAS/WinNonlin procedure — internal QC/pre-processing rule</td><td>Planned</td></tr>",
          "<tr><td>Carryover detection (post-NCA)</td><td><code>detect_carryover()</code> (<code>R/carryover_detection.R</code>) — pre-dose concentration vs. same-period Cmax, 5% threshold</td><td>ICH M13A §2.2.3.3 guideline criterion — not a SAS/WinNonlin procedure</td><td>Planned</td></tr>",
          "</tbody></table>"
        )
      )
    ),

    # ---- Results panel -------------------------------------------------
    fluidRow(
      box(
        title = "Validation Results",
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
