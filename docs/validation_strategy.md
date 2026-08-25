# BioEQ Validation Strategy

## Status

Draft v0.1 — 2026-08-20. Reflects the validation assets that exist in this
repository today (`validation/manifest.csv`, `R/validation_runner.R`,
`R/validation_embedded.RData`) and identifies what is still needed before the
suite can be called complete. Supersedes nothing; extends
[`validation/README.md`](../validation/README.md), which remains the
operational reference for the manifest schema and how to add a dataset.

## 1. Purpose and Scope

This document defines how BioEQ's analytical correctness is verified and
documented, so that:

- Every PK/statistical calculation BioEQ produces can be traced to an
  independent reference value.
- A reviewer (internal QA, a client's biostatistics team, or a regulatory
  auditor) can reproduce the validation run and reach the same pass/fail
  conclusions.
- Gaps in coverage are visible and tracked, rather than implied by the
  existence of a `validation/` folder.

Scope is BioEQ's computational core (`R/*.R`) as exercised through the Shiny
app's Validation tab. UI/UX behavior, file upload handling, and report
formatting are out of scope for this document (see `docs/user_guide.md`).

## 2. Regulatory Framing

BioEQ is not itself an FDA-regulated product, but it produces PK parameters
and bioequivalence conclusions that feed regulatory submissions. The
validation approach borrows from:

- **GAMP 5** category 4/5 software validation principles (risk-based testing
  proportional to the software's role in the decision).
- **FDA Guidance for Industry — Statistical Approaches to Establishing
  Bioequivalence** (defines the ABE/ABEL/RSABE methods being validated).
- **21 CFR Part 11** — relevant to record integrity of validation reports if
  they are retained as part of a submission package, not to BioEQ's
  day-to-day operation.

This is a black-box validation strategy, not a code-correctness proof: it
establishes that BioEQ's *outputs* match independently-derived *reference
outputs* for known inputs, within stated tolerances. It does not replace unit
testing (`tests/testthat/`) of internal functions.

## 3. Validation Principle: Fresh Computation, Independent References

Unchanged from `validation/README.md`, restated here as the controlling
principle for everything below:

1. Every validation run re-executes the full BioEQ analysis pipeline from raw
   input data. No cached or pre-computed BioEQ output is ever used as a
   comparison basis.
2. Reference ("expected") values come only from sources independent of
   BioEQ's own code: published reference datasets with peer-reviewed
   expected results (Schütz, Fuglsang, `replicateBE`), or output from
   independently validated software (SAS PROC GLM/MIXED, Phoenix WinNonlin).
3. Inputs and reference values are immutable once registered — corrections
   go through a new manifest entry or a logged, reviewed edit, never a silent
   overwrite.
4. Input datasets and expected results are baked into
   `R/validation_embedded.RData` at build time (via
   `validation/scripts/embed_validation_data.R`) so the Shiny app has no
   runtime filesystem dependency on `validation/datasets/` or
   `validation/expected_results/` — this is why those directories are empty
   in a checkout; the data lives in the `.RData` store, not as loose CSVs.

## 4. Coverage Model

Coverage is tracked per **module** (what code is exercised) crossed with
**design/dataset family** (what published or software-generated reference
exists). Current state, read from `R/*.R` and `validation/manifest.csv`:

| Module | Function | Validation datasets today | Status |
|---|---|---|---|
| `nca_analysis.R`, `nca_functions.R` | NCA: AUC, Cmax, Tmax, λz, t½, AUC∞ | 1 placeholder row (`NCA Validation Datasets (Pending)`) | **Not validated** |
| `simple_anova.R`, `be_analysis.R` | ABE: 2×2×2 crossover ANOVA, PE, 90% CI | 8 Schütz 2×2×2 datasets (A–H) | Validated (pending re-confirmation, §6) |
| `be_analysis.R` (parallel path) | Parallel-design ANOVA/t-test, PE, 90% CI | 11 Fuglsang parallel datasets (P01–P11) | Validated (pending re-confirmation) |
| `rsabe_analysis.R` | Within-subject CV, RSABE, expanded limits | 30 `replicateBE` datasets (rds01–rds30) | Validated (pending re-confirmation) |
| `cumulative_be_analysis.R` | Sequential/group-sequential BE | none | **Not validated** |
| `anomaly_detection.R` | Outlier/anomaly flagging | none | **Not validated** |
| `carryover_detection.R` | Carryover effect testing | none | **Not validated** |
| `missing_data_handling.R` | Imputation / missing-data handling | none | **Not validated** |
| `randomization.R` | Sequence randomization | none | **Not validated** (arguably not a numerical-accuracy concern — see §7) |
| `statistics.R`, `utils.R` | Shared statistical/utility functions | covered indirectly via the above | Indirect only |

49 of 50 manifest rows are BE-layer (ANOVA + PE/CI); only 1 NCA row exists,
and it is a placeholder. **NCA is the single largest coverage gap** — every
downstream BE calculation in the current suite consumes pre-calculated PK
parameters rather than exercising BioEQ's own concentration-time → PK-
parameter pipeline. Until real NCA reference datasets are embedded, BioEQ's
`nca_analysis.R`/`nca_functions.R` output is validated only by
`tests/testthat/`, not by the black-box suite.

## 5. Reference Data Sources, by Priority

1. **NCA layer (highest priority gap)**:
   - `PKNCA` (CRAN) — open-source, independently maintained NCA reference
     implementation; can generate reference AUC/Cmax/Tmax/λz for synthetic
     and public concentration-time datasets.
   - `NonCompart` (CRAN) — WinNonlin-equivalent λz algorithm, useful
     specifically for half-life/AUC∞ extrapolation cross-checks.
   - Phoenix WinNonlin, if available under license, for a smaller set of
     "gold standard" concentration-time profiles.
2. **BE/ANOVA layer (already seeded)**: continue using the Schütz 2014,
   Fuglsang 2015, and Schütz 2020 (`replicateBE`) reference-dataset papers —
   these are the field-standard reference sets for exactly this purpose and
   the manifest is already structured around them.
3. **Cumulative/group-sequential BE, anomaly detection, carryover, missing
   data**: no standard published reference-dataset family exists for these
   the way it does for ABE/RSABE. For these, build synthetic datasets with
   an analytically-known answer (e.g., a carryover effect injected at a
   known magnitude, a known fraction of missing observations), documented
   in the manifest with `reference_source = "synthetic, analytically
   derived"` rather than a literature citation. This mirrors how bioeq
   (the Python package) validates its half-life and point-estimate
   calculations — acceptable as a secondary layer, but should not be the
   *only* validation for a BE/ANOVA-adjacent module if a literature dataset
   becomes available later.

## 6. Immediate Action Items

These close the specific gaps found in this review, ordered by impact:

1. **Confirm the 49 "available" BE datasets are actually embedded and
   passing.** The manifest marks them `available`, but I could not confirm
   this from a static checkout since the data lives in
   `R/validation_embedded.RData`, a binary file. Run the validation suite
   end-to-end (via the Shiny Validation tab or a headless call to
   `run_validation_for_dataset()` across the manifest) and capture the
   pass/fail output as the first dated validation report under this
   strategy.
2. **Populate the NCA layer.** At minimum, embed 3–5 `PKNCA`-derived
   concentration-time datasets with known AUC/Cmax/Tmax/λz/t½, wired through
   `validation/scripts/embed_validation_data.R`, before treating
   `nca_analysis.R` as validated.
3. **Add synthetic reference cases for the four uncovered modules**
   (`cumulative_be_analysis.R`, `anomaly_detection.R`,
   `carryover_detection.R`, `missing_data_handling.R`), per §5.3.
4. **Decide whether `randomization.R` needs validation-suite coverage at
   all** (§7) and record that decision here rather than leaving it as a
   silent gap.
5. **Write the first traceability matrix** (§8) so each manifest dataset is
   mapped to the functional requirement and module it verifies — this makes
   the "49 available, 1 placeholder" summary auditable instead of just a
   count.

## 7. Out-of-Scope-by-Design Items

`randomization.R` generates treatment sequences; it has no single "correct"
numeric output to validate against the way an AUC or PE% calculation does.
Recommended treatment: verify it via property-based unit tests (balance
across sequences, absence of seed leakage) in `tests/testthat/`, not the
black-box numeric-comparison suite. State this explicitly in the manifest
(a `not_applicable` status alongside `available`/`partial`/`placeholder`)
rather than leaving it silently absent, so a reviewer doesn't mistake the
absence for an oversight.

## 8. Traceability

Add a `traceability_matrix.md` (or `.csv`, to stay machine-readable and
joinable with `manifest.csv`) mapping:

`functional requirement → BioEQ module/function → manifest dataset_id(s) →
test file(s) in tests/testthat/`

This is the piece bioeq (the Python package) has that BioEQ currently
lacks, and it's what turns "we have 50 datasets" into "here is proof every
stated requirement is covered by at least one dataset and one unit test."
Suggested minimal columns: `requirement_id`, `requirement_text`, `module`,
`dataset_ids`, `test_files`, `status` (`covered` / `partial` / `gap`).

## 9. Reporting and Change Control

- Each validation run produces a timestamped report (reuse the JSON/HTML
  report pattern already implied by `shiny/utils/sas_style_report.R` and the
  Validation tab UI) retained under `validation/reports_output/`.
- Re-run the full suite and file a new report:
  - after any change to `R/nca_analysis.R`, `R/nca_functions.R`,
    `R/be_analysis.R`, `R/simple_anova.R`, `R/rsabe_analysis.R`,
    `R/cumulative_be_analysis.R`, `R/statistics.R`, or `R/utils.R`;
  - after upgrading `nlme`, `lme4`, `lmerTest`, `replicateBE`, or
    `PowerTOST`;
  - after upgrading R itself;
  - quarterly, as a standing qualification check even with no code changes.
- A failing validation run blocks release until either the code is fixed or
  the reference value is shown to be wrong and corrected through a reviewed
  manifest change (never a silent edit to `expected_results`).

## 10. Open Questions for You

- Do you have (or license) Phoenix WinNonlin or SAS access to generate
  first-party NCA reference outputs, or should the NCA layer lean entirely
  on `PKNCA`/`NonCompart` as the independent reference?
- Is there a target regulatory audience for this strategy doc (an internal
  QA reviewer, a client's biostatistics team, an actual FDA submission
  package) that should change how formal/citable §2 and §9 need to be?
