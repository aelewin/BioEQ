# embed_validation_data.R
#
# ONE-TIME DEVELOPER SCRIPT — not called by the Shiny application.
#
# Reads every validation CSV (manifest, groups, datasets, expected_results) and
# serialises them into R/validation_embedded.RData.  The Shiny app loads that
# single RData file at startup and never touches the CSV files at runtime.
#
# Re-run whenever you:
#   - Add / update reference datasets or expected values
#   - Update the replicateBE package (then re-run build_replicateBE_all.R first)
#   - Add new validation groups
#
# Usage (from project root):
#   Rscript validation/scripts/embed_validation_data.R

setwd("/Users/tristanchiappetti/Workspace/BioEQ")
val_dir  <- "validation"
out_file <- "R/validation_embedded.RData"

# read.csv()'s type-guessing (type.convert) silently coerces a column to
# logical if every value in it happens to look like a logical literal - most
# notably a Treatment column that is "T" for every row (a single-arm
# synthetic profile), or any column that is entirely blank/NA. Once that
# happens the original text is UNRECOVERABLE after the fact - as.character()
# on the resulting logical gives "TRUE"/"FALSE", not "T"/"F". The only correct
# fix is to prevent the coercion at read time via colClasses, for both
# dataset and expected-results CSVs. Never include Period/Time/Concentration/
# expected_value/tolerance_value here - those must stay numeric for
# downstream comparisons and arithmetic.
.CATEGORICAL_COLS <- c("Subject", "Sequence", "Treatment",
                       "subject", "treatment", "parameter", "scope",
                       "tolerance_type", "unit", "reference_source", "notes")
.read_validation_csv <- function(path, ...) {
  header <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  col_classes <- setNames(rep(NA_character_, length(header)), header)
  col_classes[intersect(.CATEGORICAL_COLS, header)] <- "character"
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = col_classes, ...)
}

cat("Reading manifest ...\n")
manifest <- utils::read.csv(file.path(val_dir, "manifest.csv"),
                             stringsAsFactors = FALSE, comment.char = "")
if (!"group_id" %in% names(manifest)) manifest$group_id <- NA_character_

cat("Reading groups ...\n")
groups <- utils::read.csv(file.path(val_dir, "groups.csv"),
                           stringsAsFactors = FALSE, comment.char = "")
groups$is_bioeq_generated <- as.logical(groups$is_bioeq_generated)
for (col in c("pmid", "doi")) {
  if (!col %in% names(groups)) groups[[col]] <- NA_character_
}

cat("Reading datasets ...\n")
datasets <- list()
for (i in seq_len(nrow(manifest))) {
  id  <- manifest$dataset_id[i]
  rel <- manifest$data_file[i]
  if (is.na(rel) || rel == "") next
  path <- file.path(val_dir, rel)
  if (!file.exists(path)) {
    cat(sprintf("  SKIP (no file): %s -> %s\n", id, path))
    next
  }
  datasets[[id]] <- .read_validation_csv(path)
  cat(sprintf("  dataset: %-40s (%d rows)\n", id, nrow(datasets[[id]])))
}

cat("Reading expected results ...\n")
expected <- list()
for (i in seq_len(nrow(manifest))) {
  id  <- manifest$dataset_id[i]
  rel <- manifest$expected_file[i]
  if (is.na(rel) || rel == "") next
  path <- file.path(val_dir, rel)
  if (!file.exists(path)) {
    cat(sprintf("  SKIP (no file): %s -> %s\n", id, path))
    next
  }
  df <- tryCatch(
    .read_validation_csv(path, comment.char = "#"),
    error = function(e) NULL
  )
  if (!is.null(df) && nrow(df) > 0) {
    expected[[id]] <- df
    cat(sprintf("  expected: %-40s (%d rows)\n", id, nrow(df)))
  }
}

# Bundle into a single named list embedded in the package
.bioeq_validation <- list(
  manifest = manifest,
  groups   = groups,
  datasets = datasets,
  expected = expected
)

save(.bioeq_validation, file = out_file, compress = "xz")
cat(sprintf(
  "\nEmbedded %d datasets and %d expected-result sets -> %s\n",
  length(datasets), length(expected), out_file
))
cat("Done.\n")
