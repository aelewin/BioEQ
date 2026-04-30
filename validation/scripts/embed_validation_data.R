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
  datasets[[id]] <- utils::read.csv(path, stringsAsFactors = FALSE)
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
    utils::read.csv(path, stringsAsFactors = FALSE, comment.char = "#"),
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
