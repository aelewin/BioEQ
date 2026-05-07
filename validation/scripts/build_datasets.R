# Build validation datasets from downloaded source files
#
# Inputs:
#   validation/datasets/parallel_raw/ESM_4..14.txt   (Fuglsang 2015, P1..P11)
#   validation/datasets/cross2x2_raw/ESM_1..8.txt    (Schutz 2014, A..H)
#   /Users/tristanchiappetti/Desktop/replicatedatesets114/RDS 1..14.csv
#
# Outputs:
#   validation/datasets/fuglsang_parallel_P{01..11}.csv  (Subject,Treatment,PK)
#   validation/datasets/schutz_2x2_{A..H}.csv            (Subject,Sequence,Period,Treatment,PK)
#   validation/datasets/replicateBE_rds{01..14}.csv      (subject,period,sequence,treatment,PK[,logPK])

setwd("/Users/tristanchiappetti/Workspace/BioEQ/validation/scripts")
val <- normalizePath("..")            # validation/

cat("Working in:", val, "\n")

# ---- Parallel: ESM_4 = P1, ESM_5 = P2, ..., ESM_14 = P11 ------------------
parallel_map <- data.frame(
  esm   = 4:14,
  pcode = sprintf("P%02d", 1:11),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(parallel_map))) {
  src <- file.path(val, "datasets/parallel_raw",
                   sprintf("ESM_%d.txt", parallel_map$esm[i]))
  if (!file.exists(src)) { cat("Missing:", src, "\n"); next }
  d <- utils::read.table(src, header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE, fill = TRUE,
                         strip.white = TRUE)
  # Tolerate unexpected names
  names(d) <- tolower(names(d))
  out <- data.frame(
    Subject   = d$subj,
    Treatment = d$treat,
    PK        = as.numeric(d$var),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$PK) & nzchar(out$Subject), , drop = FALSE]
  dst <- file.path(val, "datasets",
                   sprintf("fuglsang_parallel_%s.csv", parallel_map$pcode[i]))
  utils::write.csv(out, dst, row.names = FALSE)
  cat(sprintf("Wrote %s  (n=%d, NT=%d, NR=%d)\n", basename(dst), nrow(out),
              sum(out$Treatment == "T"), sum(out$Treatment == "R")))
}

# ---- 2x2x2: ESM_1 = A, ESM_2 = B, ..., ESM_8 = H --------------------------
cross_map <- data.frame(
  esm   = 1:8,
  code  = LETTERS[1:8],
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(cross_map))) {
  src <- file.path(val, "datasets/cross2x2_raw",
                   sprintf("ESM_%d.txt", cross_map$esm[i]))
  if (!file.exists(src)) { cat("Missing:", src, "\n"); next }
  d <- utils::read.table(src, header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE, fill = TRUE,
                         strip.white = TRUE)
  names(d) <- tolower(names(d))
  # ESM_8 has an extra ObsNumber column up front; ignore it.
  out <- data.frame(
    Subject   = d$subj,
    Sequence  = d$seq,
    Period    = d$per,
    Treatment = d$trt,
    PK        = as.numeric(d$var),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$PK), , drop = FALSE]
  dst <- file.path(val, "datasets",
                   sprintf("schutz_2x2_%s.csv", cross_map$code[i]))
  utils::write.csv(out, dst, row.names = FALSE)
  cat(sprintf("Wrote %s  (n_obs=%d, n_subj=%d)\n", basename(dst),
              nrow(out), length(unique(out$Subject))))
}

# ---- replicateBE rds01..rds30 -- handled by build_replicateBE_all.R ------
# The 30 replicate-design datasets and their Method-A reference values are
# generated directly from the `replicateBE` R package by
# validation/scripts/build_replicateBE_all.R. That script is the single
# source of truth for replicate datasets; do not duplicate the work here.

cat("\nDone.\n")
