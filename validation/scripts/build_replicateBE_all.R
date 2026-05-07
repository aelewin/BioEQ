# Build all 30 replicateBE rds datasets AND their reference results
# (Method A and Method B) by calling the replicateBE package directly.
# This is the authoritative source for Schutz et al. AAPS J. 2020;22:44.
#
# Outputs:
#   validation/datasets/replicateBE_rds01..rds30.csv
#   validation/expected_results/replicateBE_rds01..rds30.csv

setwd("/Users/tristanchiappetti/Workspace/BioEQ")

if (!requireNamespace("replicateBE", quietly = TRUE)) {
  install.packages("replicateBE", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(library(replicateBE))

# rdsXX -> design (from data(package="replicateBE")$results)
rds_designs <- c(
  rds01 = "TRTR.RTRT", rds02 = "TRR.RTR.RRT", rds03 = "TRT.RTR",
  rds04 = "TRR.RTR.RRT", rds05 = "TRRT.RTTR", rds06 = "TRTR.RTRT",
  rds07 = "TRR.RTR.RRT", rds08 = "TRTR.RTRT", rds09 = "TRTR.RTRT",
  rds10 = "TRR.RRT", rds11 = "TRRT.RTTR", rds12 = "TRTR.RTRT",
  rds13 = "TRTR.RTRT", rds14 = "TRTR.RTRT", rds15 = "TRTR.RTRT",
  rds16 = "TRRT.RTTR", rds17 = "TRT.RTR", rds18 = "TRTR.RTRT",
  rds19 = "TRTR.RTRT", rds20 = "TRTR.RTRT", rds21 = "TRTR.RTRT",
  rds22 = "TRR.RTR", rds23 = "TRTR.RTRT.TRRT.RTTR",
  rds24 = "TRRT.RTTR.TTRR.RRTT", rds25 = "TRTR.RTRT", rds26 = "TRTR.RTRT",
  rds27 = "TR.RT.TT.RR", rds28 = "TTRR.RRTT", rds29 = "TRTR.RTRT",
  rds30 = "TRR.RTR.RRT"
)

# Cache loaded design environments
design_envs <- list()
load_design <- function(d) {
  if (is.null(design_envs[[d]])) {
    e <- new.env()
    data(list = d, package = "replicateBE", envir = e)
    design_envs[[d]] <<- e
  }
  design_envs[[d]]
}

# ---- Helpers --------------------------------------------------------------

# Parse a number followed by % from a line like "CVwR  : 46.96%"
parse_pct <- function(line) {
  m <- regmatches(line, regexpr("[-+]?[0-9]+\\.[0-9]+", line))
  if (length(m) == 0) return(NA_real_)
  as.numeric(m[1])
}

# Parse "Confidence interval: 107.11% ... 124.89%  pass"
parse_ci <- function(line) {
  m <- regmatches(line, gregexpr("[-+]?[0-9]+\\.[0-9]+", line))[[1]]
  if (length(m) < 2) return(c(NA_real_, NA_real_))
  as.numeric(m[1:2])
}

# Run replicateBE method.A or method.B and parse the .txt result
run_method <- function(df, method = c("A", "B"), tmpdir) {
  method <- match.arg(method)
  # Clear any existing MethodA/B files for this run
  old <- list.files(tmpdir, pattern = sprintf("Method%s", method),
                    full.names = TRUE)
  if (length(old)) try(file.remove(old), silent = TRUE)

  fn <- if (method == "A") replicateBE::method.A else replicateBE::method.B
  invisible(suppressWarnings(suppressMessages(
    if (method == "A") {
      fn(data = df, print = TRUE, path.out = tmpdir, ask = FALSE)
    } else {
      # option=3 -> lmerTest with Kenward-Roger DF (matches Schutz Table 2)
      fn(data = df, print = TRUE, path.out = tmpdir, ask = FALSE, option = 3)
    }
  )))
  txt <- list.files(tmpdir, pattern = sprintf("Method%s.*\\.txt$", method),
                    full.names = TRUE)
  if (length(txt) == 0) stop("Method ", method, " produced no output")
  lines <- readLines(txt[1], warn = FALSE)

  # Locate the result block. Method.B output has a similar layout.
  cvwr_line <- grep("^\\s*CVwR\\s*:", lines, value = TRUE)[1]
  ci_line   <- grep("^\\s*Confidence interval\\s*:", lines, value = TRUE)[1]
  pe_line   <- grep("^\\s*Point estimate\\s*:", lines, value = TRUE)[1]
  cvwt_line <- grep("^\\s*CVwT\\s*:", lines, value = TRUE)[1]
  df_line   <- grep("^\\s*Degrees of freedom\\s*:", lines, value = TRUE)[1]

  cvwr <- parse_pct(cvwr_line)
  cvwt <- parse_pct(cvwt_line)
  ci   <- parse_ci(ci_line)
  pe   <- parse_pct(pe_line)
  ddf  <- if (is.na(df_line)) NA_real_ else
            as.numeric(regmatches(df_line, regexpr("[0-9]+\\.?[0-9]*", df_line)))

  ci_pass <- grepl("\\bpass\\b", ci_line)
  pe_pass <- grepl("\\bpass\\b", pe_line)

  list(CVwR = cvwr, CVwT = cvwt, PE = pe,
       CI_lower = ci[1], CI_upper = ci[2], DF = ddf,
       CI_pass = ci_pass, PE_pass = pe_pass)
}

erow <- function(parameter, expected_value, tolerance_value = 0.05,
                 tolerance_type = "absolute", scope = "study", subject = NA,
                 treatment = NA, unit = "%",
                 reference_source = "Schutz H et al. AAPS J. 2020;22:44 (replicateBE v1.1.3 method.A/B)",
                 notes = "") {
  data.frame(parameter = parameter, scope = scope, subject = subject,
             treatment = treatment, expected_value = expected_value,
             tolerance_type = tolerance_type, tolerance_value = tolerance_value,
             unit = unit, reference_source = reference_source, notes = notes,
             stringsAsFactors = FALSE)
}

# ---- Main loop ------------------------------------------------------------
val <- "validation"
ds_dir  <- file.path(val, "datasets")
exp_dir <- file.path(val, "expected_results")
dir.create(ds_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(exp_dir, showWarnings = FALSE, recursive = TRUE)

tmpdir <- tempfile("repBE_"); dir.create(tmpdir)

summary_rows <- list()

for (i in 1:30) {
  rds_name <- sprintf("rds%02d", i)
  des <- rds_designs[[rds_name]]
  e <- load_design(des)
  df <- get(rds_name, envir = e)

  # Persist dataset CSV (BioEQ canonical column casing: leave as lowercase since
  # other replicate datasets use lowercase. BioEQ readers should be tolerant.)
  out_df <- df[, c("subject", "period", "sequence", "treatment", "PK"),
               drop = FALSE]
  names(out_df) <- c("Subject", "Period", "Sequence", "Treatment", "PK")
  ds_path <- file.path(ds_dir, sprintf("replicateBE_%s.csv", rds_name))
  utils::write.csv(out_df, ds_path, row.names = FALSE)

  # Run Method A and Method B
  resA <- tryCatch(run_method(df, "A", tmpdir),
                   error = function(e) { warning("rds", i, " A: ", e$message); NULL })
  resB <- tryCatch(run_method(df, "B", tmpdir),
                   error = function(e) { warning("rds", i, " B: ", e$message); NULL })

  # Build expected_results table
  rows <- list()
  if (!is.null(resA)) {
    rows[[length(rows)+1]] <- erow("MethodA_PE",       resA$PE,
                                   notes = "Method A point estimate (T/R, %)")
    rows[[length(rows)+1]] <- erow("MethodA_CI_lower", resA$CI_lower,
                                   notes = "Method A 90% CI lower bound (%)")
    rows[[length(rows)+1]] <- erow("MethodA_CI_upper", resA$CI_upper,
                                   notes = "Method A 90% CI upper bound (%)")
    rows[[length(rows)+1]] <- erow("MethodA_CVwR",     resA$CVwR,
                                   notes = "CVwR from Method A (%)")
  }
  if (!is.null(resB)) {
    rows[[length(rows)+1]] <- erow("MethodB_PE",       resB$PE,
                                   notes = "Method B point estimate (T/R, %) — lmer/KR")
    rows[[length(rows)+1]] <- erow("MethodB_CI_lower", resB$CI_lower,
                                   notes = "Method B 90% CI lower bound (%) — lmer/KR")
    rows[[length(rows)+1]] <- erow("MethodB_CI_upper", resB$CI_upper,
                                   notes = "Method B 90% CI upper bound (%) — lmer/KR")
    rows[[length(rows)+1]] <- erow("MethodB_CVwR",     resB$CVwR,
                                   notes = "CVwR from Method B (%) — lmer/KR")
  }

  if (length(rows) > 0) {
    df_out <- do.call(rbind, rows)
    utils::write.csv(df_out, file.path(exp_dir, sprintf("replicateBE_%s.csv", rds_name)),
                     row.names = FALSE)
  }

  cat(sprintf("%s [%s] n_obs=%d  PE=%.2f  CI=(%.2f, %.2f)  CVwR=%.2f\n",
              rds_name, des, nrow(df),
              if (!is.null(resA)) resA$PE else NA,
              if (!is.null(resA)) resA$CI_lower else NA,
              if (!is.null(resA)) resA$CI_upper else NA,
              if (!is.null(resA)) resA$CVwR else NA))

  summary_rows[[i]] <- data.frame(
    rds = rds_name, design = des, n_obs = nrow(df),
    n_subj = length(unique(df$subject)),
    PE = if (!is.null(resA)) resA$PE else NA_real_,
    CI_lower = if (!is.null(resA)) resA$CI_lower else NA_real_,
    CI_upper = if (!is.null(resA)) resA$CI_upper else NA_real_,
    CVwR = if (!is.null(resA)) resA$CVwR else NA_real_,
    stringsAsFactors = FALSE
  )
}

unlink(tmpdir, recursive = TRUE)

# ---- Update manifest replicate rows to status="available" -----------------
mpath <- file.path(val, "manifest.csv")
m <- utils::read.csv(mpath, stringsAsFactors = FALSE)
sel <- m$group_id == "be_replicate_abel"
sm <- do.call(rbind, summary_rows)
for (k in seq_len(nrow(m))) {
  if (!sel[k]) next
  did <- m$dataset_id[k]
  hit <- sm$rds == sub("^replicateBE_", "", did)
  if (any(hit)) {
    j <- which(hit)[1]
    m$status[k]     <- "available"
    m$n_subjects[k] <- sm$n_subj[j]
    m$data_file[k]  <- sprintf("datasets/replicateBE_%s.csv", sm$rds[j])
    m$description[k] <- sprintf(
      "Reference replicate-design dataset %s [%s] from Schutz et al. (2020). Method A: PE = %.2f%%, 90%% CI = (%.2f, %.2f), CVwR = %.2f%%.",
      sm$rds[j], sm$design[j], sm$PE[j], sm$CI_lower[j], sm$CI_upper[j], sm$CVwR[j])
  }
}
utils::write.csv(m, mpath, row.names = FALSE)
cat("\nUpdated manifest: ", sum(sel), " replicate rows now available.\n")
cat("Done.\n")
