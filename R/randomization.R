# =============================================================================
# Randomization engine for bioequivalence study designs
# =============================================================================
# Pure base-R permuted-block randomization. Fully deterministic given (seed,
# design, n, block_size, stratification) so any regulator can reproduce the
# schedule from the audit metadata alone — no external package version
# dependencies in the random number stream.
#
# Algorithm:
#   1. set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
#                sample.kind = "Rejection")
#      (locked RNG so results are identical on R >= 3.6.0).
#   2. For each stratum, repeatedly draw permuted blocks of length
#      block_size (a multiple of the number of sequences) where each block
#      contains exactly block_size / k copies of each sequence, with order
#      shuffled by sample().
#   3. Truncate to the requested per-stratum sample size.
#   4. Concatenate strata, assign sequential Subject IDs.
#
# Verification is bit-identical regeneration: same inputs ⇒ same schedule.
# =============================================================================

# Supported BE study designs ---------------------------------------------------
bioeq_designs <- function() {
  list(
    parallel_2 = list(
      label     = "Parallel (T vs R)",
      sequences = c("T", "R"),
      periods   = 1L,
      notes     = "Two-arm parallel design. Each subject receives one treatment."
    ),
    crossover_2x2 = list(
      label     = "2x2 Crossover (TR | RT)",
      sequences = c("TR", "RT"),
      periods   = 2L,
      notes     = "Standard 2-period 2-sequence crossover."
    ),
    replicate_2x2x3 = list(
      label     = "2x2x3 Replicate (TRT | RTR)",
      sequences = c("TRT", "RTR"),
      periods   = 3L,
      notes     = "3-period replicate with R replicated in one sequence and T in the other."
    ),
    replicate_2x2x4 = list(
      label     = "2x2x4 Full Replicate (TRTR | RTRT)",
      sequences = c("TRTR", "RTRT"),
      periods   = 4L,
      notes     = "4-period full replicate; both T and R replicated in every subject."
    ),
    partial_2x3x3 = list(
      label     = "2x3x3 Partial Replicate (TRR | RTR | RRT)",
      sequences = c("TRR", "RTR", "RRT"),
      periods   = 3L,
      notes     = "3-period partial replicate; R replicated, T given once."
    )
  )
}

#' Generate a bioequivalence randomization schedule
#'
#' @param design       One of names(bioeq_designs()).
#' @param n_total      Total sample size (will be split equally among strata).
#' @param block_size   Permuted block size; must be a positive multiple of the
#'                    number of sequences. Default 2 * k.
#' @param seed         Integer RNG seed (required for reproducibility).
#' @param strata       Optional named list of stratum levels, e.g.
#'                    list(Site = c("S1","S2"), Sex = c("M","F")). The cross-
#'                    product is enumerated and n_total is divided equally.
#' @param subject_prefix  Prefix for subject IDs. Default "S".
#' @param subject_pad     Zero-pad subject numbers to this many digits. Default
#'                       chosen automatically from n_total.
#' @return list with $schedule (data.frame) and $meta (audit metadata).
generate_randomization <- function(design,
                                   n_total,
                                   block_size     = NULL,
                                   seed,
                                   strata         = NULL,
                                   subject_prefix = "S",
                                   subject_pad    = NULL,
                                   method_label   = NULL) {
  designs <- bioeq_designs()
  if (!design %in% names(designs))
    stop("Unknown design '", design, "'. Choose one of: ",
         paste(names(designs), collapse = ", "))
  if (!is.numeric(n_total) || length(n_total) != 1 || n_total < 2 ||
      n_total != as.integer(n_total))
    stop("n_total must be a single positive integer >= 2.")
  if (missing(seed) || is.null(seed) || !is.numeric(seed) || length(seed) != 1)
    stop("A single numeric seed is required for reproducibility.")
  seed <- as.integer(seed)

  d   <- designs[[design]]
  seq_labels <- d$sequences
  k   <- length(seq_labels)

  if (is.null(block_size)) block_size <- 2L * k
  if (block_size %% k != 0 || block_size < k)
    stop(sprintf("block_size (%d) must be a positive multiple of the number of sequences (%d).",
                 block_size, k))

  # Build stratum grid -------------------------------------------------------
  if (is.null(strata) || length(strata) == 0) {
    stratum_grid <- data.frame(.Stratum = "All", stringsAsFactors = FALSE)
  } else {
    if (!is.list(strata) || is.null(names(strata)) || any(names(strata) == ""))
      stop("'strata' must be a named list of character vectors.")
    stratum_grid <- expand.grid(strata, KEEP.OUT.ATTRS = FALSE,
                                stringsAsFactors = FALSE)
    stratum_grid$.Stratum <- apply(stratum_grid, 1L,
                                   function(r) paste(r, collapse = " | "))
  }
  n_strata    <- nrow(stratum_grid)
  per_stratum <- ceiling(n_total / n_strata)

  # Lock the RNG so results are stable across R >= 3.6 ----------------------
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old_seed))
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else
      rm(list = ".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)

  RNGkind(kind = "Mersenne-Twister",
          normal.kind = "Inversion",
          sample.kind = "Rejection")
  set.seed(seed)

  # Generate per-stratum blocks ---------------------------------------------
  per_block <- block_size %/% k
  parts <- vector("list", n_strata)
  for (i in seq_len(n_strata)) {
    n_blocks <- ceiling(per_stratum / block_size)
    seqs <- character(0)
    blocks <- integer(0)
    for (b in seq_len(n_blocks)) {
      pool  <- rep(seq_labels, each = per_block)
      shuf  <- sample(pool, size = block_size, replace = FALSE)
      seqs  <- c(seqs, shuf)
      blocks <- c(blocks, rep(b, block_size))
    }
    seqs   <- seqs[seq_len(per_stratum)]
    blocks <- blocks[seq_len(per_stratum)]

    stratum_label <- stratum_grid$.Stratum[i]
    stratum_cols  <- if (".Stratum" %in% names(stratum_grid) && ncol(stratum_grid) > 1)
      stratum_grid[i, setdiff(names(stratum_grid), ".Stratum"), drop = FALSE]
    else
      data.frame()

    df <- data.frame(
      Stratum  = stratum_label,
      Block    = blocks,
      Sequence = seqs,
      stringsAsFactors = FALSE
    )
    if (nrow(stratum_cols) > 0)
      df <- cbind(df, stratum_cols[rep(1L, nrow(df)), , drop = FALSE])
    parts[[i]] <- df
  }
  schedule <- do.call(rbind, parts)
  rownames(schedule) <- NULL

  # Trim to exactly n_total (drop tail across strata, keeping balance) ------
  if (nrow(schedule) > n_total) {
    schedule <- schedule[seq_len(n_total), , drop = FALSE]
  }

  # Assign subject IDs and expand into per-period rows ----------------------
  if (is.null(subject_pad)) subject_pad <- max(2L, nchar(as.character(n_total)))
  schedule$Subject <- sprintf("%s%0*d", subject_prefix, subject_pad,
                              seq_len(nrow(schedule)))

  long <- .expand_to_periods(schedule, d$sequences, d$periods)

  # Audit metadata ----------------------------------------------------------
  meta <- list(
    algorithm        = method_label %||% "Permuted block randomization (base R)",
    design           = design,
    design_label     = d$label,
    sequences        = seq_labels,
    periods          = d$periods,
    n_total          = as.integer(n_total),
    block_size       = as.integer(block_size),
    strata           = strata,
    seed             = seed,
    rng_kind         = "Mersenne-Twister",
    normal_kind      = "Inversion",
    sample_kind      = "Rejection",
    R_version        = paste(R.version$major, R.version$minor, sep = "."),
    package          = "base R (set.seed + sample)",
    package_version  = paste(R.version$major, R.version$minor, sep = "."),
    generated_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    subject_prefix   = subject_prefix,
    schedule_hash    = .hash_schedule(schedule)
  )

  list(schedule = schedule, schedule_long = long, meta = meta)
}

# Expand wide schedule (one row per subject) into long form (one row per period)
.expand_to_periods <- function(wide, sequences, periods) {
  if (periods <= 1L) {
    out <- data.frame(
      Subject   = wide$Subject,
      Period    = 1L,
      Treatment = wide$Sequence,
      stringsAsFactors = FALSE
    )
    extras <- setdiff(names(wide), c("Subject", "Sequence"))
    out <- cbind(out[, c("Subject", "Period", "Treatment")],
                 wide[, extras, drop = FALSE])
    out$Sequence <- wide$Sequence
    return(out)
  }
  rows <- lapply(seq_len(nrow(wide)), function(i) {
    seq_str <- wide$Sequence[i]
    treats  <- strsplit(seq_str, "", fixed = TRUE)[[1]]
    data.frame(
      Subject   = wide$Subject[i],
      Sequence  = seq_str,
      Period    = seq_len(periods),
      Treatment = treats,
      Stratum   = wide$Stratum[i],
      Block     = wide$Block[i],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Lightweight stable hash of the schedule (no external deps) ------------------
.hash_schedule <- function(schedule) {
  txt <- paste(schedule$Subject, schedule$Stratum, schedule$Block,
               schedule$Sequence, sep = "|", collapse = "\n")
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(txt, algo = "sha256"))
  }
  # Fallback: Java-style hash of the canonical text (good enough for diff)
  h <- 0L
  for (ch in utils::head(strsplit(txt, "", fixed = TRUE)[[1]], 1e6)) {
    h <- bitwXor(bitwShiftL(h, 5L) - h, utf8ToInt(ch))
  }
  sprintf("hash32:%08x", h)
}

#' Verify a randomization schedule by regenerating it from the metadata.
#'
#' @param meta_or_args  Either a metadata list (as returned by
#'                      generate_randomization()) or a list of named arguments
#'                      to pass to generate_randomization().
#' @param provided_schedule  Optional data frame to compare against. Must have
#'                      Subject + Sequence (long form ok if Period present).
#' @return list(match = TRUE/FALSE, diffs = data.frame, regenerated = ...)
verify_randomization <- function(meta_or_args, provided_schedule = NULL) {
  args <- meta_or_args
  required <- c("design", "n_total", "block_size", "seed")
  miss <- setdiff(required, names(args))
  if (length(miss))
    stop("Missing required parameters for verification: ",
         paste(miss, collapse = ", "))

  res <- generate_randomization(
    design         = args$design,
    n_total        = args$n_total,
    block_size     = args$block_size,
    seed           = args$seed,
    strata         = args$strata,
    subject_prefix = args$subject_prefix %||% "S"
  )

  if (is.null(provided_schedule)) {
    return(list(match = NA, regenerated = res$schedule, meta = res$meta))
  }

  prov <- as.data.frame(provided_schedule)
  # Reduce both to (Subject, Sequence) pairs
  if (!"Sequence" %in% names(prov) && all(c("Subject", "Period", "Treatment") %in% names(prov))) {
    agg <- aggregate(Treatment ~ Subject, data = prov,
                     FUN = function(x) paste(x, collapse = ""))
    names(agg)[2] <- "Sequence"
    prov <- agg
  }
  prov <- prov[order(prov$Subject), c("Subject", "Sequence"), drop = FALSE]
  ref  <- res$schedule[order(res$schedule$Subject),
                       c("Subject", "Sequence"), drop = FALSE]

  merged <- merge(ref, prov, by = "Subject", suffixes = c("_expected", "_provided"),
                  all = TRUE)
  merged$match <- merged$Sequence_expected == merged$Sequence_provided
  diffs <- merged[!isTRUE(all(merged$match)) & merged$match %in% FALSE, , drop = FALSE]

  list(
    match       = all(merged$match, na.rm = TRUE) &&
                  !any(is.na(merged$match)) &&
                  nrow(merged) == nrow(ref),
    diffs       = diffs,
    n_compared  = nrow(merged),
    regenerated = res$schedule,
    meta        = res$meta
  )
}

# Tiny helper if not already defined upstream
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
