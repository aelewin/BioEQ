# SAS-style Bioequivalence Analysis Report
#
# Self-contained HTML report mirroring the structure of a typical SAS PROC GLM
# bioequivalence annexure. Sections (in order):
#
#   1. Subject-level listings (untransformed and log-transformed)
#   2. Descriptive statistics (Treatment x Period, Sequence, Period, pooled
#      by Treatment)
#   3. Per-product ANOVA on log scale (Reference, and Test if estimable)
#      with Type I and Type III sums of squares
#   4. Full ANOVA on log-transformed PK with Treatment / Sequence / Period /
#      Subject(Sequence): Type I, Type III, LS Means, treatment difference,
#      parameter estimate
#   5. Intra-subject variability summary
#   6. Final BE conclusion (LSM Test/Ref, GeoLSMeans, ratio, 90% CI, power,
#      Intra CV%-Ref, acceptance limits, BE Yes/No)
#
# All ANOVA, LS Means and CI work is performed on log-transformed values;
# descriptive statistics are presented on untransformed values. Where possible
# the report reuses results already attached to be_results / nca_results;
# additional per-product ANOVA tables and LS Means are computed on the fly
# from subject-level data via lm() refits.

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
.fmt_num <- function(x, digits = 4) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) return("\u2014")
  formatC(x, format = "f", digits = digits, big.mark = "")
}
.fmt_pct <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  paste0(formatC(x, format = "f", digits = digits), "%")
}
.fmt_int <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  formatC(round(x), format = "d", big.mark = "")
}
.fmt_p <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) return("\u2014")
  if (x < 0.0001) return("&lt;.0001")
  formatC(x, format = "f", digits = 4)
}
.html_escape <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

.df_to_html <- function(df, caption = NULL, escape = TRUE) {
  if (is.null(df) || nrow(df) == 0) return("")
  cols <- colnames(df)
  thead <- paste0("<thead><tr>",
                  paste0("<th>", vapply(cols, .html_escape, ""), "</th>",
                         collapse = ""),
                  "</tr></thead>")
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    cells <- vapply(cols, function(cn) {
      v <- df[i, cn]
      if (escape) .html_escape(v) else as.character(v)
    }, character(1))
    rows[i] <- paste0("<tr>",
                      paste0("<td>", cells, "</td>", collapse = ""),
                      "</tr>")
  }
  tbody <- paste0("<tbody>", paste(rows, collapse = ""), "</tbody>")
  cap <- if (!is.null(caption)) paste0("<caption>", .html_escape(caption),
                                       "</caption>") else ""
  paste0("<table class='sas-table'>", cap, thead, tbody, "</table>")
}

# Display label for a parameter: "lnCmax" -> "Cmax", etc.
.display_param <- function(p) {
  if (exists("log_param_to_display_name", mode = "function")) {
    return(log_param_to_display_name(p))
  }
  if (startsWith(p, "ln")) substring(p, 3) else p
}

# Pull the subject-level NCA data frame in a tolerant way and standardise
# common column casing.
.get_subject_data <- function(nca_results) {
  sd <- if (is.data.frame(nca_results)) nca_results
        else nca_results$subject_data %||% nca_results$parameters
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0) return(NULL)
  # Normalise common alias columns
  rename_map <- c(subj = "Subject", subject = "Subject",
                  tmt = "Treatment", treat = "Treatment",
                  treatment = "Treatment", FORM = "Treatment",
                  period = "Period", seq = "Sequence",
                  sequence = "Sequence")
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(sd) && !(new %in% names(sd))) {
      names(sd)[names(sd) == old] <- new
    }
  }
  sd
}

# Identify available numeric PK parameter columns.
.pk_columns <- function(sd) {
  if (is.null(sd)) return(character(0))
  id_cols <- c("Subject", "Treatment", "Period", "Sequence", "Time",
               "Concentration", "Group", "dose", "Dose", "Formulation")
  cand <- setdiff(names(sd), id_cols)
  cand <- cand[vapply(cand, function(cn) is.numeric(sd[[cn]]), logical(1))]
  preferred <- c("Cmax", "AUC0t", "AUClast", "AUC0inf", "AUCinf",
                 "Tmax", "T12", "t_half", "THALF", "Kel", "KEL",
                 "Lambda_z", "lambda_z")
  ordered <- intersect(preferred, cand)
  c(ordered, setdiff(cand, ordered))
}

# Pretty header columns for subject listings
.listing_id_cols <- function(sd) {
  out <- intersect(c("Subject", "Sequence", "Period", "Treatment"), names(sd))
  if (length(out) == 0) names(sd)[1:min(4, ncol(sd))] else out
}

# ---------------------------------------------------------------------------
# Descriptive statistics helpers
# ---------------------------------------------------------------------------
.describe_one_group <- function(sub, pk_cols) {
  rows <- lapply(pk_cols, function(p) {
    v <- suppressWarnings(as.numeric(sub[[p]]))
    v <- v[is.finite(v)]
    if (length(v) == 0) {
      return(data.frame(Variable = p, N = 0L,
                        Mean = NA_real_, SD = NA_real_,
                        Min = NA_real_, Median = NA_real_, Max = NA_real_,
                        CV_pct = NA_real_, stringsAsFactors = FALSE))
    }
    m <- mean(v); sdv <- if (length(v) > 1) sd(v) else NA_real_
    cv <- if (!is.na(sdv) && m != 0) sdv / m * 100 else NA_real_
    data.frame(Variable = p, N = length(v), Mean = m, SD = sdv,
               Min = min(v), Median = median(v), Max = max(v),
               CV_pct = cv, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.format_descr <- function(df) {
  # NCA parameter values shown at 2 decimal places throughout (matches the SAS
  # reference output's display convention, and the per-subject listing below —
  # these were previously inconsistent, showing the same raw values at 4
  # decimals here but 2 decimals in .section_subject_listing).
  data.frame(
    Variable = df$Variable,
    N = vapply(df$N, .fmt_int, ""),
    Mean = vapply(df$Mean, .fmt_num, "", digits = 2),
    `Std Dev` = vapply(df$SD, .fmt_num, "", digits = 2),
    Minimum = vapply(df$Min, .fmt_num, "", digits = 2),
    Median = vapply(df$Median, .fmt_num, "", digits = 2),
    Maximum = vapply(df$Max, .fmt_num, "", digits = 2),
    `Coeff of Variation (%)` = vapply(df$CV_pct, .fmt_num, "", digits = 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.section_descriptives_grouped <- function(sd, pk_cols, group_cols, group_label) {
  if (is.null(sd) || length(pk_cols) == 0) return("")
  group_cols <- intersect(group_cols, names(sd))
  if (length(group_cols) == 0) return("")
  if (any(vapply(group_cols, function(g) length(unique(stats::na.omit(sd[[g]]))) < 1,
                 logical(1)))) return("")
  # Build group key
  key <- do.call(paste, c(lapply(group_cols, function(g) sd[[g]]), sep = "|"))
  uniq <- sort(unique(key))
  if (length(uniq) == 0) return("")
  blocks <- character(length(uniq))
  for (i in seq_along(uniq)) {
    sub <- sd[key == uniq[i], , drop = FALSE]
    label_parts <- strsplit(uniq[i], "|", fixed = TRUE)[[1]]
    label <- paste(paste0(group_cols, " = ", label_parts), collapse = ", ")
    tab <- .describe_one_group(sub, pk_cols)
    blocks[i] <- paste0(
      "<h4 class='sas-subhead'>", .html_escape(label),
      " &mdash; N<sub>obs</sub> = ", .fmt_int(nrow(sub)), "</h4>",
      .df_to_html(.format_descr(tab))
    )
  }
  paste(blocks, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Subject-level listings
# ---------------------------------------------------------------------------
.section_subject_listing <- function(sd, pk_cols, log_only = FALSE) {
  if (is.null(sd) || nrow(sd) == 0) return("")
  id_cols <- .listing_id_cols(sd)
  if (log_only) {
    log_pk <- intersect(c("Cmax", "AUC0t", "AUClast", "AUC0inf", "AUCinf"), pk_cols)
    if (length(log_pk) == 0) return("")
    show <- sd[, c(id_cols, log_pk), drop = FALSE]
    for (p in log_pk) {
      v <- suppressWarnings(as.numeric(show[[p]]))
      v[!is.finite(v) | v <= 0] <- NA
      show[[p]] <- log(v)
    }
    names(show)[(length(id_cols) + 1):ncol(show)] <-
      paste0("ln", names(show)[(length(id_cols) + 1):ncol(show)])
  } else {
    show <- sd[, c(id_cols, pk_cols), drop = FALSE]
  }
  # Pretty-format numerics
  pretty <- show
  for (cn in names(pretty)) {
    if (is.numeric(pretty[[cn]])) {
      pretty[[cn]] <- vapply(pretty[[cn]], .fmt_num, "",
                             digits = if (log_only) 4 else 2)
    } else {
      pretty[[cn]] <- vapply(pretty[[cn]], function(x) .html_escape(x),
                             character(1))
    }
  }
  pretty <- cbind(Obs = seq_len(nrow(pretty)), pretty)
  pretty$Obs <- as.character(pretty$Obs)
  .df_to_html(pretty)
}

# ---------------------------------------------------------------------------
# ANOVA helpers (refitting on log scale)
# ---------------------------------------------------------------------------
# Build the modelling frame: y = log(value), Subject/Treatment/Period/Sequence
.build_model_frame <- function(sd, pk_col) {
  if (!pk_col %in% names(sd)) return(NULL)
  v <- suppressWarnings(as.numeric(sd[[pk_col]]))
  ok <- is.finite(v) & v > 0
  d <- data.frame(
    y = log(v[ok]),
    Subject = if ("Subject" %in% names(sd)) sd$Subject[ok] else seq_along(v)[ok],
    Treatment = if ("Treatment" %in% names(sd)) as.character(sd$Treatment)[ok] else NA,
    Period = if ("Period" %in% names(sd)) as.character(sd$Period)[ok] else NA,
    Sequence = if ("Sequence" %in% names(sd)) as.character(sd$Sequence)[ok] else NA,
    stringsAsFactors = FALSE
  )
  d <- d[is.finite(d$y), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$Subject <- factor(d$Subject)
  d$Treatment <- factor(d$Treatment)
  if (any(!is.na(d$Period))) d$Period <- factor(d$Period)
  if (any(!is.na(d$Sequence))) d$Sequence <- factor(d$Sequence)
  d
}

.class_info_table <- function(d, include_form = TRUE) {
  rows <- list()
  add_row <- function(name, levs) {
    rows[[length(rows) + 1]] <<- data.frame(
      Class = name, Levels = length(levs),
      Values = paste(levs, collapse = " "),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(d$Subject)) add_row("Subject", levels(d$Subject))
  if (include_form && !is.null(d$Treatment)) add_row("Treatment", levels(d$Treatment))
  if (!is.null(d$Period) && is.factor(d$Period)) add_row("Period", levels(d$Period))
  if (!is.null(d$Sequence) && is.factor(d$Sequence)) add_row("Sequence", levels(d$Sequence))
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Render a model summary block: "Sum of Squares" + R-Square / Coeff Var lines
.render_model_summary <- function(fit, dep_label) {
  yhat <- fitted(fit); y <- model.response(model.frame(fit))
  ss_total <- sum((y - mean(y))^2)
  ss_resid <- sum(residuals(fit)^2)
  ss_model <- ss_total - ss_resid
  df_model <- fit$rank - 1
  df_resid <- df.residual(fit)
  ms_model <- ss_model / df_model
  ms_resid <- ss_resid / df_resid
  fval <- ms_model / ms_resid
  pval <- pf(fval, df_model, df_resid, lower.tail = FALSE)
  r2 <- 1 - ss_resid / ss_total
  rmse <- sqrt(ms_resid)
  ymean <- mean(y)
  cv_pct <- if (ymean != 0) 100 * rmse / abs(ymean) else NA

  top <- data.frame(
    Source = c("Model", "Error", "Corrected Total"),
    DF = c(.fmt_int(df_model), .fmt_int(df_resid),
           .fmt_int(df_model + df_resid)),
    `Sum of Squares` = c(.fmt_num(ss_model, 8),
                         .fmt_num(ss_resid, 8),
                         .fmt_num(ss_total, 8)),
    `Mean Square` = c(.fmt_num(ms_model, 8),
                      .fmt_num(ms_resid, 8), ""),
    `F Value` = c(.fmt_num(fval, 2), "", ""),
    `Pr > F` = c(.fmt_p(pval), "", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  bottom <- data.frame(
    `R-Square` = .fmt_num(r2, 6),
    `Coeff Var` = .fmt_num(cv_pct, 6),
    `Root MSE` = .fmt_num(rmse, 6),
    `Mean` = .fmt_num(ymean, 6),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(bottom)[4] <- paste0(dep_label, " Mean")
  paste0(.df_to_html(top), .df_to_html(bottom))
}

# Render a Type-I or Type-III SS table (4 columns: Source, DF, SS, MS, F, Pr>F).
.render_ss_table <- function(ss_df, n_obs = NULL) {
  if (is.null(ss_df) || nrow(ss_df) == 0) return("")
  cols <- names(ss_df)
  src_col <- if ("Source" %in% cols) "Source" else NULL
  if (is.null(src_col)) {
    src <- rownames(ss_df); rn <- TRUE
  } else { src <- ss_df[[src_col]]; rn <- FALSE }
  df_v   <- ss_df[[intersect(c("Df", "DF", "numDF"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  ss_v   <- ss_df[["Sum Sq"]] %||% rep(NA, nrow(ss_df))
  ms_v   <- ss_df[["Mean Sq"]] %||%
            (suppressWarnings(as.numeric(ss_v) / as.numeric(df_v)))
  fv_v   <- ss_df[[intersect(c("F value", "F-value"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  pv_v   <- ss_df[[intersect(c("Pr(>F)", "p-value"), cols)[1]]] %||% rep(NA, nrow(ss_df))
  out <- data.frame(
    Source = vapply(src, .html_escape, ""),
    DF     = vapply(df_v, .fmt_int, ""),
    `Sum of Squares` = vapply(ss_v, .fmt_num, "", digits = 8),
    `Mean Square`    = vapply(ms_v, .fmt_num, "", digits = 8),
    `F Value` = vapply(fv_v, .fmt_num, "", digits = 2),
    `Pr > F`  = vapply(pv_v, .fmt_p, ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  .df_to_html(out)
}

# Type III via drop1 (matches simple_anova.R).  For aliased Seq, returns
# a row with df=0 / SS=0 (matching SAS output for nested designs).
.compute_type3 <- function(fit, term_order) {
  d1 <- tryCatch(suppressWarnings(drop1(fit, test = "F")),
                 error = function(e) NULL)
  if (is.null(d1)) return(NULL)
  d1 <- as.data.frame(d1)
  d1 <- d1[rownames(d1) != "<none>", , drop = FALSE]
  rss_full <- sum(residuals(fit)^2)
  d1$`Sum Sq` <- d1$RSS - rss_full
  d1$`Mean Sq` <- d1$`Sum Sq` / d1$Df
  d1$`F value` <- d1$`F value`
  rn_map <- rownames(d1)
  out_rows <- list()
  for (tm in term_order) {
    matched <- rn_map[grepl(paste0("^", gsub(":", ".*:", tm), "$"), rn_map) |
                      tolower(rn_map) == tolower(tm) |
                      grepl(paste0("\\b", tm, "\\b"), rn_map)]
    matched <- matched[!is.na(matched)]
    if (length(matched) == 0) {
      out_rows[[length(out_rows) + 1]] <- data.frame(
        Source = tm, Df = 0, `Sum Sq` = 0, `Mean Sq` = NA,
        `F value` = NA, `Pr(>F)` = NA,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    } else {
      r <- d1[matched[1], , drop = FALSE]
      out_rows[[length(out_rows) + 1]] <- data.frame(
        Source = tm,
        Df = r$Df,
        `Sum Sq` = r$`Sum Sq`,
        `Mean Sq` = r$`Mean Sq`,
        `F value` = r$`F value`,
        `Pr(>F)` = r$`Pr(>F)`,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out_rows)
}

# ---------------------------------------------------------------------------
# Section 3: Per-product ANOVA (Reference, Test if estimable)
# ---------------------------------------------------------------------------
.section_per_product_anova <- function(sd, pk_cols, be_results) {
  if (is.null(sd)) return("")
  ref_lvl <- be_results$reference_treatment %||% "R"
  test_lvl <- be_results$test_treatment %||% "T"
  treat_levels <- if ("Treatment" %in% names(sd))
                    sort(unique(as.character(sd$Treatment))) else character(0)
  if (!ref_lvl %in% treat_levels) {
    # Fall back: pick alphabetically first as Ref (matches SAS A=Test, B=Ref convention check)
    ref_lvl <- if (length(treat_levels) >= 1) treat_levels[1] else "R"
  }
  if (!test_lvl %in% treat_levels && length(treat_levels) >= 2) {
    test_lvl <- setdiff(treat_levels, ref_lvl)[1]
  }

  # Filter to log-PK columns of interest (Cmax, AUC0t)
  primary_pk <- intersect(c("Cmax", "AUC0t", "AUClast", "AUC0inf"), pk_cols)
  if (length(primary_pk) == 0) primary_pk <- pk_cols[1]

  fit_one_product <- function(d_product, dep_label) {
    if (is.null(d_product) || nrow(d_product) < 4) return(NULL)
    # Need within-subject replication for residual error
    rc <- table(d_product$Subject)
    if (!any(rc >= 2)) return(NULL)
    has_seq <- !is.null(d_product$Sequence) && is.factor(d_product$Sequence) &&
               nlevels(droplevels(d_product$Sequence)) >= 2
    has_per <- !is.null(d_product$Period) && is.factor(d_product$Period) &&
               nlevels(droplevels(d_product$Period)) >= 2
    d_product$Subject <- droplevels(d_product$Subject)
    if (has_seq) d_product$Sequence <- droplevels(d_product$Sequence)
    if (has_per) d_product$Period <- droplevels(d_product$Period)

    if (has_seq) {
      fml <- y ~ Sequence + Subject:Sequence + Period
      term_order <- c("Sequence", "Subject(Sequence)", "Period")
    } else {
      fml <- y ~ Subject + Period
      term_order <- c("Subject", "Period")
    }
    if (!has_per) {
      fml <- update(fml, . ~ . - Period)
      term_order <- setdiff(term_order, "Period")
    }
    fit <- tryCatch(lm(fml, data = d_product), error = function(e) NULL)
    if (is.null(fit) || df.residual(fit) <= 0) return(NULL)

    cls_html <- .df_to_html(.class_info_table(d_product, include_form = FALSE))
    n_obs_html <- paste0(
      "<table class='sas-table sas-meta-tbl'>",
      "<tr><th>Number of Observations Read</th><td>", nrow(d_product),
      "</td></tr>",
      "<tr><th>Number of Observations Used</th><td>", nrow(d_product),
      "</td></tr></table>"
    )

    # Type I SS via anova()
    type1_df <- as.data.frame(anova(fit))
    type1_df$Source <- rownames(type1_df)
    # Rename "Sequence:Subject" -> "Subject(Sequence)" for SAS-style display
    type1_df$Source[grepl("Sequence:Subject|Subject:Sequence",
                          type1_df$Source)] <- "Subject(Sequence)"

    # Type III via drop1 trick (Sequence aliased -> 0 rows)
    type3_raw <- .compute_type3(fit, term_order)

    paste0(
      "<h3 class='sas-subhead'>The GLM Procedure &mdash; Dependent Variable: ",
      .html_escape(dep_label), "</h3>",
      cls_html, n_obs_html,
      "<h4 class='sas-subhead'>Model summary</h4>",
      .render_model_summary(fit, dep_label),
      "<h4 class='sas-subhead'>Type I (sequential) Sums of Squares</h4>",
      .render_ss_table(type1_df),
      if (!is.null(type3_raw)) paste0(
        "<h4 class='sas-subhead'>Type III (marginal) Sums of Squares</h4>",
        .render_ss_table(type3_raw)
      ) else ""
    )
  }

  blocks <- character(0)
  for (p in primary_pk) {
    d <- .build_model_frame(sd, p)
    if (is.null(d)) next
    log_label <- paste0("ln", p)

    # Reference subset
    d_ref <- d[as.character(d$Treatment) == ref_lvl, , drop = FALSE]
    ref_block <- fit_one_product(d_ref, log_label)
    if (!is.null(ref_block)) {
      blocks <- c(blocks, paste0(
        "<h3 class='sas-subhead'>ANOVA of log-transformed ",
        .html_escape(p),
        " for calculation of Intra CV% &mdash; Reference (",
        .html_escape(ref_lvl), ")</h3>",
        ref_block,
        "<hr class='sas-rule'>"
      ))
    }

    # Test subset (only if estimable, i.e. replicate design)
    if (test_lvl %in% as.character(d$Treatment)) {
      d_test <- d[as.character(d$Treatment) == test_lvl, , drop = FALSE]
      test_block <- fit_one_product(d_test, log_label)
      if (!is.null(test_block)) {
        blocks <- c(blocks, paste0(
          "<h3 class='sas-subhead'>ANOVA of log-transformed ",
          .html_escape(p),
          " for calculation of Intra CV% &mdash; Test (",
          .html_escape(test_lvl), ")</h3>",
          test_block,
          "<hr class='sas-rule'>"
        ))
      }
    }
  }
  paste(blocks, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Section 4: Full ANOVA on log-transformed data with Treatment effect
# ---------------------------------------------------------------------------
.section_full_anova <- function(sd, pk_cols, be_results) {
  primary_pk <- intersect(c("Cmax", "AUC0t", "AUClast", "AUC0inf"), pk_cols)
  if (length(primary_pk) == 0) primary_pk <- pk_cols[1]

  blocks <- character(0)
  for (p in primary_pk) {
    d <- .build_model_frame(sd, p)
    if (is.null(d) || !is.factor(d$Treatment) ||
        nlevels(droplevels(d$Treatment)) < 2) next
    d$Treatment <- droplevels(d$Treatment)
    has_seq <- is.factor(d$Sequence) && nlevels(droplevels(d$Sequence)) >= 2
    has_per <- is.factor(d$Period) && nlevels(droplevels(d$Period)) >= 2
    if (has_seq) d$Sequence <- droplevels(d$Sequence)
    if (has_per) d$Period <- droplevels(d$Period)
    d$Subject <- droplevels(d$Subject)

    if (has_seq) {
      fml <- y ~ Sequence + Subject:Sequence + Period + Treatment
      term_order <- c("Sequence", "Subject(Sequence)", "Period", "Treatment")
    } else {
      fml <- y ~ Subject + Period + Treatment
      term_order <- c("Subject", "Period", "Treatment")
    }
    if (!has_per) {
      fml <- update(fml, . ~ . - Period)
      term_order <- setdiff(term_order, "Period")
    }
    fit <- tryCatch(lm(fml, data = d), error = function(e) NULL)
    if (is.null(fit) || df.residual(fit) <= 0) next

    log_label <- paste0("ln", p)

    # Class info & N
    cls_html <- .df_to_html(.class_info_table(d, include_form = TRUE))
    n_obs_html <- paste0(
      "<table class='sas-table sas-meta-tbl'>",
      "<tr><th>Number of Observations Read</th><td>", nrow(d), "</td></tr>",
      "<tr><th>Number of Observations Used</th><td>", nrow(d), "</td></tr>",
      "</table>"
    )

    # Type I SS
    type1_df <- as.data.frame(anova(fit))
    type1_df$Source <- rownames(type1_df)
    type1_df$Source[grepl("Sequence:Subject|Subject:Sequence",
                          type1_df$Source)] <- "Subject(Sequence)"

    # Type III SS
    type3_raw <- .compute_type3(fit, term_order)

    # LS Means and treatment difference (Treatment levels sorted alphabetically)
    treat_levels <- levels(d$Treatment)
    n_levels <- length(treat_levels)
    ls_means <- numeric(n_levels); names(ls_means) <- treat_levels
    for (lv in treat_levels) {
      d_pred <- d; d_pred$Treatment <- factor(lv, levels = treat_levels)
      ls_means[lv] <- mean(predict(fit, newdata = d_pred))
    }
    diff_estimate <- ls_means[1] - ls_means[2]   # "Level1 - Level2"
    coef_idx <- grep("^Treatment", names(coef(fit)))
    se_diff <- if (length(coef_idx) >= 1) {
      summary(fit)$coefficients[coef_idx[1], "Std. Error"]
    } else NA_real_
    rdf <- df.residual(fit)
    t_crit <- qt(0.95, df = rdf)  # one-sided 95% -> 90% CI
    diff_lo <- diff_estimate - t_crit * se_diff
    diff_hi <- diff_estimate + t_crit * se_diff
    t_value <- diff_estimate / se_diff
    p_value <- 2 * pt(-abs(t_value), df = rdf)

    ls_tab <- data.frame(
      Treatment = treat_levels,
      `LS Mean` = vapply(ls_means, .fmt_num, "", digits = 6),
      `90% CI Lower` = vapply(ls_means - t_crit * se_diff / sqrt(2),
                              .fmt_num, "", digits = 6),
      `90% CI Upper` = vapply(ls_means + t_crit * se_diff / sqrt(2),
                              .fmt_num, "", digits = 6),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    diff_tab <- data.frame(
      Comparison = paste0(treat_levels[1], " \u2212 ", treat_levels[2]),
      Estimate = .fmt_num(diff_estimate, 6),
      `Std Error` = .fmt_num(se_diff, 6),
      DF = .fmt_int(rdf),
      `t Value` = .fmt_num(t_value, 2),
      `Pr > |t|` = .fmt_p(p_value),
      `90% CI Lower` = .fmt_num(diff_lo, 6),
      `90% CI Upper` = .fmt_num(diff_hi, 6),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    blocks <- c(blocks, paste0(
      "<h3 class='sas-subhead'>ANOVA for log-transformed ",
      .html_escape(p), "</h3>",
      "<h4 class='sas-subhead'>Class Level Information</h4>",
      cls_html, n_obs_html,
      "<h4 class='sas-subhead'>Model summary &mdash; Dependent Variable: ",
      .html_escape(log_label), "</h4>",
      .render_model_summary(fit, log_label),
      "<h4 class='sas-subhead'>Type I (sequential) Sums of Squares</h4>",
      .render_ss_table(type1_df),
      if (!is.null(type3_raw)) paste0(
        "<h4 class='sas-subhead'>Type III (marginal) Sums of Squares</h4>",
        .render_ss_table(type3_raw)
      ) else "",
      "<h4 class='sas-subhead'>Least Squares Means &mdash; Treatment</h4>",
      .df_to_html(ls_tab),
      "<h4 class='sas-subhead'>Treatment Difference (log scale)</h4>",
      .df_to_html(diff_tab),
      "<hr class='sas-rule'>"
    ))
  }
  paste(blocks, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Section 5: Intra-subject variability summary
# ---------------------------------------------------------------------------
.section_intra_cv <- function(anova_data) {
  if (is.null(anova_data) || length(anova_data) == 0) return("")
  rows <- list()
  for (p in names(anova_data)) {
    pr <- anova_data[[p]]
    if (is.null(pr) || "error" %in% names(pr)) next
    disp <- .display_param(p)
    cvR <- NA_real_; s2R <- NA_real_; cvT <- NA_real_; s2T <- NA_real_
    src <- ""
    if (!is.null(pr$replicatebe_output)) {
      rb <- pr$replicatebe_output
      cvR <- suppressWarnings(as.numeric(rb$`CVwR(%)`))
      cvT <- suppressWarnings(as.numeric(rb$`CVwT(%)`))
      sR <- suppressWarnings(as.numeric(rb$swR))
      sT <- suppressWarnings(as.numeric(rb$swT))
      if (!is.na(sR)) s2R <- sR^2
      if (!is.na(sT)) s2T <- sT^2
      src <- "ABEL (replicateBE)"
    } else if (!is.null(pr$s2_wR)) {
      s2R <- as.numeric(pr$s2_wR)
      s2T <- as.numeric(pr$s2_wT %||% NA)
      cvR <- as.numeric(pr$cv_wr_percent %||% NA)
      cvT <- as.numeric(pr$cv_wt_percent %||% NA)
      src <- "RSABE (ISC)"
    } else if (!is.null(pr$residual_mse)) {
      mse <- as.numeric(pr$residual_mse)
      if (!is.na(mse) && mse >= 0 && mse < 5) {
        s2R <- mse; cvR <- 100 * sqrt(exp(mse) - 1)
        src <- "ABE (pooled residual MSE)"
      }
    }
    if (is.na(cvR) && is.na(s2R)) next
    rows[[length(rows) + 1]] <- data.frame(
      Parameter = disp,
      CVwR_pct  = .fmt_pct(cvR),
      swR_      = if (!is.na(s2R) && s2R >= 0) .fmt_num(sqrt(s2R), 4) else "\u2014",
      s2wR      = .fmt_num(s2R, 6),
      CVwT_pct  = .fmt_pct(cvT),
      swT_      = if (!is.na(s2T) && s2T >= 0) .fmt_num(sqrt(s2T), 4) else "\u2014",
      s2wT      = .fmt_num(s2T, 6),
      Source    = src,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("Parameter", "CVwR (%)", "swR", "s\u00b2wR",
                     "CVwT (%)", "swT", "s\u00b2wT", "Source")
  paste0(
    .df_to_html(tab),
    "<p class='sas-note'>CV<sub>w</sub>% = 100 \u00d7 \u221a(e<sup>s\u00b2</sup> &minus; 1) on the log scale.</p>"
  )
}

# ---------------------------------------------------------------------------
# Analysis Configuration summary — the parameters selected for this run
# ---------------------------------------------------------------------------
.section_analysis_config <- function(analysis_config, be_results) {
  if (is.null(analysis_config)) return("")
  ac <- analysis_config
  analysis_type <- be_results$analysis_type %||% ac$be_analysis_type %||% "ABE"
  is_parallel <- identical(ac$study_design, "parallel") ||
                 identical(be_results$design, "parallel") ||
                 !is.null(be_results$statistical_results)

  rows <- list()
  add <- function(label, value) {
    if (!is.null(value) && !(is.character(value) && !nzchar(value)) && !is.na(value))
      rows[[length(rows) + 1]] <<- c(label, as.character(value))
  }

  add("Study Design", be_results$design %||% ac$study_design)
  add("Bioequivalence Analysis Type", analysis_type)
  if (identical(analysis_type, "RSABE")) {
    add("RSABE Method", switch(ac$rsabe_method %||% "fda_linearized",
                                "fda_linearized" = "FDA Linearized Scaled Criterion (Howe UCB)",
                                "nctost" = "Non-Central TOST (ncTOST)",
                                ac$rsabe_method))
  } else if (identical(analysis_type, "ABEL")) {
    add("ABEL-eligible Parameters",
        paste(ac$abel_eligible_params %||% character(0), collapse = ", "))
  }
  add("ANOVA Model", switch(ac$anova_model %||% "fixed",
                             "fixed" = "Fixed Effects (lm)",
                             "nlme" = "Mixed Effects — nlme (REML)",
                             "satterthwaite" = "Mixed Effects — Satterthwaite DF",
                             "kenward-roger" = "Mixed Effects — Kenward-Roger DF",
                             ac$anova_model))
  if (!is_parallel && !identical(ac$anova_model, "fixed"))
    add("Random Effects Structure", ac$random_effects)
  if (is_parallel)
    add("Parallel Variance Assumption",
        if (isTRUE(ac$welch_correction)) "Welch correction (unequal variances)"
        else "Equal variances assumption")
  add("PK Parameters Analyzed",
      paste(ac$selected_pk_params %||% ac$pk_parameters %||% character(0), collapse = ", "))
  add("AUC Calculation Method", ac$auc_method)
  add("Lambda_z Estimation Method", ac$lambda_z_method)
  add("Missing Data Handling (middle points)", ac$missing_data_middle)
  add("Missing Data Handling (terminal points)", ac$missing_data_terminal)
  add("Carryover Assessment (ICH M13A)",
      if (isTRUE(ac$test_carryover))
        sprintf("Performed (exclusion threshold %s%%)", ac$carryover_threshold)
      else "Not performed")
  alpha_cfg <- ac$alpha_level %||% 0.05
  add("Alpha / Confidence Level",
      sprintf("%.3f (%.0f%% CI)", alpha_cfg, (1 - 2 * alpha_cfg) * 100))
  add("Acceptance Limits (default)",
      sprintf("%.2f%% – %.2f%%", ac$be_lower %||% 80, ac$be_upper %||% 125))

  if (length(rows) == 0) return("")
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- c("Setting", "Value")
  .df_to_html(df)
}

# ---------------------------------------------------------------------------
# Statistical Method — parallel-group two-sample t-test
# (crossover designs get their ANOVA from sections 3/4 below; a parallel
# design has no Sequence/Period/Subject(Sequence) structure, so those
# sections render empty — this is the appropriate substitute.)
# ---------------------------------------------------------------------------
.section_parallel_ttest <- function(be_results) {
  ci_results <- be_results$confidence_intervals
  stats_results <- be_results$statistical_results
  if (is.null(ci_results) || is.null(stats_results) || length(stats_results) == 0) return("")

  rows <- list()
  for (p in names(stats_results)) {
    st <- stats_results[[p]]
    ci <- ci_results[[p]]
    if (is.null(st) || is.null(ci)) next
    rows[[length(rows) + 1]] <- data.frame(
      Parameter   = .display_param(p),
      Method      = st$method %||% "Two-sample t-test",
      N_Test      = .fmt_int(st$n_test),
      N_Reference = .fmt_int(st$n_ref),
      t_Statistic = .fmt_num(ci$t_statistic %||% NA, 4),
      DF          = .fmt_num(st$degrees_freedom %||% ci$df %||% NA, 2),
      P_Value     = .fmt_p(st$p_value %||% ci$p_value %||% NA),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("Parameter", "Method", "N (Test)", "N (Reference)",
                     "t Statistic", "DF", "P-value")
  paste0(
    .df_to_html(tab),
    "<p class='sas-note'>Bioequivalence for parallel-group designs is assessed via a ",
    "two-sample t-test on log-transformed data (Welch approximation used for unequal ",
    "variances where selected). Point estimate and confidence interval are shown in the ",
    "Bioequivalence Conclusion below.</p>"
  )
}

# ---------------------------------------------------------------------------
# Reproducible Analysis Summary — the function calls and parameters used
# ---------------------------------------------------------------------------
.section_reproducible_code <- function(analysis_config, be_results, ran_nca) {
  ac <- analysis_config %||% list()
  analysis_type <- be_results$analysis_type %||% ac$be_analysis_type %||% "ABE"
  design <- be_results$design %||% ac$study_design %||% "auto"
  is_parallel <- identical(design, "parallel") || !is.null(be_results$statistical_results)
  params <- ac$selected_pk_params %||% ac$pk_parameters %||% character(0)
  params_str <- paste0("c(", paste(sprintf("\"%s\"", params), collapse = ", "), ")")

  nca_block <- if (isTRUE(ran_nca)) paste0(
    "# Non-Compartmental Analysis (per subject)\n",
    "nca_results <- perform_enhanced_nca_analysis(\n",
    "  data,\n",
    "  lambda_points = ", ac$lambda_z_points %||% 3, "\n",
    ")\n",
    "# AUC method: ", ac$auc_method %||% "mixed", "\n",
    "# Lambda_z method: ", ac$lambda_z_method %||% "aic", "\n\n"
  ) else ""

  extra_params <- character(0)
  if (identical(analysis_type, "RSABE")) {
    extra_params <- c(extra_params, sprintf("    rsabe_method  = \"%s\"",
                                             ac$rsabe_method %||% "fda_linearized"))
  }
  if (identical(analysis_type, "ABEL")) {
    extra_params <- c(extra_params, sprintf(
      "    abel_eligible_params = c(%s)",
      paste(sprintf("\"%s\"", ac$abel_eligible_params %||% "Cmax"), collapse = ", ")))
  }
  if (is_parallel) {
    extra_params <- c(extra_params,
                       sprintf("    welch_correction = %s", isTRUE(ac$welch_correction)))
  } else {
    extra_params <- c(extra_params, sprintf("    anova_model   = \"%s\"",
                                             ac$anova_model %||% "fixed"))
  }

  be_block <- paste0(
    "# Bioequivalence Analysis\n",
    "be_results <- perform_be_analysis_by_type(\n",
    "  data          = ", if (isTRUE(ran_nca)) "nca_results" else "data", ",\n",
    "  analysis_type = \"", analysis_type, "\",\n",
    "  design        = \"", design, "\",\n",
    "  params = list(\n",
    "    alpha_level   = ", ac$alpha_level %||% 0.05, ",\n",
    "    be_limits     = list(lower = ", ac$be_lower %||% 80,
    ", upper = ", ac$be_upper %||% 125, "),\n",
    "    pk_parameters = ", params_str,
    if (length(extra_params) > 0) paste0(",\n", paste(extra_params, collapse = ",\n")) else "",
    "\n  )\n",
    ")\n"
  )

  paste0(
    "<pre class='sas-code'>", .html_escape(paste0(nca_block, be_block)), "</pre>",
    "<p class='sas-note'>Reflects the actual function calls and parameter values used to ",
    "produce this report; provided for reproducibility, not a verbatim execution transcript.</p>"
  )
}

# ---------------------------------------------------------------------------
# Section 6: Final BE summary (SAS-style normal-scale results table)
# ---------------------------------------------------------------------------
.section_be_summary <- function(be_results, sd, alpha) {
  ci_results <- be_results$confidence_intervals
  be_conclusions <- be_results$be_conclusions
  if (is.null(ci_results) || length(ci_results) == 0) return("")
  params <- intersect(names(ci_results), names(be_conclusions))
  if (length(params) == 0) params <- names(ci_results)
  anova_data <- be_results$anova_results$anova_results

  has_powertost <- requireNamespace("PowerTOST", quietly = TRUE)
  design_code <- be_results$design %||% ""

  # Recompute LSM Test / LSM Ref by refitting per primary param when possible.
  pk_to_param <- function(p) {
    base <- if (startsWith(p, "ln")) substring(p, 3) else p
    if (base %in% names(sd)) base else NULL
  }

  rows <- list()
  for (p in params) {
    ci <- ci_results[[p]]
    if (is.null(ci)) next
    disp <- .display_param(p)
    n_val <- ci$n_subjects %||% be_results$n_subjects %||% NA
    pe <- ci$point_estimate
    lo <- ci$ci_lower; hi <- ci$ci_upper
    log_diff <- ci$log_difference %||% (if (!is.null(pe)) log(pe / 100) else NA)
    se_diff  <- ci$treatment_se %||% NA
    df_val   <- ci$degrees_freedom %||% NA
    cv_wr    <- ci$cv_wr %||% NA
    pr_intra <- if (!is.null(anova_data)) anova_data[[p]] else NULL
    if (is.na(cv_wr) && !is.null(pr_intra)) {
      cv_wr <- pr_intra$cv_wr_percent %||%
               (if (!is.null(pr_intra$replicatebe_output))
                  as.numeric(pr_intra$replicatebe_output$`CVwR(%)`) else NA)
    }
    is_be <- be_conclusions[[p]]
    be_lo <- ci$limits_used$lower %||% 80
    be_hi <- ci$limits_used$upper %||% 125

    # LS Means: refit if subject_data available
    ls_test <- NA_real_; ls_ref <- NA_real_
    geo_test <- NA_real_; geo_ref <- NA_real_
    pk_col <- pk_to_param(p)
    if (!is.null(pk_col) && !is.null(sd) && "Treatment" %in% names(sd)) {
      d <- .build_model_frame(sd, pk_col)
      if (!is.null(d) && is.factor(d$Treatment) &&
          nlevels(droplevels(d$Treatment)) >= 2) {
        d$Treatment <- droplevels(d$Treatment)
        d$Subject <- droplevels(d$Subject)
        has_seq <- is.factor(d$Sequence) &&
                   nlevels(droplevels(d$Sequence)) >= 2
        has_per <- is.factor(d$Period) &&
                   nlevels(droplevels(d$Period)) >= 2
        treat_lvls <- levels(d$Treatment)
        # NOTE: be_results$reference_treatment / $test_treatment are never
        # actually set anywhere in the analysis engine (R/be_analysis.R), so
        # relying on them (as this block previously did, falling back to
        # treat_lvls[length(treat_lvls)]/treat_lvls[1]) silently swapped
        # Test and Reference for the standard "R"/"T" labeling convention
        # used throughout the app. Use the literal R/T convention directly.
        ref_lvl <- be_results$reference_treatment %||%
                   (if ("R" %in% treat_lvls) "R" else treat_lvls[length(treat_lvls)])
        test_lvl <- be_results$test_treatment %||%
                    (if ("T" %in% treat_lvls) "T" else treat_lvls[1])
        if (!ref_lvl %in% treat_lvls) ref_lvl <- setdiff(treat_lvls, test_lvl)[1]
        if (!test_lvl %in% treat_lvls) test_lvl <- setdiff(treat_lvls, ref_lvl)[1]

        if (!has_seq && !has_per) {
          # No crossover structure (parallel, or any one-observation-per-
          # subject design): Subject and Treatment are perfectly confounded,
          # so a Subject+Treatment lm() refit cannot estimate a Treatment
          # effect. LS means reduce to simple per-treatment-group means.
          ls_test <- mean(d$y[d$Treatment == test_lvl], na.rm = TRUE)
          ls_ref  <- mean(d$y[d$Treatment == ref_lvl], na.rm = TRUE)
          geo_test <- exp(ls_test); geo_ref <- exp(ls_ref)
          if (is.na(df_val)) df_val <- ci$df %||% NA
        } else {
          if (has_seq) d$Sequence <- droplevels(d$Sequence)
          if (has_per) d$Period <- droplevels(d$Period)
          fml <- if (has_seq) y ~ Sequence + Subject:Sequence + Period + Treatment
                 else y ~ Subject + Period + Treatment
          if (!has_per) fml <- update(fml, . ~ . - Period)
          fit <- tryCatch(lm(fml, data = d), error = function(e) NULL)
          if (!is.null(fit)) {
            predict_at <- function(lv) {
              d_pred <- d; d_pred$Treatment <- factor(lv, levels = treat_lvls)
              mean(predict(fit, newdata = d_pred))
            }
            ls_test <- predict_at(test_lvl)
            ls_ref  <- predict_at(ref_lvl)
            geo_test <- exp(ls_test); geo_ref <- exp(ls_ref)
            if (is.na(df_val)) df_val <- df.residual(fit)
          }
        }
      }
    }

    # Power calc via PowerTOST (post-hoc, given observed CV and ratio)
    power_pct <- NA_real_
    if (has_powertost && !is.na(cv_wr) && !is.na(pe) && !is.na(n_val)) {
      pt_design <- if (grepl("2x2x4", design_code)) "2x2x4"
                   else if (grepl("2x2x3", design_code)) "2x2x3"
                   else if (grepl("parallel", design_code, ignore.case = TRUE)) "parallel"
                   else "2x2x2"
      power_pct <- tryCatch({
        100 * PowerTOST::power.TOST(
          alpha = alpha, theta1 = be_lo / 100, theta2 = be_hi / 100,
          theta0 = pe / 100, CV = cv_wr / 100,
          n = as.integer(n_val), design = pt_design,
          robust = TRUE
        )
      }, error = function(e) NA_real_)
    }

    rows[[length(rows) + 1]] <- data.frame(
      PK_Parameter   = disp,
      LSM_Test       = .fmt_num(ls_test, 4),
      LSM_Ref        = .fmt_num(ls_ref, 4),
      SE_Diff        = .fmt_num(se_diff, 4),
      GeoLSMean_Test = .fmt_num(geo_test, 2),
      GeoLSMean_Ref  = .fmt_num(geo_ref, 2),
      DF_            = .fmt_int(df_val),
      Ratio_pct      = .fmt_num(pe, 2),
      CI_Lower       = .fmt_num(lo, 2),
      CI_Upper       = .fmt_num(hi, 2),
      Power_pct      = .fmt_num(power_pct, 2),
      IntraCV_Ref    = .fmt_pct(cv_wr),
      Lower_Limit    = .fmt_num(be_lo, 2),
      Upper_Limit    = .fmt_num(be_hi, 2),
      Bioequivalence = if (is.na(is_be)) "\u2014"
                          else if (isTRUE(is_be)) "Yes" else "No",
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return("")
  tab <- do.call(rbind, rows)
  colnames(tab) <- c("PK Parameter", "LSM Test", "LSM Ref", "S.E.(Diff)",
                     "GeoLSMean Test", "GeoLSMean Ref", "DF",
                     "Ratio (%)", "90% CI Lower", "90% CI Upper",
                     "Power (%)", "Intra CV% \u2013 Ref",
                     "Lower Limit", "Upper Limit", "Bioequivalence")
  .df_to_html(tab)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------
generate_sas_style_html_report <- function(be_results,
                                           nca_results,
                                           analysis_config = NULL,
                                           output_file,
                                           data_type = NULL) {
  alpha <- analysis_config$alpha_level %||% 0.05
  ci_pct <- round((1 - alpha * 2) * 100)
  ci_label <- paste0(ci_pct, "% CI")
  design <- be_results$design %||% analysis_config$detected_design %||% "Unknown"
  method <- be_results$be_method %||% be_results$analysis_method %||%
            "Average Bioequivalence"
  analysis_type <- be_results$analysis_type %||% "ABE"
  n_subj <- be_results$n_subjects %||% NA
  is_parallel_run <- identical(design, "parallel") || !is.null(be_results$statistical_results)
  ran_nca <- identical(data_type, "concentration")

  sd <- .get_subject_data(nca_results)
  pk_cols <- .pk_columns(sd)

  # ── Analysis Configuration — selected parameters for this run ─────────────
  config_block <- .section_analysis_config(analysis_config, be_results)

  # ── Section 1: Subject-level listings ─────────────────────────────────────
  listing_untrans <- .section_subject_listing(sd, pk_cols, log_only = FALSE)
  listing_log     <- .section_subject_listing(sd, pk_cols, log_only = TRUE)

  # ── Section 2: Descriptive statistics ─────────────────────────────────────
  desc_treat_per <- if (!is.null(sd) && all(c("Treatment", "Period") %in% names(sd)))
    .section_descriptives_grouped(sd, pk_cols,
                                  c("Treatment", "Period"),
                                  "Treatment x Period") else ""
  desc_seq <- if (!is.null(sd) && "Sequence" %in% names(sd) &&
                  length(unique(stats::na.omit(sd$Sequence))) >= 2)
    .section_descriptives_grouped(sd, pk_cols, "Sequence", "Sequence") else ""
  desc_per <- if (!is.null(sd) && "Period" %in% names(sd) &&
                  length(unique(stats::na.omit(sd$Period))) >= 2)
    .section_descriptives_grouped(sd, pk_cols, "Period", "Period") else ""
  desc_treat <- if (!is.null(sd) && "Treatment" %in% names(sd))
    .section_descriptives_grouped(sd, pk_cols, "Treatment",
                                  "Treatment (pooled)") else ""

  # ── Section 3 (parallel only): Statistical Method — two-sample t-test ─────
  parallel_block <- if (is_parallel_run) .section_parallel_ttest(be_results) else ""

  # ── Sections 3 & 4 (crossover/replicate only): ANOVA ───────────────────────
  per_prod_block <- .section_per_product_anova(sd, pk_cols, be_results)
  full_anova_block <- .section_full_anova(sd, pk_cols, be_results)

  # ── Section 5: Intra-subject variability ──────────────────────────────────
  # Not applicable to parallel designs: there is no within-subject repeated
  # dosing at all (each subject receives a single treatment), so no
  # intra-subject variance is estimable, unlike a crossover/replicate design.
  intra_block <- if (is_parallel_run) "" else
    .section_intra_cv(be_results$anova_results$anova_results)

  # ── Section 6: Final BE summary ───────────────────────────────────────────
  be_block <- .section_be_summary(be_results, sd, alpha)

  # ── Reproducible Analysis Summary ─────────────────────────────────────────
  code_block <- .section_reproducible_code(analysis_config, be_results, ran_nca)

  # ── Header ────────────────────────────────────────────────────────────────
  header_html <- paste0(
    "<div class='sas-header'>",
    "<h1>BioEQ &mdash; Bioequivalence Analysis Report</h1>",
    "<p class='sas-meta'>Generated ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
    "<table class='sas-meta-tbl'>",
    "<tr><th>Study Design</th><td>", .html_escape(design), "</td></tr>",
    "<tr><th>Analysis Type</th><td>", .html_escape(analysis_type),
    " &mdash; ", .html_escape(method), "</td></tr>",
    "<tr><th>N Subjects</th><td>", .fmt_int(n_subj), "</td></tr>",
    "<tr><th>Alpha</th><td>", .fmt_num(alpha, 4),
    " (", ci_label, ")</td></tr>",
    "</table></div>"
  )

  # ── Body assembly ─────────────────────────────────────────────────────────
  body <- paste0(
    header_html,

    if (nzchar(config_block)) paste0(
      "<h2 class='sas-section'>Analysis Configuration</h2>",
      config_block) else "",

    if (nzchar(listing_untrans) || nzchar(listing_log))
      "<h2 class='sas-section'>1. Subject-Level Pharmacokinetic Listings</h2>"
    else "",
    if (nzchar(listing_untrans)) paste0(
      "<h3 class='sas-subhead'>1.1 Untransformed Pharmacokinetic Parameters</h3>",
      listing_untrans) else "",
    if (nzchar(listing_log)) paste0(
      "<h3 class='sas-subhead'>1.2 Log-transformed Pharmacokinetic Parameters (lnCmax, lnAUC0-t)</h3>",
      listing_log) else "",

    if (nzchar(desc_treat_per) || nzchar(desc_seq) ||
        nzchar(desc_per) || nzchar(desc_treat))
      "<h2 class='sas-section'>2. Descriptive Statistics &mdash; Untransformed Pharmacokinetic Parameters</h2>"
    else "",
    if (nzchar(desc_treat_per)) paste0(
      "<h3 class='sas-subhead'>2.1 Summary statistics &mdash; for each Treatment x Period</h3>",
      desc_treat_per) else "",
    if (nzchar(desc_seq)) paste0(
      "<h3 class='sas-subhead'>2.2 Summary statistics &mdash; for each Sequence</h3>",
      desc_seq) else "",
    if (nzchar(desc_per)) paste0(
      "<h3 class='sas-subhead'>2.3 Summary statistics &mdash; for each Period</h3>",
      desc_per) else "",
    if (nzchar(desc_treat)) paste0(
      "<h3 class='sas-subhead'>2.4 Pooled summary statistics &mdash; by Treatment</h3>",
      desc_treat) else "",

    if (nzchar(parallel_block)) paste0(
      "<h2 class='sas-section'>3. Statistical Method &mdash; Two-Sample t-test</h2>",
      parallel_block) else "",

    if (nzchar(per_prod_block)) paste0(
      "<h2 class='sas-section'>3. Per-product ANOVA &mdash; Log-transformed Data (Intra CV%)</h2>",
      "<p class='sas-note'>Reference (and Test, when estimable) subset only. ",
      "Model: ln(PK) = Sequence + Subject(Sequence) + Period.</p>",
      per_prod_block) else "",

    if (nzchar(full_anova_block)) paste0(
      "<h2 class='sas-section'>4. Full ANOVA &mdash; Log-transformed Data</h2>",
      "<p class='sas-note'>Model: ln(PK) = Sequence + Subject(Sequence) + ",
      "Period + Treatment. Type III SS uses marginal contributions; ",
      "the Sequence row is aliased with Subject(Sequence) (DF = 0) under ",
      "this nesting (matches SAS PROC GLM).</p>",
      full_anova_block) else "",

    if (nzchar(intra_block)) paste0(
      "<h2 class='sas-section'>5. Intra-subject Variability Summary</h2>",
      intra_block) else "",

    if (nzchar(be_block)) paste0(
      "<h2 class='sas-section'>6. Bioequivalence Conclusion &mdash; Normal-scale Results of Log-transformed Data</h2>",
      be_block) else "",

    if (nzchar(code_block)) paste0(
      "<h2 class='sas-section'>7. Reproducible Analysis Summary</h2>",
      code_block) else "",

    "<hr class='sas-rule'>",
    "<p class='sas-footer'>Generated by BioEQ. ",
    "Descriptive statistics are computed on untransformed PK values; ",
    "ANOVA, intra-subject variability and BE confidence intervals are ",
    "computed on log-transformed values per regulatory convention.</p>"
  )

  css <- "
    body { font-family: 'Helvetica Neue', Arial, sans-serif; color:#212529;
           margin:32px; line-height:1.45; max-width:1200px; }
    .sas-header { border-bottom:2px solid #1e3a5f; padding-bottom:12px;
                   margin-bottom:24px; }
    .sas-header h1 { color:#1e3a5f; margin:0 0 4px 0; font-size:22px; }
    .sas-meta { color:#6c757d; font-size:12px; margin:0 0 10px 0; }
    table.sas-meta-tbl { border-collapse:collapse; font-size:13px; }
    table.sas-meta-tbl th { text-align:left; padding:2px 14px 2px 0;
                       color:#495057; font-weight:600; }
    table.sas-meta-tbl td { padding:2px 0; }
    h2.sas-section { color:#1e3a5f; border-bottom:1px solid #dee2e6;
                     margin-top:32px; padding-bottom:4px; font-size:17px; }
    h3.sas-subhead { color:#2c5282; margin-top:18px; font-size:14px;
                     font-weight:600; }
    h4.sas-subhead { color:#495057; margin:12px 0 6px 0; font-size:13px;
                     font-weight:600; }
    table.sas-table { border-collapse:collapse; margin:6px 0 16px 0;
                      font-size:12px; min-width:50%; }
    table.sas-table caption { caption-side:top; text-align:left;
                              font-weight:600; color:#495057;
                              padding-bottom:4px; }
    table.sas-table th, table.sas-table td {
      border:1px solid #ced4da; padding:4px 10px; text-align:right; }
    table.sas-table thead th { background:#f1f3f5; color:#212529;
                                font-weight:600; text-align:center; }
    table.sas-table tbody tr:nth-child(even) { background:#fafbfc; }
    table.sas-table td:first-child, table.sas-table th:first-child {
      text-align:left; }
    p.sas-note { color:#6c757d; font-size:12px; font-style:italic;
                 margin:4px 0 12px 0; }
    p.sas-footer { color:#6c757d; font-size:11px; margin-top:24px; }
    hr.sas-rule { border:none; border-top:1px dashed #ced4da; margin:18px 0; }
    pre.sas-code { background:#f8f9fa; border:1px solid #dee2e6; border-radius:4px;
                   padding:14px 16px; font-size:12px; line-height:1.5;
                   overflow-x:auto; white-space:pre; font-family:'SF Mono',
                   Consolas,'Courier New',monospace; color:#2d3748; }

    @media print {
      body { margin:12px; max-width:none; }
      h2.sas-section { break-after:avoid; page-break-after:avoid; }
      h3.sas-subhead, h4.sas-subhead { break-after:avoid; page-break-after:avoid; }
      table.sas-table, pre.sas-code { break-inside:avoid; page-break-inside:avoid; }
      table.sas-table tbody tr { break-inside:avoid; page-break-inside:avoid; }
    }
  "

  html <- paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
    "<title>BioEQ \u2013 SAS-style BE Report</title>",
    "<style>", css, "</style></head><body>", body, "</body></html>"
  )

  writeLines(html, output_file, useBytes = TRUE)
  invisible(output_file)
}
