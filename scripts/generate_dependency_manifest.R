#!/usr/bin/env Rscript
# generate_dependency_manifest.R
#
# DEVELOPER SCRIPT — read-only. Installs nothing, changes nothing.
#
# Regenerates the categorized package inventory documented in
# docs/user_guide.md section 2. Run this whenever BioEQ's dependencies change,
# then paste the output into that section, so the documented list never drifts
# away from what the code actually uses.
#
# Usage (from the BioEQ project root):
#   Rscript scripts/generate_dependency_manifest.R
#
# Categories follow the R package-priority convention:
#   Base R           - ship with R (priority "base"); no installation needed
#   Recommended      - ship with R (priority "recommended"); R Core maintained
#   Intended for Use - loaded directly by BioEQ code
#   Imports          - installed automatically to support the above ("back-end")

options(repos = c(CRAN = "https://cloud.r-project.org"))

# ---------------------------------------------------------------------------
# The packages BioEQ loads directly. Keep in sync with install_dependencies.R's
# required_packages (this script checks that for you, below).
# ---------------------------------------------------------------------------
direct <- c(
  "bslib", "digest", "dplyr", "DT", "dtw", "emmeans", "flextable", "ggplot2",
  "htmltools", "htmlwidgets", "knitr", "lme4", "lmerTest", "nlme", "officer",
  "PKNCA", "plotly", "PowerTOST", "progress", "readr", "readxl", "replicateBE",
  "rlang", "rmarkdown", "shiny", "shinycssloaders", "shinydashboard",
  "shinyjs", "tidyr", "writexl", "zip"
)

# Canonical priority lists (R 4.x)
base_r <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
            "methods", "parallel", "splines", "stats", "stats4", "tcltk",
            "tools", "utils")
recommended <- c("boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
                 "lattice", "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart",
                 "spatial", "survival")

cat("Fetching CRAN metadata ...\n")
ap <- available.packages()
ip <- installed.packages()

# ---------------------------------------------------------------------------
# Recursive dependency closure
# ---------------------------------------------------------------------------
deps <- tools::package_dependencies(
  direct, db = ap, recursive = TRUE,
  which = c("Depends", "Imports", "LinkingTo")
)
allp <- sort(unique(c(direct, unlist(deps))))

b <- sort(intersect(allp, base_r))
r <- sort(intersect(allp, recommended))
d <- sort(setdiff(intersect(allp, direct), c(base_r, recommended)))
i <- sort(setdiff(allp, c(b, r, d)))

ver <- function(p) {
  if (p %in% rownames(ip)) return(as.character(ip[p, "Version"]))
  if (p %in% rownames(ap)) return(paste0(ap[p, "Version"], " (CRAN; not installed locally)"))
  "-"
}

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
cat(sprintf("\n%s\nBioEQ DEPENDENCY MANIFEST  (generated %s)\n%s\n",
            strrep("=", 62), Sys.Date(), strrep("=", 62)))
cat(sprintf("R version used to generate: %s\n", as.character(getRversion())))
cat(sprintf("TOTAL PACKAGES: %d   (base %d | recommended %d | intended-for-use %d | imports %d)\n",
            length(allp), length(b), length(r), length(d), length(i)))

cat(sprintf("\n-- 1. BASE R (%d) --\n%s\n", length(b), paste(b, collapse = ", ")))

cat(sprintf("\n-- 2. RECOMMENDED (%d) --\n", length(r)))
for (p in r) {
  cat(sprintf("  %-16s %s%s\n", p, ver(p),
              if (p %in% direct) "   [ALSO loaded directly by BioEQ]" else ""))
}

cat(sprintf("\n-- 3. INTENDED FOR USE (%d) --\n", length(d)))
for (p in d) cat(sprintf("  %-16s %s\n", p, ver(p)))

cat(sprintf("\n-- 4. IMPORTS / BACK-END (%d) --\n%s\n",
            length(i), paste(i, collapse = ", ")))

# ---------------------------------------------------------------------------
# Drift checks against install_dependencies.R
# ---------------------------------------------------------------------------
cat(sprintf("\n%s\nDRIFT CHECKS\n%s\n", strrep("=", 62), strrep("=", 62)))

inst_file <- "install_dependencies.R"
if (file.exists(inst_file)) {
  # Parse the file and evaluate ONLY the `required_packages <- c(...)` literal.
  # (Regex-scraping the source is fragile: a ")" inside a comment truncates the
  # match. Parsing is exact, and evaluating a bare c("a","b") literal has no
  # side effects — the install calls in that file are never run.)
  declared <- character(0)
  for (e in parse(inst_file)) {
    if (is.call(e) && length(e) >= 3 &&
        as.character(e[[1]])[1] %in% c("<-", "=", "<<-") &&
        identical(as.character(e[[2]])[1], "required_packages")) {
      declared <- sort(unique(as.character(eval(e[[3]]))))
      break
    }
  }
  if (!length(declared)) {
    cat("⚠ Could not locate `required_packages` in install_dependencies.R.\n")
  }

  only_declared <- setdiff(declared, direct)
  only_here     <- setdiff(direct, declared)

  if (!length(only_declared) && !length(only_here)) {
    cat("✓ install_dependencies.R matches this script's direct list.\n")
  } else {
    if (length(only_declared))
      cat("⚠ In install_dependencies.R but not in this script:",
          paste(only_declared, collapse = ", "), "\n")
    if (length(only_here))
      cat("⚠ In this script but not in install_dependencies.R:",
          paste(only_here, collapse = ", "), "\n")
  }
} else {
  cat("⚠ install_dependencies.R not found (run from the project root).\n")
}

# Phantom check: a declared package that is not in the closure at all
phantoms <- setdiff(direct, allp)
if (length(phantoms)) {
  cat("⚠ PHANTOM packages (declared but absent from the dependency closure):",
      paste(phantoms, collapse = ", "), "\n")
} else {
  cat("✓ No phantom packages.\n")
}

# Highest R version required anywhere in the closure
pat <- "R [(]>= [0-9.]+[)]"
dep_field <- ap[intersect(allp, rownames(ap)), "Depends"]
dep_field[is.na(dep_field)] <- ""
matched <- regmatches(dep_field, regexpr(pat, dep_field))
if (length(matched)) {
  floors <- package_version(gsub("R [(]>= |[)]", "", unlist(matched)))
  cat(sprintf("✓ Highest R version required in closure: %s\n",
              as.character(max(floors))))
}

cat("\nDone. Paste the sections above into docs/user_guide.md section 2.\n")
