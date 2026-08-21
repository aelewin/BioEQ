# BioEQ - Utility Functions
# Modernized utility functions for bioequivalence analysis

#' Null Coalescing Operator
#'
#' @param x First value
#' @param y Default value if x is NULL or NA
#' @return x if not NULL/NA, otherwise y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

#' Print BioEQ Object
#'
#' @param x BioEQ analysis result
#' @export
print.bioeq <- function(x, ...) {
  cat("🧬 BioEQ Analysis Result\n")
  cat("========================\n\n")
  
  if (!is.null(x$metadata)) {
    cat("Study Design:", x$metadata$design, "\n")
    cat("Analysis Date:", format(x$metadata$analysis_date), "\n")
    cat("BE Limits:", paste(x$metadata$be_limits, collapse = " - "), "\n\n")
  }
  
  if (!is.null(x$summary)) {
    cat("Summary:\n")
    print(x$summary)
  }
  
  invisible(x)
}


