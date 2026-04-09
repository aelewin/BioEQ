# R/security_database.R
# Parameterized query wrapper for safe database interactions
#
# NOTE: The BioEQ application currently uses only in-memory data frames
# and does NOT connect to any database. These functions are provided as
# defense-in-depth utilities in case a database backend is added in the future.

#' Create a parameterized query string (safe from SQL injection)
#'
#' Returns a list with the query template and parameters, suitable for use
#' with DBI::dbGetQuery() or DBI::dbExecute() with parameter binding.
#'
#' @param template A SQL template with `?` placeholders (e.g., "SELECT * FROM t WHERE id = ?")
#' @param params A list of parameter values, one per placeholder
#' @return A list with `query` and `params`
safe_query <- function(template, params = list()) {
  if (!is.character(template) || length(template) != 1 || nchar(template) == 0) {
    stop("Query template must be a non-empty string.")
  }

  # Count placeholders
  placeholder_count <- nchar(gsub("[^?]", "", template))
  if (placeholder_count != length(params)) {
    stop(
      "Mismatch: query template has ", placeholder_count,
      " placeholder(s) but ", length(params), " parameter(s) were supplied."
    )
  }

  # Validate that no raw string concatenation patterns are present
  dangerous_patterns <- c(
    "['\"]; *DROP",
    "['\"]; *DELETE",
    "['\"]; *INSERT",
    "['\"]; *UPDATE",
    "UNION\\s+SELECT",
    "--\\s*$",
    "/\\*.*\\*/",
    "OR\\s+1\\s*=\\s*1",
    "AND\\s+1\\s*=\\s*1"
  )

  for (p in params) {
    if (is.character(p)) {
      for (pat in dangerous_patterns) {
        if (grepl(pat, p, ignore.case = TRUE)) {
          warning("Potentially dangerous SQL pattern detected in parameter: '", p, "'")
        }
      }
    }
  }

  list(query = template, params = params)
}


#' Sanitize a string for safe inclusion in SQL (escaping approach)
#'
#' This is a fallback for systems that don't support parameterized queries.
#' Always prefer `safe_query()` with parameter binding.
#'
#' @param value A character string to sanitize
#' @return The sanitized string with single quotes escaped
sanitize_sql_string <- function(value) {
  if (!is.character(value) || length(value) != 1) {
    stop("Value must be a single character string.")
  }
  # Escape single quotes by doubling them
  gsub("'", "''", value, fixed = TRUE)
}


#' Check if a string contains SQL injection patterns
#'
#' @param input_string A character string to check
#' @return A list with `safe` (logical) and `patterns_found` (character vector)
detect_sql_injection <- function(input_string) {
  if (!is.character(input_string) || length(input_string) != 1) {
    return(list(safe = FALSE, patterns_found = "Input is not a valid string."))
  }

  patterns <- list(
    "DROP statement"     = "DROP\\s+(TABLE|DATABASE|INDEX)",
    "DELETE statement"   = "DELETE\\s+FROM",
    "UNION SELECT"       = "UNION\\s+SELECT",
    "Comment injection"  = "(--|#|/\\*)",
    "Tautology"          = "OR\\s+1\\s*=\\s*1",
    "String termination" = "';",
    "Batch separator"    = ";\\s*(DROP|DELETE|INSERT|UPDATE|EXEC)"
  )

  found <- character(0)
  for (name in names(patterns)) {
    if (grepl(patterns[[name]], input_string, ignore.case = TRUE)) {
      found <- c(found, name)
    }
  }

  list(safe = length(found) == 0, patterns_found = found)
}
