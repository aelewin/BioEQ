# R/security_output.R
# Safe HTML escaping and output utilities for the BioEQ Shiny application

#' Escape HTML special characters in a string
#'
#' Converts &, <, >, ", and ' to their HTML entity equivalents.
#' This prevents XSS when rendering user-supplied content in the browser.
#'
#' @param text A character string (or vector) to escape
#' @return The escaped string(s)
escape_html <- function(text) {
  if (is.null(text)) return("")
  if (!is.character(text)) text <- as.character(text)

  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}


#' Check if a string contains potential XSS payloads
#'
#' @param text A character string to check
#' @return A list with `safe` (logical) and `patterns_found` (character vector)
detect_xss <- function(text) {
  if (is.null(text) || !is.character(text)) {
    return(list(safe = FALSE, patterns_found = "Input is not a valid string."))
  }

  patterns <- list(
    "Script tag"       = "<\\s*script",
    "Event handler"    = "\\bon\\w+\\s*=",
    "JavaScript URI"   = "javascript\\s*:",
    "Data URI"         = "data\\s*:.*base64",
    "Iframe tag"       = "<\\s*iframe",
    "Object tag"       = "<\\s*object",
    "Embed tag"        = "<\\s*embed",
    "SVG onload"       = "<\\s*svg[^>]+onload",
    "IMG onerror"      = "<\\s*img[^>]+onerror"
  )

  found <- character(0)
  for (name in names(patterns)) {
    if (grepl(patterns[[name]], text, ignore.case = TRUE)) {
      found <- c(found, name)
    }
  }

  list(safe = length(found) == 0, patterns_found = found)
}


#' Safely render a column name from uploaded data
#'
#' Column names from user-uploaded CSV/Excel files should be escaped
#' before being rendered in HTML contexts.
#'
#' @param col_name A character string representing a column name
#' @return The HTML-escaped column name
safe_column_name <- function(col_name) {
  if (is.null(col_name) || !is.character(col_name)) return("")
  escape_html(trimws(col_name))
}


#' Sanitize a data frame's column names for safe display
#'
#' @param df A data frame whose column names may need escaping
#' @return The same data frame with HTML-escaped column names
sanitize_column_names <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  names(df) <- vapply(names(df), safe_column_name, character(1))
  df
}


#' Create a safe HTML snippet for displaying user-supplied text
#'
#' Wraps escaped text in a <span> element.
#'
#' @param text User-supplied text
#' @param class Optional CSS class for the span
#' @return An HTML character string safe for rendering
safe_html_text <- function(text, class = NULL) {
  escaped <- escape_html(text)
  if (!is.null(class)) {
    paste0('<span class="', escape_html(class), '">', escaped, '</span>')
  } else {
    paste0("<span>", escaped, "</span>")
  }
}
