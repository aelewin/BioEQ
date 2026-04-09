# R/security_validation.R
# Input validation functions for the BioEQ Shiny application
# Provides defense-in-depth input sanitization utilities

#' Validate a file upload by extension and size
#'
#' @param filepath Path to the uploaded file
#' @param allowed_extensions Character vector of allowed file extensions (lowercase)
#' @param max_size_mb Maximum file size in megabytes
#' @return A list with `valid` (logical) and `message` (character)
validate_file_upload <- function(filepath,
                                 allowed_extensions = c("csv", "xlsx", "xls"),
                                 max_size_mb = 50) {
  if (is.null(filepath) || !file.exists(filepath)) {
    return(list(valid = FALSE, message = "File does not exist."))
  }

  # Check file extension

  ext <- tolower(tools::file_ext(filepath))
  if (!ext %in% allowed_extensions) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Unsupported file type: '.", ext,
        "'. Allowed types: ",
        paste0(".", allowed_extensions, collapse = ", "), "."
      )
    ))
  }

  # Check file size
  size_mb <- file.size(filepath) / (1024^2)
  if (size_mb > max_size_mb) {
    return(list(
      valid = FALSE,
      message = paste0(
        "File too large (", round(size_mb, 2), " MB). ",
        "Maximum allowed size is ", max_size_mb, " MB."
      )
    ))
  }

  # Content sniff: first few bytes to verify it looks like text/zip
  con <- file(filepath, "rb")
  on.exit(close(con), add = TRUE)
  header_bytes <- readBin(con, "raw", n = 4)

  if (ext == "csv") {
    # CSV files should be readable as text (not binary/zip headers)
    zip_magic <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
    if (length(header_bytes) >= 4 && identical(header_bytes[1:4], zip_magic)) {
      return(list(valid = FALSE, message = "File content does not match CSV format."))
    }
  } else if (ext %in% c("xlsx", "xls")) {
    # XLSX files are ZIP archives; XLS files start with D0 CF 11 E0
    xlsx_magic <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
    xls_magic <- as.raw(c(0xD0, 0xCF, 0x11, 0xE0))
    if (length(header_bytes) >= 4 &&
        !identical(header_bytes[1:4], xlsx_magic) &&
        !identical(header_bytes[1:4], xls_magic)) {
      return(list(valid = FALSE, message = "File content does not match Excel format."))
    }
  }

  list(valid = TRUE, message = "File validation passed.")
}


#' Validate that a string is safe for display (no script injection)
#'
#' @param input_string The string to validate
#' @param max_length Maximum allowed length (default 1000)
#' @param allow_html Logical; if FALSE (default), reject strings containing HTML tags
#' @return A list with `valid` (logical) and `sanitized` (character)
validate_text_input <- function(input_string, max_length = 1000, allow_html = FALSE) {
  if (is.null(input_string) || !is.character(input_string)) {
    return(list(valid = FALSE, sanitized = "", message = "Input must be a character string."))
  }

  if (nchar(input_string) > max_length) {
    return(list(
      valid = FALSE,
      sanitized = "",
      message = paste0("Input exceeds maximum length of ", max_length, " characters.")
    ))
  }

  if (!allow_html) {
    # Check for HTML/script tags
    html_pattern <- "<\\s*script|<\\s*iframe|<\\s*object|<\\s*embed|<\\s*form|<\\s*img[^>]+onerror|javascript:|on\\w+\\s*="
    if (grepl(html_pattern, input_string, ignore.case = TRUE)) {
      return(list(
        valid = FALSE,
        sanitized = "",
        message = "Input contains potentially dangerous HTML content."
      ))
    }
  }

  list(valid = TRUE, sanitized = input_string, message = "Input is valid.")
}


#' Validate a numeric input value within an expected range
#'
#' @param value The value to validate
#' @param min_val Minimum allowed value (inclusive)
#' @param max_val Maximum allowed value (inclusive)
#' @param allow_na Whether NA values are acceptable
#' @return A list with `valid` (logical) and `message` (character)
validate_numeric_input <- function(value, min_val = -Inf, max_val = Inf, allow_na = FALSE) {
  if (is.null(value)) {
    return(list(valid = FALSE, message = "Value is NULL."))
  }

  if (is.na(value)) {
    if (allow_na) {
      return(list(valid = TRUE, message = "Value is NA (allowed)."))
    }
    return(list(valid = FALSE, message = "Value is NA."))
  }

  if (!is.numeric(value)) {
    return(list(valid = FALSE, message = "Value is not numeric."))
  }

  if (value < min_val || value > max_val) {
    return(list(
      valid = FALSE,
      message = paste0("Value ", value, " is outside range [", min_val, ", ", max_val, "].")
    ))
  }

  list(valid = TRUE, message = "Numeric value is valid.")
}


#' Validate a file path to prevent directory traversal attacks
#'
#' @param path The file path to validate
#' @param allowed_dir The base directory that the path must be within
#' @return A list with `valid` (logical), `normalized` (character), and `message` (character)
validate_file_path <- function(path, allowed_dir = tempdir()) {
 if (is.null(path) || !is.character(path) || nchar(path) == 0) {
    return(list(valid = FALSE, normalized = "", message = "Path is empty or NULL."))
  }

  # Reject obvious traversal patterns
  if (grepl("\\.\\.", path)) {
    return(list(valid = FALSE, normalized = "", message = "Path contains directory traversal sequence."))
  }

  # Normalize both paths
  norm_path <- tryCatch(
    normalizePath(path, mustWork = FALSE),
    error = function(e) ""
  )
  norm_allowed <- normalizePath(allowed_dir, mustWork = FALSE)

  if (nchar(norm_path) == 0) {
    return(list(valid = FALSE, normalized = "", message = "Path could not be normalized."))
  }

  # Check containment
  if (!startsWith(norm_path, norm_allowed)) {
    return(list(
      valid = FALSE,
      normalized = "",
      message = "Path is outside the allowed directory."
    ))
  }

  list(valid = TRUE, normalized = norm_path, message = "Path is valid.")
}


#' Validate Treatment column values
#'
#' @param values Character vector of Treatment values
#' @param allowed Character vector of allowed Treatment values
#' @return A list with `valid` (logical), `invalid_values` (character vector), and `message` (character)
validate_treatment_values <- function(values,
                                       allowed = c("R", "T", "Reference", "Test",
                                                    "r", "t", "reference", "test")) {
  if (is.null(values) || length(values) == 0) {
    return(list(valid = FALSE, invalid_values = character(0), message = "No treatment values provided."))
  }

  bad <- unique(values[!values %in% allowed])
  if (length(bad) > 0) {
    return(list(
      valid = FALSE,
      invalid_values = bad,
      message = paste0("Invalid treatment values: ", paste(bad, collapse = ", "))
    ))
  }

  list(valid = TRUE, invalid_values = character(0), message = "Treatment values are valid.")
}
