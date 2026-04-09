# tests/test_security_functions.R
# TEST 1: Unit tests for all security utility functions
# Run with: testthat::test_file("tests/test_security_functions.R")

library(testthat)

# Source the security modules - handle both running from project root and tests/ dir
find_project_root <- function() {
  # Try common locations
  candidates <- c(
    ".",                    # Running from project root
    "..",                   # Running from tests/
    "../.."                 # Running from tests/testthat/
  )
  for (d in candidates) {
    if (file.exists(file.path(d, "R", "security_validation.R"))) return(normalizePath(d))
  }
  stop("Cannot find project root (looked for R/security_validation.R)")
}
.proj_root <- find_project_root()

source(file.path(.proj_root, "R", "security_validation.R"))
source(file.path(.proj_root, "R", "security_database.R"))
source(file.path(.proj_root, "R", "security_output.R"))

# ============================================================================
# Input Validation Tests (security_validation.R)
# ============================================================================

context("Input Validation — validate_file_upload")

test_that("validate_file_upload accepts valid CSV file", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b,c\n1,2,3", tmp)
  on.exit(unlink(tmp), add = TRUE)
  result <- validate_file_upload(tmp)
  expect_true(result$valid)
  expect_match(result$message, "passed")
})

test_that("validate_file_upload rejects non-existent file", {
  result <- validate_file_upload("/nonexistent/path.csv")
  expect_false(result$valid)
  expect_match(result$message, "does not exist")
})

test_that("validate_file_upload rejects NULL input", {
  result <- validate_file_upload(NULL)
  expect_false(result$valid)
})

test_that("validate_file_upload rejects disallowed extension", {
  tmp <- tempfile(fileext = ".exe")
  writeLines("bad", tmp)
  on.exit(unlink(tmp), add = TRUE)
  result <- validate_file_upload(tmp)
  expect_false(result$valid)
  expect_match(result$message, "Unsupported file type")
})

test_that("validate_file_upload rejects oversized files", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b,c\n1,2,3", tmp)
  on.exit(unlink(tmp), add = TRUE)
  # 0 MB limit makes any real file too large

  result <- validate_file_upload(tmp, max_size_mb = 0)
  expect_false(result$valid)
  expect_match(result$message, "too large")
})

test_that("validate_file_upload rejects CSV with ZIP magic bytes", {
  tmp <- tempfile(fileext = ".csv")
  con <- file(tmp, "wb")
  writeBin(as.raw(c(0x50, 0x4B, 0x03, 0x04, 0x00, 0x00)), con)
  close(con)
  on.exit(unlink(tmp), add = TRUE)
  result <- validate_file_upload(tmp)
  expect_false(result$valid)
  expect_match(result$message, "does not match CSV")
})

# ---- validate_text_input ----

context("Input Validation — validate_text_input")

test_that("valid text passes", {
  result <- validate_text_input("Hello, world!")
  expect_true(result$valid)
})

test_that("NULL is rejected", {
  result <- validate_text_input(NULL)
  expect_false(result$valid)
})

test_that("too-long text is rejected", {
  result <- validate_text_input(paste(rep("a", 1001), collapse = ""), max_length = 1000)
  expect_false(result$valid)
  expect_match(result$message, "maximum length")
})

test_that("script tag is rejected", {
  result <- validate_text_input("<script>alert('xss')</script>")
  expect_false(result$valid)
  expect_match(result$message, "dangerous HTML")
})

test_that("event handler attribute is rejected", {
  result <- validate_text_input('<img onerror="alert(1)">')
  expect_false(result$valid)
})

test_that("javascript: URI is rejected", {
  result <- validate_text_input("javascript:alert(1)")
  expect_false(result$valid)
})

test_that("iframe tag is rejected", {
  result <- validate_text_input('<iframe src="evil.com"></iframe>')
  expect_false(result$valid)
})

test_that("non-character input is rejected", {
  result <- validate_text_input(42)
  expect_false(result$valid)
})

test_that("HTML is allowed when allow_html = TRUE", {
  result <- validate_text_input("<b>bold</b>", allow_html = TRUE)
  expect_true(result$valid)
})

# ---- validate_numeric_input ----

context("Input Validation — validate_numeric_input")

test_that("valid number in range passes", {
  result <- validate_numeric_input(5, min_val = 0, max_val = 10)
  expect_true(result$valid)
})

test_that("NULL is rejected", {
  result <- validate_numeric_input(NULL)
  expect_false(result$valid)
})

test_that("NA is rejected by default", {
  result <- validate_numeric_input(NA)
  expect_false(result$valid)
})

test_that("NA is accepted when allow_na = TRUE", {
  result <- validate_numeric_input(NA, allow_na = TRUE)
  expect_true(result$valid)
})

test_that("out-of-range value is rejected", {
  result <- validate_numeric_input(100, min_val = 0, max_val = 10)
  expect_false(result$valid)
  expect_match(result$message, "outside range")
})

test_that("non-numeric is rejected", {
  result <- validate_numeric_input("abc")
  expect_false(result$valid)
})

# ---- validate_file_path ----

context("Input Validation — validate_file_path")

test_that("valid path within allowed dir passes", {
  allowed <- tempdir()
  test_path <- file.path(allowed, "subdir", "file.txt")
  result <- validate_file_path(test_path, allowed_dir = allowed)
  expect_true(result$valid)
})

test_that("directory traversal is rejected", {
  result <- validate_file_path("../../../etc/passwd")
  expect_false(result$valid)
  expect_match(result$message, "traversal")
})

test_that("NULL path is rejected", {
  result <- validate_file_path(NULL)
  expect_false(result$valid)
})

test_that("empty path is rejected", {
  result <- validate_file_path("")
  expect_false(result$valid)
})

test_that("path outside allowed dir is rejected", {
  result <- validate_file_path("/etc/passwd", allowed_dir = tempdir())
  expect_false(result$valid)
  expect_match(result$message, "outside")
})

# ---- validate_treatment_values ----

context("Input Validation — validate_treatment_values")

test_that("valid treatments pass", {
  result <- validate_treatment_values(c("R", "T", "Reference", "Test"))
  expect_true(result$valid)
})

test_that("lowercase variants pass", {
  result <- validate_treatment_values(c("r", "t", "reference", "test"))
  expect_true(result$valid)
})

test_that("invalid treatment is rejected", {
  result <- validate_treatment_values(c("R", "X", "T"))
  expect_false(result$valid)
  expect_equal(result$invalid_values, "X")
})

test_that("empty vector is rejected", {
  result <- validate_treatment_values(character(0))
  expect_false(result$valid)
})

# ============================================================================
# Database Security Tests (security_database.R)
# ============================================================================

context("Database Security — safe_query")

test_that("safe_query creates correct parameterized query", {
  q <- safe_query("SELECT * FROM subjects WHERE id = ?", list(42))
  expect_equal(q$query, "SELECT * FROM subjects WHERE id = ?")
  expect_equal(q$params, list(42))
})

test_that("safe_query rejects empty template", {
  expect_error(safe_query(""), "non-empty string")
})

test_that("safe_query rejects placeholder/param mismatch", {
  expect_error(safe_query("SELECT * FROM t WHERE a = ? AND b = ?", list(1)),
               "Mismatch")
})

test_that("safe_query warns on dangerous pattern in param", {
  expect_warning(
    safe_query("SELECT * FROM t WHERE name = ?", list("'; DROP TABLE users; --")),
    "dangerous"
  )
})

# ---- sanitize_sql_string ----

context("Database Security — sanitize_sql_string")

test_that("single quotes are doubled", {
  expect_equal(sanitize_sql_string("O'Reilly"), "O''Reilly")
})

test_that("normal string passes through unchanged", {
  expect_equal(sanitize_sql_string("hello"), "hello")
})

test_that("non-string input is rejected", {
  expect_error(sanitize_sql_string(42))
})

# ---- detect_sql_injection ----

context("Database Security — detect_sql_injection")

test_that("normal input is safe", {
  result <- detect_sql_injection("John Doe")
  expect_true(result$safe)
  expect_length(result$patterns_found, 0)
})

test_that("DROP TABLE is detected", {
  result <- detect_sql_injection("'; DROP TABLE users; --")
  expect_false(result$safe)
  expect_true("DROP statement" %in% result$patterns_found)
})

test_that("UNION SELECT is detected", {
  result <- detect_sql_injection("1 UNION SELECT * FROM passwords")
  expect_false(result$safe)
  expect_true("UNION SELECT" %in% result$patterns_found)
})

test_that("tautology (OR 1=1) is detected", {
  result <- detect_sql_injection("1 OR 1=1")
  expect_false(result$safe)
  expect_true("Tautology" %in% result$patterns_found)
})

test_that("comment injection is detected", {
  result <- detect_sql_injection("admin'--")
  expect_false(result$safe)
  expect_true("Comment injection" %in% result$patterns_found)
})

# ============================================================================
# Output Escaping Tests (security_output.R)
# ============================================================================

context("Output Escaping — escape_html")

test_that("HTML special characters are escaped", {
  expect_equal(escape_html("<script>"), "&lt;script&gt;")
  expect_equal(escape_html("a & b"), "a &amp; b")
  expect_equal(escape_html('say "hello"'), "say &quot;hello&quot;")
  expect_equal(escape_html("it's"), "it&#39;s")
})

test_that("NULL returns empty string", {
  expect_equal(escape_html(NULL), "")
})

test_that("non-character is coerced", {
  expect_equal(escape_html(42), "42")
})

test_that("vector input is escaped element-wise", {
  result <- escape_html(c("<a>", "<b>"))
  expect_equal(result, c("&lt;a&gt;", "&lt;b&gt;"))
})

# ---- detect_xss ----

context("Output Escaping — detect_xss")

test_that("normal text is safe", {
  result <- detect_xss("Hello, world!")
  expect_true(result$safe)
})

test_that("script tag is detected", {
  result <- detect_xss("<script>alert('XSS')</script>")
  expect_false(result$safe)
  expect_true("Script tag" %in% result$patterns_found)
})

test_that("event handler is detected", {
  result <- detect_xss('<img onerror="alert(1)">')
  expect_false(result$safe)
  expect_true("Event handler" %in% result$patterns_found)
})

test_that("javascript URI is detected", {
  result <- detect_xss("javascript:alert(1)")
  expect_false(result$safe)
})

test_that("iframe is detected", {
  result <- detect_xss('<iframe src="evil.com"></iframe>')
  expect_false(result$safe)
})

test_that("NULL is flagged", {
  result <- detect_xss(NULL)
  expect_false(result$safe)
})

# ---- safe_column_name / sanitize_column_names ----

context("Output Escaping — safe_column_name & sanitize_column_names")

test_that("safe_column_name escapes HTML in column names", {
  expect_equal(safe_column_name("<script>x</script>"), "&lt;script&gt;x&lt;/script&gt;")
})

test_that("safe_column_name trims whitespace", {
  expect_equal(safe_column_name("  Name  "), "Name")
})

test_that("sanitize_column_names works on data frames", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("<b>Col1</b>", "Col2")
  result <- sanitize_column_names(df)
  expect_equal(names(result)[1], "&lt;b&gt;Col1&lt;/b&gt;")
  expect_equal(names(result)[2], "Col2")
})

test_that("sanitize_column_names rejects non-data-frame", {
  expect_error(sanitize_column_names("not a df"))
})

# ---- safe_html_text ----

context("Output Escaping — safe_html_text")

test_that("safe_html_text wraps in span", {
  result <- safe_html_text("hello")
  expect_match(result, "<span>hello</span>")
})

test_that("safe_html_text escapes content", {
  result <- safe_html_text("<script>bad</script>")
  expect_match(result, "&lt;script&gt;")
  expect_false(grepl("<script>", result, fixed = TRUE))
})

test_that("safe_html_text supports CSS class", {
  result <- safe_html_text("text", class = "info")
  expect_match(result, 'class="info"')
})
