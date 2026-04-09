# tests/test_security_config.R
# TEST 5: Configuration and secrets management tests
# Run with: testthat::test_file("tests/test_security_config.R")

library(testthat)

# ============================================================================
# Hardcoded Secrets Check
# ============================================================================

find_project_root <- function() {
  candidates <- c(".", "..", "../..")
  for (d in candidates) {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(normalizePath(d))
  }
  stop("Cannot find project root")
}
.proj_root <- find_project_root()

context("Configuration — No Hardcoded Secrets")

test_that("no hardcoded passwords in R source files", {
  r_files <- list.files(.proj_root, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  # Exclude test files from this check
  r_files <- r_files[!grepl("tests/", r_files)]

  password_patterns <- c(
    "password\\s*=\\s*['\"]",
    "passwd\\s*=\\s*['\"]",
    "secret\\s*=\\s*['\"]",
    "api_key\\s*=\\s*['\"]",
    "apikey\\s*=\\s*['\"]",
    "token\\s*=\\s*['\"][A-Za-z0-9]"
  )

  secrets_found <- character(0)
  for (f in r_files) {
    content <- readLines(f, warn = FALSE)
    for (pat in password_patterns) {
      matches <- grepl(pat, content, ignore.case = TRUE)
      if (any(matches)) {
        matched_lines <- content[matches]
        real_secrets <- matched_lines[!grepl("^\\s*#", matched_lines)]
        real_secrets <- real_secrets[!grepl("Sys\\.getenv|getOption", real_secrets)]
        if (length(real_secrets) > 0) {
          secrets_found <- c(secrets_found, paste(f, ":", real_secrets))
        }
      }
    }
  }
  expect_equal(length(secrets_found), 0,
               info = paste("Secrets found:", paste(secrets_found, collapse = "; ")))
})

test_that("no hardcoded API keys in JavaScript or CSS files", {
  web_files <- list.files(file.path(.proj_root, "shiny", "www"),
                          pattern = "\\.(js|css)$", recursive = TRUE, full.names = TRUE)

  key_patterns <- c("api[_-]?key", "secret", "token", "password")

  for (f in web_files) {
    if (file.exists(f)) {
      content <- readLines(f, warn = FALSE)
      for (pat in key_patterns) {
        matches <- grepl(pat, content, ignore.case = TRUE)
        matched_lines <- content[matches]
        # Exclude comments
        real <- matched_lines[!grepl("^\\s*(//|/\\*|\\*)", matched_lines)]
        expect_equal(
          length(real), 0,
          info = paste("Potential secret in", f)
        )
      }
    }
  }
})

# ============================================================================
# .gitignore Configuration
# ============================================================================

context("Configuration — .gitignore")

test_that(".gitignore excludes .env files", {
  gitignore_path <- file.path(.proj_root, ".gitignore")
  expect_true(file.exists(gitignore_path))

  content <- readLines(gitignore_path, warn = FALSE)
  combined <- paste(content, collapse = "\n")

  expect_true(any(grepl("\\.env", content)),
              info = ".gitignore should exclude .env files")
})

test_that(".gitignore excludes .Renviron", {
  gitignore_path <- file.path(.proj_root, ".gitignore")
  if (file.exists(gitignore_path)) {
    content <- readLines(gitignore_path, warn = FALSE)
    expect_true(any(grepl("\\.Renviron", content)),
                info = ".gitignore should exclude .Renviron")
  } else {
    skip(".gitignore not found")
  }
})

test_that(".gitignore excludes data directory", {
  gitignore_path <- file.path(.proj_root, ".gitignore")
  if (file.exists(gitignore_path)) {
    content <- readLines(gitignore_path, warn = FALSE)
    expect_true(any(grepl("data/", content)),
                info = ".gitignore should exclude data directory")
  } else {
    skip(".gitignore not found")
  }
})

# ============================================================================
# Environment Variable Configuration
# ============================================================================

context("Configuration — Environment Variables")

test_that(".env.example file exists with documentation", {
  env_example <- file.path(.proj_root, ".env.example")
  expect_true(file.exists(env_example),
              info = ".env.example should exist to document required env vars")

  if (file.exists(env_example)) {
    content <- readLines(env_example, warn = FALSE)
    combined <- paste(content, collapse = "\n")

    expect_true(grepl("BIOEQ_HOST", combined),
                info = ".env.example should document BIOEQ_HOST")
    expect_true(grepl("BIOEQ_PORT", combined),
                info = ".env.example should document BIOEQ_PORT")
  }
})

test_that("launch_app.R uses environment variables with safe defaults", {
  launch_file <- file.path(.proj_root, "launch_app.R")
  if (file.exists(launch_file)) {
    content <- readLines(launch_file, warn = FALSE)
    combined <- paste(content, collapse = "\n")

    # Should use Sys.getenv for host configuration
    expect_true(grepl("Sys\\.getenv", combined),
                info = "launch_app.R should use Sys.getenv for configuration")
    # Should default to 127.0.0.1
    expect_true(grepl("127\\.0\\.0\\.1", combined),
                info = "launch_app.R should default to 127.0.0.1")
  } else {
    skip("launch_app.R not found")
  }
})
