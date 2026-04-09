# BioEQ Security Test Suite

## Overview

This directory contains a comprehensive security test suite for the BioEQ Shiny application. The tests verify that all identified security vulnerabilities have been fixed and that the application handles malicious inputs safely.

## Prerequisites

- **R** >= 4.0.0
- **testthat** >= 3.0.0

Install testthat if needed:

```r
install.packages("testthat")
```

## Running Tests

### Run All Tests

From the project root directory:

```bash
Rscript -e 'library(testthat); test_dir("tests/")'
```

Or run all security test files individually:

```bash
cd /path/to/BioEQ
Rscript -e 'library(testthat); test_file("tests/test_security_functions.R")'
Rscript -e 'library(testthat); test_file("tests/test_vulnerability_proofs.R")'
Rscript -e 'library(testthat); test_file("tests/test_app_security_integration.R")'
Rscript -e 'library(testthat); test_file("tests/test_dependencies.R")'
Rscript -e 'library(testthat); test_file("tests/test_security_config.R")'
Rscript -e 'library(testthat); test_file("tests/test_performance.R")'
Rscript -e 'library(testthat); test_file("tests/test_functionality.R")'
```

### Run From R Console

```r
library(testthat)
test_dir("tests/")
```

## Test Files

| File | Description | Tests |
|------|-------------|-------|
| `test_security_functions.R` | Unit tests for all security utility functions (validation, database, output escaping) | 87 |
| `test_vulnerability_proofs.R` | Exploitation tests that attempt each original vulnerability and prove it fails | 51 |
| `test_app_security_integration.R` | End-to-end tests verifying secure data flow from input through processing to output | 21 |
| `test_dependencies.R` | Checks for vulnerable package versions and required dependencies | 18 |
| `test_security_config.R` | Verifies no hardcoded secrets, proper .gitignore, environment variable config | 18 |
| `test_performance.R` | Ensures security functions don't cause unacceptable performance regression | 7 |
| `test_functionality.R` | Functional regression tests verifying original app features still work | 46 |

## Security Modules Tested

| Module | Location | Purpose |
|--------|----------|---------|
| `security_validation.R` | `R/security_validation.R` | Input validation (file uploads, text, numbers, paths, treatment values) |
| `security_database.R` | `R/security_database.R` | Parameterized queries, SQL injection detection |
| `security_output.R` | `R/security_output.R` | HTML escaping, XSS detection, safe rendering utilities |

## CI/CD

Tests run automatically via GitHub Actions on every push and pull request. See `.github/workflows/security-tests.yml`.
