# BioEQ R Functions

## 📁 Directory Structure

This directory contains the core R functions for bioequivalence analysis.

### Core Modules

#### `bioeq_main.R`
Main entry point and workflow orchestration
- `run_bioeq_analysis()`: Complete analysis pipeline
- `launch_app()`: Start Shiny application (future feature)

**Note**: To launch the Shiny web interface, see [shiny/README.md](../shiny/README.md) for current instructions.

#### `nca_functions.R`
Non-compartmental analysis functions
- `calculate_pk_parameters()`: Calculate AUC, Cmax, Tmax
- `calculate_auc()`: Area under curve calculation
- `calculate_half_life()`: Terminal half-life estimation

#### `be_analysis.R`
Bioequivalence assessment functions
- `run_bioequivalence_analysis()`: Main BE analysis
- `calculate_confidence_intervals()`: 90% CI calculation
- `assess_bioequivalence()`: BE criteria evaluation

#### `simple_anova.R`
Statistical analysis using linear models
- `run_anova_analysis()`: ANOVA for crossover designs
- `extract_anova_results()`: Format ANOVA output

#### `statistics.R`
Statistical utility functions
- `geometric_mean()`: Calculate geometric means
- `calculate_cv()`: Coefficient of variation
- `calculate_mse()`: Mean squared error

#### `carryover_detection.R`
Carryover effect assessment
- `detect_carryover()`: ICH M13A compliant detection
- `exclude_carryover_subjects()`: Subject exclusion logic

#### `missing_data_handling.R`
Missing data management for NCA
- `handle_missing_nca_data()`: Imputation strategies
- `validate_missing_pattern()`: Check missing data patterns

#### `plotting.R`
Visualization functions
- `plot_concentration_time()`: Concentration-time profiles
- `plot_pk_comparison()`: Test vs Reference comparison
- `create_forest_plot()`: Forest plot for BE results

#### `utils.R`
Utility and validation functions
- `validate_be_data()`: Data validation
- `standardize_column_names()`: Column name mapping
- `format_results()`: Output formatting

## 🔧 Usage Examples

### Basic NCA Analysis
```r
# Load data
data <- read.csv("concentration_data.csv")

# Validate data
validation <- validate_be_data(data)
if (!validation$valid) {
  stop(validation$errors)
}

# Calculate PK parameters
pk_results <- calculate_pk_parameters(
  data = data,
  method = "linear_log",
  lambda_z_method = "automatic"
)

# View results
summary(pk_results)
```

### Bioequivalence Analysis
```r
# Run BE analysis on PK results
be_results <- run_bioequivalence_analysis(
  pk_data = pk_results,
  design = "2x2x2",
  alpha = 0.05,
  be_limits = c(0.80, 1.25)
)

# Check if bioequivalent
if (all(be_results$bioequivalent)) {
  print("Products are bioequivalent!")
}
```

### Complete Pipeline
```r
# One-step analysis
results <- run_bioeq_analysis(
  data = "path/to/data.csv",
  config = list(
    design = "2x2x2",
    auc_method = "linear_log",
    carryover_test = TRUE,
    report_format = "pdf"
  )
)
```

## 🧪 Testing

Each function has corresponding unit tests in `tests/testthat/`:

```r
# Run all tests
testthat::test_dir("../tests/testthat")

# Test specific module
testthat::test_file("../tests/testthat/test-nca_functions.R")
```

## 📝 Function Documentation

All functions use roxygen2 documentation format:

```r
#' Calculate PK Parameters
#' 
#' @param data Data frame with concentration-time data
#' @param method AUC calculation method ("linear", "log", "mixed")
#' @param lambda_z_method Terminal phase selection ("automatic", "manual")
#' @return List containing PK parameters for each subject
#' @export
#' @examples
#' pk_results <- calculate_pk_parameters(data, method = "linear_log")
```

## 🔄 Dependencies

Required packages:
- `dplyr`: Data manipulation
- `stats`: Statistical functions
- `ggplot2`: Plotting (optional)

## 📊 Output Formats

Functions return standardized list structures:

```r
# NCA results structure
list(
  subject_data = data.frame(...),
  summary_stats = list(...),
  parameters = c("AUC0t", "AUCinf", "Cmax", "Tmax"),
  method = "linear_log"
)

# BE results structure  
list(
  confidence_intervals = data.frame(...),
  point_estimates = data.frame(...),
  bioequivalent = logical(...),
  anova_results = list(...)
)
```

## 🚨 Error Handling

All functions include comprehensive error checking:

```r
# Input validation
if (!is.data.frame(data)) {
  stop("Input must be a data frame")
}

# Column checking
required_cols <- c("subject", "treatment", "time", "concentration")
missing <- setdiff(required_cols, names(data))
if (length(missing) > 0) {
  stop(paste("Missing columns:", paste(missing, collapse = ", ")))
}
```

---
*For more details, see the main [BioEQ documentation](../README.md)*