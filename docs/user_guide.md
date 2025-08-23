# BioEQ User Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Web Application](#web-application)
5. [R Package Functions](#r-package-functions)
6. [Data Requirements](#data-requirements)
7. [Analysis Workflows](#analysis-workflows)
8. [Plotting and Visualization](#plotting-and-visualization)
9. [Study Planning](#study-planning)
10. [Examples](#examples)
11. [Troubleshooting](#troubleshooting)

## Introduction

BioEQ is a comprehensive bioequivalence and bioavailability analysis platform that provides both a modern web interface and powerful R functions. It includes proven algorithms modernized for R 4.4+ compatibility and enhanced with contemporary R practices.

### Key Features
- **Web Application**: Professional Shiny interface with drag-and-drop data upload
- **Non-compartmental analysis (NCA)** with comprehensive PK parameter calculation
- **Bioequivalence analysis** for 2x2x2 crossover, replicate, and parallel designs
- **Statistical functions** for sample size calculation and power analysis
- **Modern visualization** with interactive and static plotting options
- **Data validation** and quality control features
- **Report generation** in PDF, Word, and HTML formats
- **Regulatory compliance** following FDA, EMA, and ICH guidelines

### Two Ways to Use BioEQ

1. **Web Application** - User-friendly interface for point-and-click analysis
2. **R Package** - Programmatic access for advanced users and automation

### Supported Study Designs
- **2x2x2 crossover** (standard bioequivalence)
- **2x2x4 replicate crossover** (for highly variable drugs)
- **Parallel group design**
- **Multiple period designs**

### Regulatory Standards
- FDA Guidance for Industry: Statistical Approaches to Establishing Bioequivalence
- EMA Guideline on the Investigation of Bioequivalence
- ICH M13A: Bioequivalence for Immediate-Release Solid Oral Dosage Forms

## Installation

### Prerequisites
BioEQ requires R version 4.0 or higher and the following packages:

```r
# Required packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "readr", "readxl",
  "nlme", "ggplot2", "dplyr", "reshape2", 
  "plotrix", "ICSNP", "coin", "gdata", "png"
))

# Optional packages for enhanced features
install.packages(c(
  "shinyjs", "shinycssloaders", "plotly", 
  "rmarkdown", "knitr", "officer", "flextable"
))
```

### Starting BioEQ

#### Option 1: Web Application (Recommended)
```r
# Navigate to the BioEQ directory
setwd("path/to/BioEQ")

# Launch the Shiny web application
shiny::runApp("shiny/app.R")
```

#### Option 2: R Package Functions
```r
# Load the R functions
source("R/bioeq_main.R")
init_bioeq()

# View available functions
help_bioeq()
```

## Quick Start

### Web Application Quick Start

1. **Launch the Application**
```r
shiny::runApp("shiny/app.R")
```

2. **Upload Your Data**
   - Navigate to "Data Upload" tab
   - Drag and drop your CSV/Excel file
   - Or use the example data templates

3. **Configure Analysis**
   - Go to "Analysis Setup"
   - Select study design (2x2x2, parallel, replicate)
   - Choose PK parameters and analysis options

4. **View Results**
   - Click "Results" to see analysis output
   - Interactive tables and plots
   - Statistical summaries and conclusions

5. **Export Reports**
   - Generate PDF or Word reports
   - Download data and plots

### R Package Quick Start

1. **Initialize BioEQ**
```r
source("R/bioeq_main.R")
init_bioeq()
```

2. **Load Example Data**
```r
# Create a standard 2x2x2 crossover study
example_data <- load_example_data("standard_2x2x2")
print(example_data)
```

3. **Perform Analysis**
```r
# Extract PK parameters
pk_data <- example_data$pk_parameters

# Perform BE analysis
be_results <- perform_be_analysis(pk_data, design = "2x2x2")
print(be_results)
```

4. **Create Visualizations**
```r
# Concentration-time profiles
conc_plot <- plot_concentration_time(example_data$concentration_data, log_scale = TRUE)
print(conc_plot)
```

5. **Run Complete Demo**
```r
# Run comprehensive demo
run_bioeq_demo("all")
```

## Web Application

### Interface Overview

The BioEQ web application provides a professional, user-friendly interface for bioequivalence analysis without requiring R programming knowledge.

#### Main Navigation
- **Data Upload** - Import and validate your data
- **Analysis Setup** - Configure study parameters
- **Results** - View analysis results and plots  
- **Exports & Reports** - Generate and download reports
- **Advanced Options** - Fine-tune analysis settings
- **Validation** - Quality control and validation tools
- **Help & Support** - Documentation and support

### Data Upload Tab

**Features:**
- Drag-and-drop file upload
- Support for CSV, Excel (.xlsx), and tab-delimited files
- Real-time data validation and error checking
- Data preview with interactive tables
- Example data templates for download

**Supported File Formats:**
```
Concentration Data: Subject, Treatment, Period, Time, Concentration
PK Parameters: Subject, Treatment, Period, AUC0t, AUCinf, Cmax, Tmax
```

**Data Validation:**
- Column name checking
- Data type validation
- Missing value detection
- Outlier identification
- Study design verification

### Analysis Setup Tab

**Configuration Options:**
- **Study Design**: 2x2x2 crossover, parallel, replicate
- **PK Parameters**: AUC0t, AUCinf, Cmax selection
- **Statistical Settings**: Alpha level, BE limits
- **Regulatory Standard**: FDA, EMA, ICH guidelines
- **AUC Method**: Linear, log-linear, mixed methods
- **Carryover Testing**: Enable/disable carryover detection

**Advanced Options:**
- Missing data handling strategies
- Lambda-z estimation methods
- Reference scaling for highly variable drugs
- Custom bioequivalence limits

### Results Tab

**Analysis Output:**
- **Summary Statistics**: Descriptive statistics by treatment
- **ANOVA Results**: Statistical model output
- **Bioequivalence Assessment**: 90% confidence intervals
- **Conclusion**: Pass/fail determination with rationale

**Interactive Visualizations:**
- Concentration-time profiles (individual and mean)
- PK parameter comparisons
- Confidence interval plots
- Diagnostic plots (residuals, normality)

**Result Tables:**
- Sortable and searchable data tables
- Export individual tables to CSV/Excel
- Statistical summaries with proper formatting

### Exports & Reports Tab

**Report Generation:**
- **PDF Reports**: Comprehensive analysis reports
- **Word Documents**: Editable report templates
- **HTML Reports**: Interactive web-based reports

**Data Export:**
- Analysis results in CSV/Excel format
- High-resolution plots (PNG, PDF, SVG)
- R code for reproducibility
- Complete analysis package (ZIP)

**Report Contents:**
- Study summary and design
- Data characteristics and validation
- Statistical analysis methodology
- Results tables and figures
- Bioequivalence conclusions
- Regulatory compliance statement

## R Package Functions

### Main Analysis Functions

#### `perform_be_analysis()`
Main bioequivalence analysis function with auto-detection capabilities.

```r
be_results <- perform_be_analysis(
  data = pk_data,                    # PK parameters data frame
  design = "auto",                   # "2x2x2", "parallel", "replicate", "auto"
  alpha = 0.05,                      # Significance level
  be_limits = c(0.8, 1.25),         # Bioequivalence limits
  parameters = c("AUC0t", "Cmax"),   # Parameters to analyze
  regulatory_standard = "FDA"        # "FDA", "EMA", "ICH"
)
```

#### `analyze_be_study()`
Quick analysis wrapper for standard bioequivalence studies.

```r
results <- analyze_be_study(
  data = your_data,
  design = "2x2x2",
  alpha = 0.05,
  be_limits = c(0.8, 1.25)
)
```

### Non-Compartmental Analysis Functions

#### `calculate_nca()`
Comprehensive NCA analysis from concentration-time data.

```r
nca_results <- calculate_nca(
  data = conc_data,                  # Concentration-time data
  method = "linear_log",             # AUC calculation method
  lambda_z_method = "automatic"      # Terminal phase detection
)
```

#### `calculate_auc_linear()`
Calculate AUC using various trapezoidal methods.

```r
auc_value <- calculate_auc_linear(
  time = time_points,
  conc = concentrations,
  method = "mixed"                   # "linear", "log", "mixed", "linear_log"
)
```

#### `estimate_lambda_z()`
Terminal elimination rate constant estimation.

```r
lambda_z <- estimate_lambda_z(
  time = time_points,
  conc = concentrations,
  method = "automatic"               # "automatic", "manual", "adjusted_r2"
)
```

### Statistical Functions

#### `calculate_sample_size()`
Sample size calculation for bioequivalence studies.

```r
sample_size <- calculate_sample_size(
  cv = 0.25,                        # Coefficient of variation
  power = 0.8,                      # Desired power
  theta0 = 0.95,                    # Expected true ratio
  alpha = 0.05,                     # Significance level
  design = "2x2x2"                  # Study design
)
```

#### `calculate_power()`
Power analysis for existing study designs.

```r
power_result <- calculate_power(
  n_per_group = 24,                 # Sample size per group
  cv = 0.22,                        # Observed CV
  theta0 = 0.95,                    # True ratio
  alpha = 0.05
)
```

### Data Handling Functions

#### `validate_be_data()`
Comprehensive data validation for bioequivalence studies.

```r
validation <- validate_be_data(
  data = your_data,
  design = "2x2x2",
  required_columns = c("Subject", "Treatment", "AUC0t", "Cmax")
)
```

#### `format_be_data()`
Standardize data format for analysis.

```r
formatted_data <- format_be_data(
  data = raw_data,
  subject_col = "ID",
  treatment_col = "Formulation"
)
```

### Plotting Functions

#### `plot_concentration_time()`
Create concentration-time profile plots.

```r
ct_plot <- plot_concentration_time(
  data = conc_data,
  log_scale = TRUE,                 # Logarithmic y-axis
  interactive = FALSE,              # Static or interactive plot
  individual = TRUE,                # Show individual profiles
  mean_profiles = TRUE              # Show mean profiles
)
```

#### `plot_be_confidence_intervals()`
Forest plot of bioequivalence confidence intervals.

```r
ci_plot <- plot_be_confidence_intervals(
  be_results = analysis_results,
  be_limits = c(0.8, 1.25),
  parameters = c("AUC0t", "Cmax")
)
```

### Utility Functions

#### `load_example_data()`
Load built-in example datasets.

```r
# Available datasets
example_data <- load_example_data("standard_2x2x2")
high_cv_data <- load_example_data("high_cv")
parallel_data <- load_example_data("parallel")
```

#### `run_bioeq_demo()`
Run interactive demonstrations.

```r
# Available demo types
run_bioeq_demo("basic")            # Basic analysis workflow
run_bioeq_demo("advanced")         # Advanced scenarios
run_bioeq_demo("sample_size")      # Sample size calculations
run_bioeq_demo("all")              # Complete demonstration
```

## Data Requirements

### Concentration Data Format
Required columns for concentration-time data:
- `Subject`: Subject identifier (numeric or character)
- `Period`: Study period (1, 2, 3, 4...)
- `Formulation`: Treatment ("Test" or "Reference")
- `Time`: Time point (numeric, same units throughout)
- `Concentration`: Measured concentration (numeric, ≥ 0)

Example:
```r
conc_data <- data.frame(
  Subject = c(1, 1, 1, 2, 2, 2),
  Period = c(1, 1, 1, 2, 2, 2),
  Formulation = c("Test", "Test", "Test", "Reference", "Reference", "Reference"),
  Time = c(0, 2, 4, 0, 2, 4),
  Concentration = c(0, 100, 50, 0, 95, 48)
)
```

### PK Parameters Data Format
Required columns for bioequivalence analysis:
- `Subject`: Subject identifier
- `Period`: Study period
- `Formulation`: Treatment ("Test" or "Reference")
- `AUC0t`: AUC from time zero to last measurable concentration
- `AUC0inf`: AUC from time zero to infinity (optional)
- `Cmax`: Maximum observed concentration
- `Tmax`: Time to maximum concentration (optional)

## Analysis Workflows

### Standard 2x2x2 Crossover Analysis

1. **Data Preparation**
```r
# Validate your data
validated_data <- validate_pk_data(your_pk_data)

# Check for missing values and outliers
summary(validated_data)
```

2. **Bioequivalence Analysis**
```r
# Perform complete BE analysis
be_results <- perform_be_analysis(validated_data, design = "2x2x2")

# View results summary
summary(be_results)
```

3. **Results Interpretation**
```r
# Check confidence intervals
ci_results <- be_results$confidence_intervals

# Bioequivalence criteria (80-125% rule)
for (param in names(ci_results)) {
  ci <- ci_results[[param]]
  is_be <- ci$ci_lower >= 80 && ci$ci_upper <= 125
  cat(sprintf("%s: %s (%.1f%% - %.1f%%)\n", 
              param, 
              ifelse(is_be, "BIOEQUIVALENT", "NOT BIOEQUIVALENT"),
              ci$ci_lower, ci$ci_upper))
}
```

### Replicate Design Analysis

```r
# Generate replicate crossover data
replicate_data <- load_example_data("replicate_2x2x4")

# Analyze (when fully implemented)
# be_results <- perform_be_analysis(replicate_data$pk_parameters, design = "replicate")
```

### High Variability Drug Analysis

```r
# Load high CV example
high_cv_data <- load_example_data("high_cv")

# Perform analysis
be_results <- perform_be_analysis(high_cv_data$pk_parameters, design = "2x2x2")

# For high CV drugs, consider widened limits or replicate design
```

## Plotting and Visualization

BioEQ provides both static (ggplot2) and interactive (plotly) visualizations for comprehensive data exploration and presentation.

### Concentration-Time Profiles

#### Basic Concentration Plots
```r
# Simple concentration-time plot
ct_plot <- plot_concentration_time(
  data = conc_data,
  log_scale = FALSE,
  interactive = FALSE
)
print(ct_plot)

# Log-scale with enhanced features
ct_log <- plot_concentration_time(
  data = conc_data,
  log_scale = TRUE,
  individual = TRUE,              # Show individual subject profiles
  mean_profiles = TRUE,           # Overlay mean profiles
  error_bars = "se",              # Standard error bars
  interactive = TRUE              # Interactive plotly version
)
print(ct_log)
```

#### Individual Subject Profiles
```r
# Faceted individual profiles
ind_plot <- plot_individual_profiles(
  data = conc_data,
  facet_wrap = TRUE,              # Separate panel per subject
  treatments = c("Test", "Reference"),
  log_scale = TRUE,
  ncol = 4                        # Number of columns in facet
)
print(ind_plot)

# Overlaid individual profiles
overlay_plot <- plot_individual_profiles(
  data = conc_data,
  facet_wrap = FALSE,             # All on same panel
  alpha = 0.3,                    # Transparency for overlapping lines
  highlight_mean = TRUE
)
print(overlay_plot)
```

### Bioequivalence Visualization

#### Confidence Interval Plots
```r
# Forest plot of confidence intervals
ci_forest <- plot_be_confidence_intervals(
  be_results = analysis_results,
  be_limits = c(0.8, 1.25),
  parameters = c("AUC0t", "AUCinf", "Cmax"),
  style = "forest",               # Forest plot style
  show_individual = TRUE          # Show individual ratios
)
print(ci_forest)

# Horizontal bar plot
ci_horizontal <- plot_be_confidence_intervals(
  be_results = analysis_results,
  style = "horizontal",
  color_by_result = TRUE          # Color by pass/fail
)
print(ci_horizontal)
```

#### PK Parameter Comparisons
```r
# Box plots comparing treatments
pk_box <- plot_pk_comparison(
  data = pk_data,
  parameters = c("AUC0t", "Cmax"),
  plot_type = "boxplot",
  log_scale = TRUE,
  show_points = TRUE              # Overlay individual points
)
print(pk_box)

# Violin plots with summary statistics
pk_violin <- plot_pk_comparison(
  data = pk_data,
  plot_type = "violin",
  add_median = TRUE,
  add_quartiles = TRUE
)
print(pk_violin)

# Paired scatter plots
pk_scatter <- plot_pk_comparison(
  data = pk_data,
  plot_type = "paired_scatter",   # Test vs Reference scatter
  add_identity_line = TRUE,
  add_regression = TRUE
)
print(pk_scatter)
```

### Diagnostic Plots

#### Statistical Diagnostics
```r
# Comprehensive diagnostic plot set
diagnostics <- plot_be_diagnostics(
  be_results = analysis_results,
  include = c("residuals", "qq", "leverage", "influence")
)

# Individual diagnostic plots
residual_plot <- diagnostics$residuals
qq_plot <- diagnostics$qq_normal
leverage_plot <- diagnostics$leverage
```

#### Data Quality Plots
```r
# Missing data patterns
missing_plot <- plot_missing_data(
  data = conc_data,
  by_subject = TRUE,
  by_time = TRUE
)
print(missing_plot)

# Outlier detection
outlier_plot <- plot_outliers(
  data = pk_data,
  method = "iqr",                 # IQR or z-score method
  threshold = 3
)
print(outlier_plot)
```

### Study Planning Visualizations

#### Sample Size Curves
```r
# Sample size vs CV relationship
ss_curve <- plot_sample_size_curve(
  cv_range = seq(0.1, 0.5, 0.01),
  power = 0.8,
  theta0 = c(0.90, 0.95, 1.0),    # Multiple scenarios
  alpha = 0.05
)
print(ss_curve)

# Power vs sample size
power_curve <- plot_power_curve(
  n_range = seq(10, 50, 2),
  cv = c(0.2, 0.25, 0.3),         # Multiple CV scenarios
  theta0 = 0.95
)
print(power_curve)
```

### Report-Quality Plots

#### High-Resolution Export
```r
# Save plots for reports
ggsave(
  filename = "concentration_profiles.png",
  plot = ct_plot,
  width = 10, height = 6,
  dpi = 300,                      # High resolution
  units = "in"
)

# PDF for vector graphics
ggsave(
  filename = "be_confidence_intervals.pdf",
  plot = ci_forest,
  width = 8, height = 10
)
```

#### Custom Themes
```r
# Apply professional theme
library(ggplot2)

professional_plot <- ct_plot +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Concentration-Time Profiles",
    subtitle = "Test vs Reference Formulations",
    caption = "Data: Example 2x2x2 Crossover Study"
  )
```

### Interactive Features

#### Plotly Integration
```r
# Convert ggplot to interactive plotly
library(plotly)

interactive_ct <- ggplotly(
  ct_plot,
  tooltip = c("x", "y", "colour", "text")
)

# Add custom hover information
interactive_ct <- interactive_ct %>%
  layout(
    title = "Interactive Concentration-Time Profiles",
    hovermode = "closest"
  )

  )

# Display interactive plot
interactive_ct
```

## Study Planning

### Sample Size Calculation
```r
# Basic sample size calculation
ss_result <- calculate_sample_size(
  cv = 0.25,           # Coefficient of variation
  power = 0.8,         # Desired power (80%)
  theta0 = 0.95,       # Expected true ratio
  alpha = 0.05         # Significance level
)

print(ss_result)
cat("Required sample size:", ss_result$n_per_group, "per group
")

# Sample size for different scenarios
cv_values <- c(0.15, 0.20, 0.25, 0.30, 0.35)
sample_sizes <- sapply(cv_values, function(cv) {
  calculate_sample_size(cv = cv, power = 0.8, theta0 = 0.95)$n_per_group
})

ss_table <- data.frame(CV = cv_values, Sample_Size = sample_sizes)
print(ss_table)
```

### Power Analysis
```r
# Calculate power for existing study
power_result <- calculate_power(
  n_per_group = 24,    # Subjects per group
  cv = 0.22,           # Observed CV
  theta0 = 0.92,       # Assumed true ratio
  alpha = 0.05
)

cat("Study power:", round(power_result$power * 100, 1), "%
")

# Power for multiple scenarios
true_ratios <- seq(0.85, 1.15, 0.05)
power_values <- sapply(true_ratios, function(ratio) {
  calculate_power(n_per_group = 24, cv = 0.25, theta0 = ratio)$power
})

power_table <- data.frame(True_Ratio = true_ratios, Power = round(power_values, 3))
print(power_table)
```

## Examples

### Example 1: Complete 2x2x2 Analysis

```r
# Initialize BioEQ
source("R/bioeq_main.R")
init_bioeq()

# Load example dataset
cat("Loading example 2x2x2 crossover data...
")
example_data <- load_example_data("standard_2x2x2")

# Examine data structure
str(example_data$pk_parameters)
head(example_data$concentration_data)

# Perform bioequivalence analysis
cat("Performing bioequivalence analysis...
")
be_results <- perform_be_analysis(
  data = example_data$pk_parameters,
  design = "2x2x2",
  alpha = 0.05,
  be_limits = c(0.8, 1.25),
  parameters = c("AUC0t", "AUCinf", "Cmax")
)

# Display results
print(be_results)

# Create visualizations
cat("Creating plots...
")
conc_plot <- plot_concentration_time(
  data = example_data$concentration_data,
  log_scale = TRUE,
  individual = TRUE,
  mean_profiles = TRUE
)

ci_plot <- plot_be_confidence_intervals(
  be_results = be_results,
  be_limits = c(0.8, 1.25)
)

# Display plots
print(conc_plot)
print(ci_plot)

# Generate summary report
cat("Analysis Summary:
")
cat("================
")
for (param in names(be_results$confidence_intervals)) {
  ci <- be_results$confidence_intervals[[param]]
  is_be <- ci$ci_lower >= 80 && ci$ci_upper <= 125
  cat(sprintf("%s: %s (%.1f%% - %.1f%%)
", 
              param, 
              ifelse(is_be, "BIOEQUIVALENT", "NOT BIOEQUIVALENT"),
              ci$ci_lower, ci$ci_upper))
}
```

### Example 2: NCA from Concentration Data

```r
# Load concentration-time data
conc_data <- example_data$concentration_data

# Perform NCA analysis
nca_results <- calculate_nca(
  data = conc_data,
  method = "linear_log",
  lambda_z_method = "automatic"
)

# Review NCA parameters
print(nca_results$summary_stats)

# Calculate individual AUC values
subjects <- unique(conc_data$Subject)
auc_results <- data.frame()

for (subj in subjects[1:3]) {  # First 3 subjects as example
  subj_data <- subset(conc_data, Subject == subj)
  
  for (trt in c("Test", "Reference")) {
    trt_data <- subset(subj_data, Formulation == trt)
    if (nrow(trt_data) > 1) {
      auc <- calculate_auc_linear(trt_data$Time, trt_data$Concentration, method = "mixed")
      auc_results <- rbind(auc_results, data.frame(
        Subject = subj,
        Treatment = trt,
        AUC = auc
      ))
    }
  }
}

print(auc_results)
```

### Example 3: Sample Size Planning

```r
# Planning a bioequivalence study
cat("Sample Size Planning Example
")
cat("===========================
")

# Define study parameters
target_power <- 0.8
alpha_level <- 0.05
expected_ratio <- 0.95
cv_estimate <- 0.25

# Calculate required sample size
ss_result <- calculate_sample_size(
  cv = cv_estimate,
  power = target_power,
  theta0 = expected_ratio,
  alpha = alpha_level,
  design = "2x2x2"
)

cat("Study Design: 2x2x2 Crossover
")
cat("Expected T/R Ratio:", expected_ratio, "
")
cat("Estimated CV:", cv_estimate * 100, "%
")
cat("Target Power:", target_power * 100, "%
")
cat("Alpha Level:", alpha_level, "
")
cat("Required Sample Size:", ss_result$n_per_group, "per group
")
cat("Total Subjects:", ss_result$n_per_group * 2, "
")

# Sensitivity analysis
cat("
Sensitivity Analysis:
")
cv_scenarios <- c(0.20, 0.25, 0.30, 0.35)
for (cv in cv_scenarios) {
  n <- calculate_sample_size(cv = cv, power = 0.8, theta0 = 0.95)$n_per_group
  cat(sprintf("CV = %2.0f%%: n = %2d per group
", cv * 100, n))
}
```

### Example 4: High Variability Drug

```r
# Simulate high CV scenario
high_cv_data <- load_example_data("high_cv")

cat("High Variability Drug Analysis
")
cat("==============================
")

# Standard analysis
standard_results <- perform_be_analysis(
  data = high_cv_data$pk_parameters,
  design = "2x2x2",
  be_limits = c(0.8, 1.25)
)

cat("Standard BE limits (80-125%):
")
for (param in names(standard_results$confidence_intervals)) {
  ci <- standard_results$confidence_intervals[[param]]
  is_be <- ci$ci_lower >= 80 && ci$ci_upper <= 125
  cat(sprintf("%s: %s (%.1f%% - %.1f%%)
", 
              param, 
              ifelse(is_be, "PASS", "FAIL"),
              ci$ci_lower, ci$ci_upper))
}

# Note: For highly variable drugs, consider:
# 1. Replicate crossover design (2x2x4, 2x3x3)
# 2. Reference-scaled average bioequivalence
# 3. Widened BE limits for certain parameters
cat("
Consider replicate design for highly variable drugs
")
```

## Troubleshooting

### Common Issues and Solutions

#### Data Upload Problems

**Issue**: File upload fails or data not recognized
```
Solutions:
1. Ensure file format is CSV, Excel (.xlsx), or tab-delimited
2. Check that column names match expected format:
   - Subject, Treatment, Period, Time, Concentration (for conc data)
   - Subject, Treatment, Period, AUC0t, Cmax (for PK data)
3. Verify no special characters in column names
4. Check for empty rows or columns
5. Ensure numeric data is properly formatted (no text in numeric columns)
```

**Issue**: Column validation errors
```r
# Check your data structure
str(your_data)
head(your_data)

# Rename columns if needed
names(your_data)[names(your_data) == "ID"] <- "Subject"
names(your_data)[names(your_data) == "Formulation"] <- "Treatment"

# Validate data
validation_result <- validate_be_data(your_data, design = "2x2x2")
print(validation_result)
```

#### Analysis Errors

**Issue**: "Design detection failed" or "Invalid study design"
```r
# Manually specify design
be_results <- perform_be_analysis(
  data = your_pk_data,
  design = "2x2x2",  # Don't use "auto"
  alpha = 0.05
)

# Check data structure for design compatibility
table(your_pk_data$Subject, your_pk_data$Treatment)
```

**Issue**: "Insufficient data for analysis"
```r
# Check for missing values
summary(your_pk_data)

# Remove subjects with incomplete data
complete_subjects <- your_pk_data %>%
  group_by(Subject) %>%
  summarise(n_treatments = n_distinct(Treatment)) %>%
  filter(n_treatments >= 2) %>%
  pull(Subject)

filtered_data <- your_pk_data[your_pk_data$Subject %in% complete_subjects, ]
```

**Issue**: "Lambda-z estimation failed"
```r
# Use manual lambda-z estimation
lambda_z_result <- estimate_lambda_z(
  time = time_points,
  conc = concentrations,
  method = "manual",
  time_range = c(8, 24)  # Specify time range
)

# Or use adjusted R-squared method
lambda_z_result <- estimate_lambda_z(
  time = time_points,
  conc = concentrations,
  method = "adjusted_r2",
  min_points = 3
)
```

#### Plotting Issues

**Issue**: Plots not displaying correctly
```r
# Check if required packages are installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")

# Create simple plot to test
library(ggplot2)
test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
print(test_plot)
```

**Issue**: Interactive plots not working
```r
# Check plotly installation
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}

# Test interactive functionality
library(plotly)
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
ggplotly(p)
```

#### Performance Issues

**Issue**: Slow analysis or memory errors
```r
# For large datasets, sample data for testing
sample_data <- your_large_dataset[sample(nrow(your_large_dataset), 1000), ]

# Increase memory limit (Windows)
memory.limit(size = 8000)

# Use data.table for large datasets
library(data.table)
dt_data <- as.data.table(your_data)
```

### Web Application Troubleshooting

#### Browser Issues
- **Recommended browsers**: Chrome, Firefox, Safari, Edge
- **Clear browser cache** if interface appears broken
- **Disable ad blockers** that might interfere with JavaScript
- **Enable JavaScript** in browser settings

#### File Upload Issues
- **Maximum file size**: 100 MB (configurable)
- **Supported formats**: CSV, Excel (.xlsx), tab-delimited
- **Check file encoding**: UTF-8 recommended
- **Remove special characters** from file names

#### Analysis Timeout
- **Default timeout**: 300 seconds
- **For large datasets**: Consider subsetting data
- **Check server resources** if self-hosting

### Error Messages and Solutions

| Error Message | Cause | Solution |
|---------------|-------|----------|
| "Column 'Subject' not found" | Missing required column | Rename or add Subject column |
| "Design detection failed" | Ambiguous study design | Specify design explicitly |
| "Insufficient data points" | Too few observations | Check data completeness |
| "Lambda-z estimation failed" | Poor concentration profile | Use manual time range |
| "ANOVA model failed" | Statistical model issues | Check data distribution |
| "Memory allocation error" | Large dataset | Increase memory or subset data |
| "Package not found" | Missing dependencies | Install required packages |

### Getting Help

#### Built-in Help
```r
# Function documentation
help(perform_be_analysis)
?calculate_nca

# Package overview
help_bioeq()

# Run examples
run_bioeq_demo("all")
```

#### Diagnostic Information
```r
# System information
sessionInfo()

# BioEQ configuration
get_bioeq_config()

# Test installation
test_bioeq()
```

#### Support Resources
- **GitHub Issues**: Report bugs and request features
- **Documentation**: Comprehensive function reference
- **Examples**: Built-in demonstration datasets
- **Community**: User forums and discussions

### Best Practices

#### Data Preparation
1. **Validate data early** using validation functions
2. **Check for outliers** before analysis
3. **Use consistent units** throughout study
4. **Document data transformations** applied
5. **Keep backup copies** of original data

#### Analysis Workflow
1. **Start with exploration** using demo functions
2. **Validate assumptions** before formal analysis
3. **Check diagnostic plots** for model appropriateness
4. **Document analysis decisions** and rationale
5. **Save intermediate results** for large studies

#### Reporting
1. **Use reproducible workflows** with R scripts
2. **Include diagnostic information** in reports
3. **Provide context** for bioequivalence decisions
4. **Follow regulatory guidelines** for your jurisdiction
5. **Archive analysis code** and data for future reference

---

*BioEQ User Guide - Comprehensive Bioequivalence Analysis Platform*  
*Version 1.0.0 | Updated: December 2024*
```

## Study Planning

### Sample Size Calculation
```r
# Basic sample size calculation
ss_result <- calculate_sample_size(
  cv = 0.25,           # Coefficient of variation
  power = 0.8,         # Desired power (80%)
  theta0 = 0.95,       # Expected true ratio
  alpha = 0.05         # Significance level
)

print(ss_result)
cat("Required sample size:", ss_result$n_per_group, "per group\n")
```

### Power Analysis
```r
# Calculate power for existing study
power_result <- calculate_power(
  n_per_group = 24,    # Subjects per group
  cv = 0.22,           # Observed CV
  theta0 = 0.92,       # Assumed true ratio
  alpha = 0.05
)

cat("Study power:", round(power_result$power * 100, 1), "%\n")
```

### Planning Different Scenarios
```r
# Scenario analysis
scenarios <- data.frame(
  CV = c(0.15, 0.20, 0.25, 0.30, 0.35),
  SampleSize = numeric(5)
)

for (i in 1:nrow(scenarios)) {
  ss <- calculate_sample_size(cv = scenarios$CV[i], power = 0.8)
  scenarios$SampleSize[i] <- ss$n_per_group * 2  # Total sample size
}

print(scenarios)
```

## Examples

### Example 1: Complete Bioequivalence Study
```r
# Run complete example
source("examples/basic_be_analysis.R")
```

### Example 2: Sample Size Planning
```r
# Run sample size example
source("examples/sample_size_calculation.R")
```

### Example 3: Failed Bioequivalence
```r
# Analyze a failed BE study
failed_data <- load_example_data("failed_be")
be_results <- perform_be_analysis(failed_data$pk_parameters, design = "2x2x2")

# Results will show non-bioequivalence
ci_plot <- plot_be_confidence_intervals(be_results)
print(ci_plot)
```

### Example 4: Custom Data Analysis
```r
# Load your own data
# your_data <- read.csv("your_study_data.csv")

# Validate format
# validated_data <- validate_pk_data(your_data)

# Perform analysis
# results <- perform_be_analysis(validated_data, design = "2x2x2")
```

## Troubleshooting

### Common Issues

#### "Error in source: cannot open file"
- Ensure you're in the correct working directory
- Use `setwd()` to navigate to the BioEQ folder
- Use absolute paths if necessary

#### "Package not found"
- Install missing packages: `install.packages("package_name")`
- Check R version compatibility

#### "Invalid data format"
- Verify column names match requirements
- Check for missing or negative concentration values
- Ensure Subject IDs are consistent across periods

#### "ANOVA failed"
- Check for sufficient data (minimum subjects per sequence)
- Verify balanced design (equal subjects in each sequence)
- Look for extreme outliers in PK parameters

### Data Quality Checks
```r
# Validate your data before analysis
validation_result <- validate_pk_data(your_data)

# Check for issues
summary(your_data)
anyNA(your_data)

# Look for outliers
boxplot(log(AUC0t) ~ Formulation, data = your_data)
boxplot(log(Cmax) ~ Formulation, data = your_data)
```

### Getting Help
1. Check function documentation: `help(function_name)`
2. Run built-in tests: `test_bioeq()`
3. Use examples: `run_bioeq_demo("all")`
4. Review error messages carefully

### Performance Tips
- For large datasets, consider subsetting data for initial exploration
- Use appropriate time ranges for lambda_z estimation
- Validate data quality before complex analyses
- Save intermediate results for large studies

## Advanced Topics

### Custom Bioequivalence Limits
```r
# Narrow therapeutic index drugs (90-111%)
be_results <- perform_be_analysis(pk_data, design = "2x2x2", be_limits = c(0.9, 1.11))

# Highly variable drugs (75-133% with replicate design)
# be_results <- perform_be_analysis(pk_data, design = "replicate", be_limits = c(0.75, 1.33))
```

### Multiple Endpoints
```r
# Analyze multiple PK parameters
parameters <- c("AUC0t", "AUC0inf", "Cmax")
for (param in parameters) {
  if (param %in% names(be_results$confidence_intervals)) {
    ci <- be_results$confidence_intervals[[param]]
    cat(sprintf("%s: %.1f%% - %.1f%%\n", param, ci$ci_lower, ci$ci_upper))
  }
}
```

### Exporting Results
```r
# Extract key results for reporting
results_summary <- data.frame(
  Parameter = names(be_results$confidence_intervals),
  Point_Estimate = sapply(be_results$confidence_intervals, function(x) x$point_estimate),
  CI_Lower = sapply(be_results$confidence_intervals, function(x) x$ci_lower),
  CI_Upper = sapply(be_results$confidence_intervals, function(x) x$ci_upper)
)

# Save to CSV
write.csv(results_summary, "be_results.csv", row.names = FALSE)
```

---

*BioEQ User Guide - Modern Bioequivalence Analysis for R 4.4+*
