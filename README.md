# BioEQ: Bioequivalence Analysis Platform

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/License-GPL--3-green.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-brightgreen.svg)]()
[![Validation](https://img.shields.io/badge/Validation-FDA%20Compliant-success.svg)]()

## 🎯 Overview

BioEQ is a comprehensive R package and Shiny application for bioequivalence analysis, providing regulatory-compliant statistical analysis for pharmaceutical studies. BioEQ offers both programmatic R functions and an intuitive web interface.

## ✨ Features

- **Regulatory Compliance**: FDA, EMA, and ICH M13A guideline adherence
- **Comprehensive Analysis**: NCA, ANOVA, and bioequivalence assessment
- **Study Designs**: 2×2×2 crossover, parallel, replicate designs
- **Web Interface**: Professional Shiny application with drag-and-drop data upload
- **Report Generation**: Automated regulatory-compliant reports in multiple formats
- **Validation**: Cross-validated with industry-standard software (WinNonlin, SAS)

## 🚀 Quick Start

### Installation

```r
# Install from GitHub
devtools::install_github("yourusername/BioEQ")

# Load the package
library(BioEQ)
```

### Basic Usage

#### R Package
```r
# Load example data
data <- read.csv("your_data.csv")

# Run NCA analysis
nca_results <- calculate_pk_parameters(data)

# Perform bioequivalence analysis
be_results <- run_bioequivalence_analysis(nca_results)

# Generate report
generate_report(be_results, format = "pdf")
```

#### Shiny Application
```r
# Launch the web interface
BioEQ::launch_app()

# Or run directly
shiny::runApp("shiny/app.R")
```

## 📊 Analysis Capabilities

### Non-Compartmental Analysis (NCA)
- AUC (linear, log-linear, mixed methods)
- Cmax, Tmax determination
- Terminal half-life calculation
- Clearance and volume parameters

### Statistical Analysis
- ANOVA with fixed and random effects
- 90% confidence intervals
- Geometric mean ratios
- Intra-subject variability

### Bioequivalence Assessment
- 80-125% acceptance criteria
- Scaled average bioequivalence
- Reference-scaled approaches
- Outlier detection

## 📋 Data Requirements

### Concentration-Time Data
- Columns: Subject, Treatment, Period, Sequence, Time, Concentration
- Formats: CSV, Excel, tab-delimited

### PK Parameters Data
- Pre-calculated AUC, Cmax, Tmax values
- Direct bioequivalence assessment

## 🛠️ Development

### Prerequisites
- R (≥ 4.0.0)
- RStudio (recommended)
- Required packages: See [DESCRIPTION](DESCRIPTION)

### Testing
```r
# Run unit tests
devtools::test()

# Check package
devtools::check()
```

## 📚 Documentation

- [User Guide](docs/user_guide.md)
- [API Reference](https://yourusername.github.io/BioEQ/)
- [Shiny App Guide](shiny/README.md)
- [Validation Report](VALIDATION_REPORT.md)

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md).

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests
5. Submit a pull request

## 📄 License

This project is licensed under the GPL-3 License - see [LICENSE](LICENSE) for details.

## 🏆 Validation & Compliance

BioEQ has been validated against:
- FDA Guidance for Industry: Statistical Approaches to Establishing Bioequivalence
- EMA Guideline on the Investigation of Bioequivalence
- ICH M13A: Bioequivalence for Immediate-Release Solid Oral Dosage Forms

## 👥 Authors

- BioEQ Development Team

## 🙏 Acknowledgments

- R Consortium for statistical computing
- Regulatory agencies for guidance documents

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/yourusername/BioEQ/issues)
- **Discussions**: [GitHub Discussions](https://github.com/yourusername/BioEQ/discussions)
- **Email**: support@bioeq.org

## 📈 Citation

If you use BioEQ in your research, please cite:

```
BioEQ Development Team (2025). BioEQ: Bioequivalence Analysis Platform. 
R package version 1.0.0. https://github.com/yourusername/BioEQ
```

---
*Last updated: December 2024*