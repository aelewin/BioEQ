# BioEQ Shiny Web Application

## 🌐 Overview

Professional web interface for bioequivalence analysis, providing an intuitive GUI for the BioEQ R package functionality.

## 🚀 Quick Start

### ⚡ One-Command Launch (Recommended)

```bash
# From the BioEQ project root directory:
cd shiny && R -e "options(repos = c(CRAN = 'https://cran.rstudio.com/')); shiny::runApp(host='0.0.0.0', port=4000, launch.browser=TRUE)"
```

The app will automatically open in your browser at: **http://localhost:4000**

### Launch Application

```r
# Method 1: From command line (recommended)
# Navigate to the shiny directory first
cd /path/to/BioEQ/shiny
R -e "options(repos = c(CRAN = 'https://cran.rstudio.com/')); shiny::runApp(host='0.0.0.0', port=4000, launch.browser=TRUE)"

# Method 2: From R console (in shiny directory)
setwd("/path/to/BioEQ/shiny")
options(repos = c(CRAN = 'https://cran.rstudio.com/'))
shiny::runApp(host='0.0.0.0', port=4000, launch.browser=TRUE)

# Method 3: Direct execution with absolute path
shiny::runApp("/path/to/BioEQ/shiny", port=4000, host="0.0.0.0", launch.browser=TRUE)

# Method 4: Via package (when BioEQ package is installed - future feature)
# library(BioEQ)
# BioEQ::launch_app()
```

**Important Notes:**
- Ensure you're in the `shiny/` directory before running the app
- The `options(repos = ...)` setting ensures packages install from CRAN if needed
- The app requires several R packages (automatically loaded on startup)
- Some optional packages (officer, zip) may show warnings but don't affect core functionality

### Access Points
- Local: http://localhost:4000
- Network: http://[your-ip]:4000

## 📁 Application Structure

```
shiny/
├── app.R                    # Main application file
├── ui/                      # User interface components
│   ├── data_upload_ui.R    # Data upload interface
│   ├── analysis_setup_ui.R # Analysis configuration
│   ├── results_dashboard_ui.R # Results display
│   └── exports_reports_ui.R # Export functionality
├── server/                  # Server logic
│   ├── data_upload_server.R # Upload handling
│   ├── analysis_setup_server.R # Analysis configuration
│   └── results_dashboard_server.R # Results processing
├── utils/                   # Utility functions
│   ├── help_utils.R        # Help system
│   └── report_generation.R # Report creation
├── templates/              # Report templates
└── www/                    # Static assets
    └── custom.css          # Application styling
```

## ✨ Features

### Data Upload
- **Drag & Drop**: Modern file upload interface
- **Multiple Formats**: CSV, Excel, tab-delimited
- **Validation**: Comprehensive data checking
- **Templates**: Downloadable format templates
- **Preview**: Interactive data table with validation

### Analysis Setup
- **Study Designs**: 2×2×2, parallel, replicate
- **NCA Methods**: Linear, log, mixed AUC calculation
- **Carryover**: ICH M13A compliant detection
- **ANOVA**: Fixed and Mixed effects models
- **BE Analysis**: Average BE, Reference Scaled ABE, Average BE with Expanding Limits

### Results Dashboard
- **Interactive Tables**: Sortable, searchable results
- **Visualizations**: Concentration-time plots, confidence intervals
- **Statistics**: ANOVA tables, confidence intervals
- **Assessment**: Bioequivalence conclusions
- **Summary**: Executive summary with key findings

### Export & Reports
- **Data Export**: CSV, Excel formats
- **Reports**: PDF, Word, HTML
- **Plots**: High-resolution graphics
- **Code**: R script generation
- **Archive**: Complete analysis package

## ✅ Validation & Testing

The BioEQ application has been tested against established reference datasets from peer-reviewed publications:

### 2×2 Crossover Studies
**Reference**: Schütz, H., Labes, D., & Fuglsang, A. (2014). Reference Datasets for 2-Treatment, 2-Sequence, 2-Period Bioequivalence Studies. *The AAPS Journal*, 16(6), 1292-1297.  
**DOI**: [10.1208/s12248-014-9661-0](https://doi.org/10.1208/s12248-014-9661-0)

### Parallel Group Studies  
**Reference**: Fuglsang, A., Schütz, H., & Labes, D. (2015). Reference Datasets for Bioequivalence Trials in a Two-Group Parallel Design. *The AAPS Journal*, 17(2), 400-404.  
**DOI**: [10.1208/s12248-014-9704-6](https://doi.org/10.1208/s12248-014-9704-6)


## 🎨 User Interface

### Navigation Flow
1. **Data Upload** → Upload and validate mapping of data
2. **Analysis Setup** → Configure data analysis
3. **Run Analysis** → Execute calculations
4. **View Results** → Review outcomes
5. **Export** → Download results and reports

### Design Features
- **Progress**: Step-by-step indicators
- **Help**: Contextual tooltips and guides
- **Validation**: Real-time input checking
- **Data Detection**: Automatic detection of standard BE data

## 🔧 Configuration

### Application Settings
```r
# config/app_config.R
app_config <- list(
  max_file_size = 100,  # MB
  timeout = 300,        # seconds
  port = 4000,
  host = "0.0.0.0"
)
```

### Customization
- **Branding**: Update logo in `www/`
- **Styling**: Modify `www/custom.css`
- **Templates**: Edit files in `templates/`
- **Help Text**: Update `utils/help_utils.R`

## 📊 Data Requirements

### Input Format
```csv
Subject,Treatment,Period,Sequence,Time,Concentration
1,R,1,RT,0,0
1,R,1,RT,0.5,125.3
1,R,1,RT,1,245.8
...
```

### Required Columns
- **Subject**: Unique identifier
- **Treatment**: R (Reference) or T (Test)
- **Time**: Sampling time points
- **Concentration**: Drug concentration

### Optional Columns
- **Period**: Study period
- **Sequence**: Treatment sequence
- **Dose**: Administered dose

## 🧪 Testing

### Unit Tests
```r
# Run Shiny tests
shiny::testApp(".")
```

### Manual Testing
1. Upload sample data
2. Configure analysis
3. Run calculation
4. Verify results
5. Export reports

## 🐛 Troubleshooting

### Common Issues

**App Won't Start - "App dir must contain either app.R or server.R"**
```bash
# This error occurs when R is looking in the wrong directory
# Solution: Ensure you're in the shiny directory

# Check current directory
pwd  # Should show .../BioEQ/shiny
ls app.R  # Should show the app.R file exists

# If not in shiny directory, navigate there:
cd /path/to/BioEQ/shiny

# Alternative: Set working directory within R
R -e "setwd('shiny'); options(repos = c(CRAN = 'https://cran.rstudio.com/')); shiny::runApp(host='0.0.0.0', port=4000)"
```

**App Won't Start - "No such file or directory"**
```bash
# Ensure you're in the correct directory
cd /path/to/BioEQ/shiny
pwd  # Should show .../BioEQ/shiny
ls app.R  # Should show the app.R file exists

# Then run the app
R -e "shiny::runApp(host='0.0.0.0', port=4000, launch.browser=TRUE)"
```

**Port Already in Use**
```r
# Change port
shiny::runApp(port = 4001, host='0.0.0.0', launch.browser=TRUE)
```

**Missing Package Warnings**
- `officer/flextable not available` - Affects Word report generation only
- `zip package not available` - Affects archive creation only
- These warnings don't prevent core functionality

**File Upload Error**
- Check file format (CSV, Excel)
- Verify column names
- Ensure numeric data validity

**Analysis Fails**
- Review data validation messages
- Check for missing values
- Verify study design selection

## 🔐 Security

- **File Validation**: Type and size checking
- **Input Sanitization**: XSS prevention
- **Session Management**: Isolated user sessions
- **Data Privacy**: No permanent storage

## 📈 Performance

### Optimization Tips
- **Data Size**: < 10,000 rows recommended
- **Browser**: Chrome/Firefox for best performance
- **Memory**: Allocate sufficient R memory
```r
options(shiny.maxRequestSize = 100*1024^2)  # 100MB
```

## 🎯 Deployment

### Local Server
```bash
# Navigate to shiny directory first (IMPORTANT!)
cd /path/to/BioEQ/shiny

# Then launch the app
R -e "options(repos = c(CRAN = 'https://cran.rstudio.com/')); shiny::runApp(host='0.0.0.0', port=4000)"

# Alternative: Set directory within R command
R -e "setwd('shiny'); options(repos = c(CRAN = 'https://cran.rstudio.com/')); shiny::runApp(host='0.0.0.0', port=4000)"
```

### Shiny Server
```bash
# Copy to server directory
sudo cp -R shiny/ /srv/shiny-server/bioeq/
```

### Docker Container
```dockerfile
FROM rocker/shiny:latest
WORKDIR /srv/shiny-server
COPY shiny/ ./
EXPOSE 3838
```
EXPOSE 3838
```

## 📚 Resources

- [Shiny Documentation](https://shiny.rstudio.com/)
- [BioEQ Package](../R/README.md)
- [User Guide](../docs/user_guide.md)
- [Video Tutorials](https://bioeq.org/tutorials)

## 🤝 Support

For issues or questions:
- GitHub Issues: [Report bugs](https://github.com/yourusername/BioEQ/issues)
- Email: support@bioeq.org

---
Author: Amanda Lewin
*Version BETA 
