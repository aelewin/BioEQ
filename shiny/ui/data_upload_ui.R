# Enhanced Data Upload UI with Visual Improvements
# This file contains the complete data upload interface with modern styling and reorganized flow

# Return a tagList containing the UI elements
tagList(
  # Custom CSS for step progression and enhanced styling
  tags$head(
    tags$style(HTML("
      /* Step progression styling */
      .step-number {
        background: #3498db;
        color: white;
        border-radius: 50%;
        width: 25px;
        height: 25px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-right: 10px;
        font-weight: bold;
        font-size: 14px;
        transition: all 0.3s ease;
      }
      
      .step-number.completed {
        background: #27ae60;
        transform: scale(1.1);
      }
      
      .step-number.current {
        background: #f39c12;
        box-shadow: 0 0 0 3px rgba(243, 156, 18, 0.3);
      }
      
      /* Section box enhancements */
      .box.box-primary .box-header {
        border-bottom: 2px solid #3498db;
      }
      
      .box.box-success .box-header {
        border-bottom: 2px solid #27ae60;
      }
      
      .box.box-warning .box-header {
        border-bottom: 2px solid #f39c12;
      }
      
      .box.box-info .box-header {
        border-bottom: 2px solid #17a2b8;
      }
      
      /* Hover effects for better interactivity */
      .box:hover {
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        transform: translateY(-2px);
        transition: all 0.3s ease;
      }
      
      /* Data type selection styling */
      .data-type-selection {
        background: linear-gradient(135deg, #f8f9fa 0%, #e3f2fd 100%);
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        border: 2px solid #e9ecef;
        transition: all 0.3s ease;
      }
      
      .data-type-selection:hover {
        border-color: #3498db;
        box-shadow: 0 2px 10px rgba(52, 152, 219, 0.1);
      }
      
      /* Upload zone enhancements */
      #upload-zone {
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      #upload-zone.dragover {
        border-color: var(--success) !important;
        background: linear-gradient(135deg, #f0fff4 0%, #e6fffa 100%) !important;
        transform: scale(1.02) !important;
      }
    ")),
    
    # Custom JavaScript for enhanced file upload experience
    tags$script(HTML("
$(document).ready(function() {
  // Enhanced drag and drop functionality
  var uploadZone = $('#upload-zone');
  var fileInput = $('#data_file');
  
  // Prevent default drag behaviors
  $(document).on('dragenter dragover drop', function(e) {
    e.preventDefault();
    e.stopPropagation();
  });
  
  // Highlight upload zone on drag enter
  uploadZone.on('dragenter dragover', function(e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).css({
      'border-color': 'var(--success)',
      'background': 'linear-gradient(135deg, #f0fff4 0%, #e6fffa 100%)',
      'transform': 'scale(1.02)'
    });
  });
  
  // Remove highlight on drag leave
  uploadZone.on('dragleave', function(e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).css({
      'border-color': 'var(--blue-accent)',
      'background': 'linear-gradient(135deg, var(--neutral-100) 0%, #e0f2fe 100%)',
      'transform': 'scale(1.0)'
    });
  });
  
  // Handle file drop
  uploadZone.on('drop', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    $(this).css({
      'border-color': 'var(--blue-accent)',
      'background': 'linear-gradient(135deg, var(--neutral-100) 0%, #e0f2fe 100%)',
      'transform': 'scale(1.0)'
    });
    
    var files = e.originalEvent.dataTransfer.files;
    if (files.length > 0) {
      // Trigger the file input change event
      fileInput[0].files = files;
      fileInput.trigger('change');
      
      // Show upload feedback
      Shiny.notifications.show({
        html: '<i class=\"fa fa-upload\"></i> File uploaded: ' + files[0].name,
        type: 'message',
        duration: 3000
      });
    }
  });
  
  // File input change event for visual feedback
  fileInput.on('change', function() {
    if (this.files && this.files.length > 0) {
      var fileName = this.files[0].name;
      var fileSize = (this.files[0].size / 1024 / 1024).toFixed(2);
      
      uploadZone.find('h4').html('<i class=\"fa fa-check-circle\" style=\"color: #27ae60;\"></i> File Selected: ' + fileName);
      uploadZone.find('p:first').html('File size: ' + fileSize + ' MB');
      
      // Add success styling
      uploadZone.css({
        'border-color': 'var(--success)',
        'background': 'linear-gradient(135deg, #f0fff4 0%, #e6fffa 100%)'
      });
    }
  });
  
  // Add smooth transitions to buttons
  $('.btn').hover(
    function() {
      $(this).css('transform', 'translateY(-2px)');
    },
    function() {
      $(this).css('transform', 'translateY(0px)');
    }
  );
  
  // Add loading spinner to continue button
  $('#go_to_setup').on('click', function() {
    var btn = $(this);
    var originalText = btn.html();
    btn.html('<i class=\"fa fa-spinner fa-spin\"></i> Loading Analysis Setup...');
    btn.prop('disabled', true);
    
    // Re-enable after navigation (in case it fails)
    setTimeout(function() {
      btn.html(originalText);
      btn.prop('disabled', false);
    }, 3000);
  });
});
    "))
  ),

  # ---- Header --------------------------------------------------------
  div(
    class = "upload-header",
    style = "padding: 12px 18px; margin-bottom: 14px; background: linear-gradient(135deg, #1e3a5f 0%, #2c5282 100%); border-radius: 8px; color: white;",
    h3(icon("upload"), " Data Upload",
       style = "margin: 0; font-weight: 700;"),
    p("Upload concentration-time data or pre-calculated PK parameters to begin your bioequivalence analysis.",
      style = "margin: 4px 0 0 0; font-size: 13px; color: #e2e8f0;")
  ),

fluidRow(
  # Main upload area - now full width
  column(
    width = 12,
    # Welcome message with enhanced styling
    box(
      title = NULL,
      status = "primary",
      solidHeader = FALSE,
      width = 12,
      div(style = "text-align: center; padding: 20px;",
        p("Choose your data type and upload your bioequivalence study data", 
          style = "font-size: 16px; color: #7f8c8d; margin-bottom: 20px;"),
        
        # Data type selection
        radioButtons(
          "data_type",
          label = NULL,
          choices = list(
            "Concentration-Time Data" = "concentration",
            "Pre-calculated PK Parameters" = "pk_parameters"
          ),
          selected = "concentration",
          inline = FALSE
        ),
        
        # Enhanced file upload area with conditional descriptions
        div(
          style = "border: 3px dashed #3498db; border-radius: 15px; padding: 40px; 
                   background: linear-gradient(135deg, #f8f9fa 0%, #e3f2fd 100%); 
                   margin-bottom: 25px; transition: all 0.3s ease;
                   box-shadow: 0 4px 15px rgba(52, 152, 219, 0.1);",
          id = "upload-zone",
          div(class = "tooltip-wrapper",
            fileInput(
              "data_file", 
              label = NULL,
              accept = c(".csv", ".xlsx", ".xls"),
              placeholder = "No file selected",
              buttonLabel = "Browse Files",
              multiple = FALSE,
              width = "100%"
            ),
            # Conditional tooltip based on data type
            conditionalPanel(
              condition = "input.data_type == 'concentration'",
              span(class = "tooltip-text", 
                   "Upload concentration-time data. Required columns: Subject, Treatment, Time, Concentration")
            ),
            conditionalPanel(
              condition = "input.data_type == 'pk_parameters'",
              span(class = "tooltip-text", 
                   "Upload pre-calculated PK parameters. Required columns: Subject, Treatment, and PK parameters (AUC0t, AUC0inf, Cmax, etc.)")
            )
          ),
          div(style = "margin-top: 20px;",
            icon("cloud-upload-alt", class = "fa-4x", 
                 style = "color: #3498db; margin-bottom: 20px; opacity: 0.8;"),
            h4("Drag & Drop or Click to Upload", 
               style = "color: #3498db; margin: 15px 0; font-weight: 500;"),
            p("Supported formats: CSV, Excel (.xlsx, .xls)", 
              style = "color: #7f8c8d; margin: 8px 0; font-size: 15px;"),
            p("Maximum file size: 50MB", 
              style = "color: #95a5a6; font-size: 13px; margin: 5px 0;")
          )
        ),
        
        # Reset button and upload status
        conditionalPanel(
          condition = "output.upload_status",
          div(style = "text-align: center; margin-top: 15px;",
            actionButton(
              "reset_upload",
              "Reset & Upload New File",
              icon = icon("refresh"),
              class = "btn btn-outline-secondary",
              style = "padding: 8px 20px; font-weight: 500; border-radius: 6px;"
            )
          )
        )
      )
    ),
    
    # SECTION 2: Data Preview & Summary (moved up to appear immediately after upload)
    conditionalPanel(
      condition = "output.data_preview_available",
      box(
        title = "Data Preview & Summary", 
        status = "success", 
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        div(style = "padding: 15px;",
          # Data table with enhanced styling
          div(style = "margin-bottom: 20px;",
            DT::dataTableOutput("data_preview")
          )
        )
      )
    ),

    # SECTION 3A: PK Parameter Identification (only for PK data - moved up before unit specs)
    conditionalPanel(
      condition = "output.upload_status && input.data_type == 'pk_parameters'",
      box(
        title = "PK Parameter Identification", 
        status = "warning", 
        solidHeader = TRUE,
        width = 12,
        div(style = "padding: 15px;",
          uiOutput("pk_parameter_selector")
        )
      )
    ),
    
    # SECTION 3B: Column Mapping & Units (only for concentration data)
    conditionalPanel(
      condition = "output.upload_status && input.data_type == 'concentration'",
      box(
        title = "Column Mapping & Units", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        div(style = "padding: 15px;",
          # Column mapping interface
          uiOutput("concentration_column_mapper"),
          
          br(),
          div(style = "text-align: center;",
            actionButton(
              "confirm_concentration_mapping",
              "Confirm Column Mapping & Units",
              class = "btn btn-primary",
              icon = icon("check"),
              style = "padding: 10px 20px; font-weight: 500;"
            )
          )
        )
      )
    ),
    
    # SECTION 4: Data Validation Results (final section for both data types)
    conditionalPanel(
      condition = "output.step4_ready",
      box(
        title = "Data Summary and Verification", 
        status = "info", 
        solidHeader = TRUE,
        width = 12,
        div(style = "padding: 10px;",
          p("Summary and verification of your uploaded data:", 
            style = "margin-bottom: 15px; color: #2d3748;"),
          
          # Two-column layout
          fluidRow(
            # Left column - Column Mappings and Status
            column(6,
              div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #28a745; margin-bottom: 20px;",
                h4(icon("link", style = "color: #28a745;"), " Column Mappings", 
                   style = "color: #28a745; margin-top: 0; margin-bottom: 15px;"),
                verbatimTextOutput("column_mappings_display", placeholder = FALSE)
              )
            ),
            
            # Right column - Data Summary
            column(6,
              div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #3498db; margin-bottom: 20px;",
                h4(icon("chart-bar", style = "color: #3498db;"), " Data Summary", 
                   style = "color: #3498db; margin-top: 0; margin-bottom: 15px;"),
                verbatimTextOutput("data_summary", placeholder = FALSE)
              )
            )
          ),
          br(),
          
          conditionalPanel(
            condition = "output.data_preview_available",
            div(style = "text-align: center; margin-top: 20px;",
              actionButton(
                "go_to_setup", 
                "Continue to Analysis Setup",
                class = "btn btn-success btn-lg",
                icon = icon("arrow-right"),
                style = "padding: 12px 30px; font-size: 16px; font-weight: 600;
                         background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%);
                         border: none; border-radius: 8px; box-shadow: 0 4px 15px rgba(46, 204, 113, 0.3);"
              )
            )
          )
        )
      )
    )
  )
)
)
