# Update the calculate_auc_trapezoidal function to include all 4 methods:

calculate_auc_trapezoidal <- function(time, conc, method = "linear") {
  n <- length(time)
  if (n != length(conc)) stop("Time and concentration vectors must have the same length")
  if (n < 2) return(0)
  
  auc <- 0
  
  # Find Cmax and Tmax for linear_log method
  cmax_index <- which.max(conc)
  
  for (i in 2:n) {
    dt <- time[i] - time[i-1]
    c1 <- conc[i-1]
    c2 <- conc[i]
    
    if (dt <= 0) next  # Skip if time doesn't increase
    
    segment_auc <- 0
    
    if (method == "linear") {
      # Linear trapezoidal for all segments
      segment_auc <- (c1 + c2) * dt / 2
      
    } else if (method == "log") {
      # Log trapezoidal when both concentrations are positive
      if (c1 > 0 && c2 > 0 && c1 != c2) {
        segment_auc <- (c1 - c2) * dt / log(c1/c2)
      } else {
        # Fall back to linear when log cannot be used
        segment_auc <- (c1 + c2) * dt / 2
      }
      
    } else if (method == "mixed") {
      # Linear up/Log down (FDA preferred)
      if (c2 >= c1) {
        # Concentration increasing or equal - use linear
        segment_auc <- (c1 + c2) * dt / 2
      } else {
        # Concentration decreasing - use log if possible
        if (c1 > 0 && c2 > 0) {
          segment_auc <- (c1 - c2) * dt / log(c1/c2)
        } else {
          # Fall back to linear when log cannot be used
          segment_auc <- (c1 + c2) * dt / 2
        }
      }
      
    } else if (method == "linear_log") {
      # Linear up to Cmax, log after Cmax
      if (i <= cmax_index) {
        # Before or at Cmax - use linear (absorption phase)
        segment_auc <- (c1 + c2) * dt / 2
      } else {
        # After Cmax - use log if possible (elimination phase)
        if (c1 > 0 && c2 > 0 && c1 != c2) {
          segment_auc <- (c1 - c2) * dt / log(c1/c2)
        } else {
          # Fall back to linear when log cannot be used
          segment_auc <- (c1 + c2) * dt / 2
        }
      }
      
    } else {
      stop(paste("Unknown AUC calculation method:", method))
    }
    
    auc <- auc + segment_auc
  }
  
  return(auc)
}

# In the perform_nca function, ensure the AUC method is properly passed:

perform_nca <- function(data, auc_method = "mixed", lambda_z_method = "manual", 
                       lambda_z_points = 3, ...) {
  # ... existing code ...
  
  # Calculate AUC using the selected method
  pk_params$AUC0t <- calculate_auc_trapezoidal(time, conc, method = auc_method)
  
  # ... rest of the function ...
}