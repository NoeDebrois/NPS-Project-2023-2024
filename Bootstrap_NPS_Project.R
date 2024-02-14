# This code loads data from multiple CSV files, performs linear regression on each
# dataset, conducts Bootstrap inference to obtain confidence intervals for 
# regression coefficients, and then exports the results to a CSV file.

# Load necessary libraries
library(dplyr)
library(lubridate)

################################################################################
## DATA LOADER #################################################################
################################################################################
# Path to the folder containing the files
folder_path <- "your_path"
# Get the list of files in the folder
files <- list.files(path = folder_path, pattern = "\\.csv", full.names = TRUE)
# Initialize a list to store the data
data_list <- list()
# Loop to read each file and store the data in the list
for (file in files) {
  # Read the file and store the data in the list
  data <- read.table(file, header = TRUE, sep = ",")
  data_list[[basename(file)]] <- data
}

# Calculate the length of data_list
list_length <- length(data_list)
# Create an empty array with the number of rows equal to the length of data_list
RP <- array(NA, dim = c(list_length, 12, 3))

# Define the total number of iterations (adjust as needed)
number_iterations <- length(data_list)
# Create a text progress bar
progress_bar <- txtProgressBar(min = 0, max = number_iterations, style = 3)
# Loop with progress bar
for (i in 1:number_iterations) {
  # Import data from a file
  data <- data_list[[i]] # access the i-th table
  if (nrow(data) == 0){
    next
  }
  # Set the parameter of interest
  data <- data[!(is.na(data$pres)), ]
  data$time <- as.Date(data$time)
  
  for (month in 1:12) {
    data_current_year <- data %>% filter(month(time) == month)
    if (nrow(data_current_year) == 0){
      next
    }
    
    years <- unique(year(data_current_year$time))
    
    # Response and predictor variables for the parameter of interest
    response  <- data_current_year$pres
    regressor <- years
    
    # Fit a linear model
    fm <- lm(response ~ regressor)
    
    ################################################################################
    ### Bootstrap Inference ########################################################
    ################################################################################
    # Calculate residuals and fitted values
    fitted.obs <- fitted(fm)
    res.obs    <- residuals(fm)
    
    # Estimated coefficients of the original model
    b0.obs <- coefficients(fm)[1] # intercept of the regression line
    b1.obs <- coefficients(fm)[2] # slope of the regression line
    
    # Set random seed for reproducibility
    set.seed(24021979)
    # Number of Bootstrap iterations
    B <- 1
    # Initialize vectors to store Bootstrap estimates of coefficients
    T.boot.b0 <- numeric(B)
    T.boot.b1 <- numeric(B)
    
    # Loop over Bootstrap iterations
    for (b in 1:B) {
      # Resample residuals with replacement
      response.b <- fitted.obs + sample(res.obs, replace = TRUE)
      # Fit a linear model on Bootstrap data
      fm.b <- lm(response.b ~ regressor)
      # Store Bootstrap estimated coefficients
      T.boot.b0[b] <- coefficients(fm.b)[1]
      T.boot.b1[b] <- coefficients(fm.b)[2]
    }
    
    # Calculate quantiles for the intercept
    right.quantile.b0 <- quantile(T.boot.b0, 1 - alpha/2, na.rm = TRUE)
    left.quantile.b0  <- quantile(T.boot.b0, alpha/2, na.rm = TRUE)
    
    # Calculate Reverse Percentile confidence interval for the intercept
    CI.RP.b0 <- c(b0.obs - (right.quantile.b0 - b0.obs), b0.obs - (left.quantile.b0 - b0.obs))
    
    # Calculate quantiles for the slope
    right.quantile.b1 <- quantile(T.boot.b1, 1 - alpha/2, na.rm = TRUE)
    left.quantile.b1  <- quantile(T.boot.b1, alpha/2, na.rm = TRUE)
    
    # Calculate Reverse Percentile confidence interval for the slope
    CI.RP.b1 <- c(b1.obs - (right.quantile.b1 - b1.obs), b1.obs - (left.quantile.b1 - b1.obs))
    
    # Store confidence interval for the slope in the array
    RP[i, month, 1] <- unname(CI.RP.b1)[1]
    RP[i, month, 2] <- unname(b1.obs)
    RP[i, month, 3] <- unname(CI.RP.b1)[2]
  }
  # Update progress bar
  setTxtProgressBar(progress_bar, i)
  # Force immediate display of progress bar in the console
  flush.console()
}
# Print a new line to avoid overwriting the progress bar
cat("\n")
# Close the progress bar at the end
close(progress_bar)

# Rename rows and columns
row_names <- basename(files)
dimnames(RP)[[1]] <- row_names 
column_names <- c("RP Int January", "RP Int February", "RP Int March", "RP Int April", "RP Int May", "RP Int June", "RP Int July", "RP Int August", "RP Int September", "RP Int October", "RP Int November", "RP Int December")
dimnames(RP)[[2]] <- column_names
matrix_names <- c("M1", "M2", "M3")
dimnames(RP)[[3]] <- matrix_names

# Specify the full path to the output file
output_file_path <- "your_path"
# Export the dataframe in CSV format
write.csv(RP, file = output_file_path, row.names = TRUE)
