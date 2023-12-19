# Load necessary libraries
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tools)
setwd("/Users/kevinledezma/Downloads")
# Function to perform time series analysis
perform_time_series_analysis <- function(file_name) {
  # Read the CSV file
  dat <- read.csv(file_name)
  
  # Rename columns and adjust timestamp format
  dat <- dat %>%
    rename(
      time_stamp = X,
      power = Total.Power..kWh.
    ) %>%
    mutate(
      time_stamp = as.Date(time_stamp, "%m/%d/%Y"),
      heat_sum = Heating..Full...kWh. + Heating..Part...kWh.,
      cool_sum = Cooling..Part...kWh. + Cooling..Full...kWh.
    )
  
  # Convert timestamp to POSIXct if not already in that format
  dat$time_stamp <- as.POSIXct(dat$time_stamp)
  
  # Create time series object
  ts_data <- ts(dat$power, frequency = 365)  # Assuming daily frequency (adjust if necessary)
  
  # Fit ARIMA model
  arima_model <- auto.arima(ts_data)
  
  # Summary of the ARIMA model
  print(summary(arima_model))
  
  # Forecast future values
  forecast_values <- forecast(arima_model, h = 30)  # Adjust 'h' as needed for the number of days to forecast
  
  # Plot forecast
  pdf("/Users/kevinledezma/Downloads/plot.pdf")
  plot(forecast_values)
  ggplot(dat, aes(x = time_stamp, y = power)) +
    geom_line() +
    geom_smooth(se = TRUE)
  plot_file_name <- paste0("plot_", tools::file_path_sans_ext(basename(file_name)), ".pdf")
  pdf(plot_file_name)
  plot(forecast_values)
  dev.off()
  
  # Print the path to the saved plot file
  cat("Plot saved as:", plot_file_name, "\n")
}

# Command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if argument is provided
if (length(args) == 0) {
  stop("Please provide the CSV file name as an argument.")
} else {
  file_name <- args[1]
  perform_time_series_analysis(file_name)
}