library(data.table)
library(dplyr)
library(lubridate)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

crime_data <- data.table::fread(file.path(root_dir, "Crime", "Chicago", "Crimes_-_2001_to_Present_20250612.csv"))
crime_data$Date <- parse_date_time(crime_data$Date, '%m/%d/%Y %I:%M:%S %p')
crime_data$month <- month(crime_data$Date)
crime_data %>% group_by(Year, month) %>% summarize(count = sum(!is.na(Latitude) & !is.na(Longitude)))
crime_data_filtered <- crime_data[!is.na(crime_data$Latitude) & !is.na(crime_data$Longitude), ]
rownames(crime_data_filtered) <- NULL
crime_data_filtered <- as.data.frame(crime_data_filtered)

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(crime_data_filtered, time_col = "Date", lat_col = "Latitude", lon_col = "Longitude", lon_low = min(crime_data_filtered$Longitude), lon_high = max(crime_data_filtered$Longitude), lat_low = min(crime_data_filtered$Latitude), lat_high = max(crime_data_filtered$Latitude), frame_type = "year", city = "chicago")
process_df_filtered(crime_data_filtered, time_col = "Date", lat_col = "Latitude", lon_col = "Longitude", lon_low = min(crime_data_filtered$Longitude), lon_high = max(crime_data_filtered$Longitude), lat_low = min(crime_data_filtered$Latitude), lat_high = max(crime_data_filtered$Latitude), frame_type = "year_month", city = "chicago")
