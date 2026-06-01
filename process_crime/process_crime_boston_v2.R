library(data.table)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(plyr)
library(leaflet)
library(leaflet.extras)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

# Boston
crime_data <- data.frame()
for(i in 2015:2023) {
  tmp_data <- data.table::fread(file.path(root_dir, "Crime", "Boston", paste0(as.character(i), ".csv")))
  crime_data <- rbind.fill(crime_data, tmp_data)
}

crime_data <- crime_data[!is.na(crime_data$OCCURRED_ON_DATE), ]
crime_data <- crime_data[!is.na(crime_data$Lat) & !is.na(crime_data$Long), ]
rownames(crime_data) <- NULL

# Cambridge
cambridge_raw <- data.frame(data.table::fread(file.path(root_dir, "Crime", "Boston", "Cambridge", "Crime_Reports_20250726.csv")))
cambridge_raw <- cambridge_raw[!is.na(cambridge_raw$Location), ]
rownames(cambridge_raw) <- NULL

uniqs <- data.frame(Location = unique(cambridge_raw$Location), lat = 1, lon = 1)

pb <- progress::progress_bar$new(total = nrow(uniqs))

for(i in 1:nrow(uniqs)) {
  result <- tryCatch({
    geo <- tmaptools::geocode_OSM(uniqs$Location[i], as.data.frame = TRUE)
    uniqs$lat[i] <- geo$lat
    uniqs$lon[i] <- geo$lon
  }, error = function(e) {
    uniqs$lat[i] <- NA
    uniqs$lon[i] <- NA
  })
  pb$tick()
  # Sys.sleep(1)  # To respect OSM API limits
}

# Add geocoded columns
cambridge_raw <- merge(cambridge_raw, uniqs)
cols <- colnames(cambridge_raw)
colnames(cambridge_raw)[cols == "lat"] <- "Lat"
colnames(cambridge_raw)[cols == "lon"] <- "Long"
colnames(cambridge_raw)[cols == "Crime Date Time"] <- "OCCURRED_ON_DATE"
cambridge_raw <- cambridge_raw[!is.na(cambridge_raw$Lat) & !is.na(cambridge_raw$Long) & (cambridge_raw$Lat != 1) & (cambridge_raw$Long != 1), ]
cambridge_raw <- data.frame(cambridge_raw)
strsplit(cambridge_raw$OCCURRED_ON_DATE, " - ")
rownames(cambridge_raw) <- NULL

reqd_cols <- c("OCCURRED_ON_DATE", "Lat", "Long")
crime_data <- plyr::rbind.fill(crime_data[, reqd_cols], cambridge_raw[, reqd_cols])

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(crime_data, time_col = "OCCURRED_ON_DATE", lat_col = "Lat", lon_col = "Long", lon_low = -71.4, lon_high = -71, lat_low = 42, lat_high = 42.5, frame_type = "year", city = "boston")
process_df_filtered(crime_data, time_col = "OCCURRED_ON_DATE", lat_col = "Lat", lon_col = "Long", lon_low = -71.4, lon_high = -71, lat_low = 42, lat_high = 42.5, frame_type = "year_month", city = "boston")
