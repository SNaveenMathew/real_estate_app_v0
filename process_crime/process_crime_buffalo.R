library(data.table)
library(tidyr)
library(tibble)
library(lubridate)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

df <- data.frame(data.table::fread(file.path(root_dir, "Crime", "Buffalo", "Crime_Incidents_20250724.csv")))
df$Incident.Datetime <- parse_date_time(df$Incident.Datetime, "mdy HMS p", tz = "")
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df_filtered <- df[(!is.na(df$Incident.Datetime)) & (!is.na(df$Latitude)) & (!is.na(df$Longitude)), ]
rownames(df_filtered) <- NULL

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "Incident.Datetime", lat_col = "Latitude", lon_col = "Longitude", lon_low = -79.1, lon_high = -78.4, lat_low = 42.5, lat_high = 43.02, frame_type = "year", city = "buffalo")
process_df_filtered(df_filtered, time_col = "Incident.Datetime", lat_col = "Latitude", lon_col = "Longitude", lon_low = -79.1, lon_high = -78.4, lat_low = 42.5, lat_high = 43.02, frame_type = "year_month", city = "buffalo")
