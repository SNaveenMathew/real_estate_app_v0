library(data.table)
library(plyr)
library(xts)
library(dplyr)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

df <- data.table::fread(file.path(root_dir, "Crime", "Minneapolis", "Minneapolis.csv"))
df$Longitude <- as.numeric(df$Longitude)
df$Longitude[df$Longitude == 0] <- NA
df$Latitude <- as.numeric(df$Latitude)
df$Latitude[df$Latitude == 0] <- NA
df$Occurred_Date <- lubridate::as_datetime(sapply(strsplit(x = df$Occurred_Date, split = "\\+"), function(x) x[1]), tz = "")

df_filtered <- df[(!is.na(Occurred_Date)) & (!is.na(df$Longitude)) & (!is.na(df$Latitude)), ]
df_filtered <- df_filtered[(df_filtered$Latitude>=44.8) & (df_filtered$Latitude<=45.1) &
                             (df_filtered$Longitude>=-93.4) & (df_filtered$Longitude<=-93.1), ]
rownames(df_filtered) <- NULL
df_filtered <- data.frame(df_filtered)

df <- data.table::fread(file.path(root_dir, "Crime", "Minneapolis", "St Paul.csv"))
df$Longitude <- as.numeric(df$Longitude)
df$Longitude[df$Longitude == 0] <- NA
df$Latitude <- as.numeric(df$Latitude)
df$Latitude[df$Latitude == 0] <- NA
df$DATE <- lubridate::as_datetime(sapply(strsplit(x = df$DATE, split = "\\+"), function(x) x[1]), tz = "")

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "Occurred_Date", lat_col = "Latitude", lon_col = "Longitude", lon_low = -93.4, lon_high = -93.1, lat_low = 44.8, lat_high = 45.1, frame_type = "year", city = "minneapolis")
process_df_filtered(df_filtered, time_col = "Occurred_Date", lat_col = "Latitude", lon_col = "Longitude", lon_low = -93.4, lon_high = -93.1, lat_low = 44.8, lat_high = 45.1, frame_type = "year_month", city = "minneapolis")
