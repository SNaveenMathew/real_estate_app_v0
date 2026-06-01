library(readxl)
library(plyr)
library(xts)
library(dplyr)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

df <- read_excel(file.path(root_dir, "Crime", "Baltimore", "Part1_Crime_Beta_3834807016258906358.xlsx"))
df$Longitude <- as.numeric(df$Longitude)
df$Longitude[df$Longitude == 0] <- NA
df$Latitude <- as.numeric(df$Latitude)
df$Latitude[df$Latitude == 0] <- NA

df_filtered <- df[(!is.na(df$CrimeDateTime)) & (!is.na(df$Longitude)) & (!is.na(df$Latitude)), ]
df_filtered <- df_filtered[(df_filtered$Latitude>=39) & (df_filtered$Latitude<=39.4) &
                             (df_filtered$Longitude>=-76.8) & (df_filtered$Longitude<=-76.5), ]
rownames(df_filtered) <- NULL
df_filtered <- data.frame(df_filtered)

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "CrimeDateTime", lat_col = "Latitude", lon_col = "Longitude", lon_low = -76.8, lon_high = -76.5, lat_low = 39, lat_high = 39.4, frame_type = "year", city = "baltimore")
process_df_filtered(df_filtered, time_col = "CrimeDateTime", lat_col = "Latitude", lon_col = "Longitude", lon_low = -76.8, lon_high = -76.5, lat_low = 39, lat_high = 39.4, frame_type = "year_month", city = "baltimore")
