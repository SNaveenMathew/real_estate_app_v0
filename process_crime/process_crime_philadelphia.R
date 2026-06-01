library(data.table)
library(plyr)
library(xts)
library(dplyr)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

files <- list.files(file.path(root_dir, "Crime", "Philadelphia"))
df_filtered <- c()
for(file in files) {
  df <- data.table::fread(file.path(root_dir, "Crime", "Philadelphia", file))
  
  tmp_ <- df[(!is.na(df$lat)) & (!is.na(df$lng)) & (!is.na(df$dispatch_date_time)), ]
  tmp_ <- tmp_[(tmp_$lat>=39.8) & (tmp_$lat<=40.2) &
                 (tmp_$lng>=-75.3) & (tmp_$lng<=-74.9), ]
  rownames(tmp_) <- NULL
  tmp_ <- data.frame(tmp_)
  df_filtered <- rbind.fill(df_filtered, tmp_)
}
rownames(df_filtered) <- NULL

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "dispatch_date_time", lat_col = "lat", lon_col = "lng", lon_low = -75.3, lon_high = -74.9, lat_low = 39.8, lat_high = 40.2, frame_type = "year", city = "philadelphia")
process_df_filtered(df_filtered, time_col = "dispatch_date_time", lat_col = "lat", lon_col = "lng", lon_low = -75.3, lon_high = -74.9, lat_low = 39.8, lat_high = 40.2, frame_type = "year_month", city = "philadelphia")
