library(data.table)
library(plyr)
library(xts)
library(dplyr)
library(lubridate)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

files <- list.files(file.path(root_dir, "Crime", "Indianapolis"))
df_filtered <- c()
for(file in files) {
  df <- data.table::fread(file.path(root_dir, "Crime", "Indianapolis", file))
  df$DATE_ <- as.Date(df$DATE_)
  tmp_df_filtered <- df[(!is.na(df$DATE_)) & (!is.na(df$ADDRESS)) & (df$X_COORD != 0) & (df$Y_COORD != 0) & (!is.na(df$X_COORD)) & (!is.na(df$Y_COORD)), ]
  # df$date <- parse_date_time(gsub(pattern = ",", replacement = "", x = df$date), "mdy HMS p", tz="")
  # tmp_df_filtered <- df[(!is.na(df$date)) & (!is.na(df$blocksizedAddress)), ]
  df_filtered <- rbind.fill(df_filtered, tmp_df_filtered)
}

df_filtered$DATE_TIME <- parse_date_time(paste(df_filtered$DATE_, df_filtered$TIME), "ymd HM", tz = "")
c1 <- is.na(df_filtered$DATE_TIME)
df_filtered$DATE_TIME[c1] <- parse_date_time(paste(df_filtered$DATE_[c1], df_filtered$TIME[c1]), "ymd HMS", tz = "")
c1 <- is.na(df_filtered$DATE_TIME)
df_filtered$DATE_TIME[c1] <- parse_date_time(paste(df_filtered$DATE_[c1], df_filtered$TIME[c1]), "ymd", tz = "")
c1 <- is.na(df_filtered$DATE_TIME)
df_filtered$DATE_TIME[c1] <- parse_date_time(df_filtered$DATE_[c1], "ymd", tz = "")

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "DATE_TIME", lat_col = "Y_COORD", lon_col = "X_COORD", lon_low = -86.4, lon_high = -85.9, lat_low = 39.6, lat_high = 40, frame_type = "year", city = "indianapolis")
process_df_filtered(df_filtered, time_col = "DATE_TIME", lat_col = "Y_COORD", lon_col = "X_COORD", lon_low = -86.4, lon_high = -85.9, lat_low = 39.6, lat_high = 40, frame_type = "year_month", city = "indianapolis")
