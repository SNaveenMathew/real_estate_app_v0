library(readxl)
library(plyr)
library(xts)
library(dplyr)

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir <- if(length(script_path)) dirname(normalizePath(script_path)) else "."
root_dir <- if(length(script_path)) normalizePath(file.path(script_dir, "..")) else "."

df <- read_excel(file.path(root_dir, "Crime", "Pittsburgh", "044f2016-1dfd-4ab0-bc1e-065da05fca2e.xlsx"))
df1 <- read_excel(file.path(root_dir, "Crime", "Pittsburgh", "archive-police-blotter.xlsx"))
df <- rbind.fill(df, df1)
rm(df1)
gc()
rownames(df) <- NULL
df$`_id` <- NULL
df$X[df$X == 0] <- NA
df$Y[df$Y == 0] <- NA
cols <- colnames(df)
cols[cols == "X"] <- "lon"
cols[cols == "Y"] <- "lat"
colnames(df) <- cols

df_filtered <- df[(!is.na(df$INCIDENTTIME)) & (!is.na(df$lon)) & (!is.na(df$lat)), ]
df_filtered$INCIDENTTIME <- format(gsub(pattern = "T", replacement = " ", x = df_filtered$INCIDENTTIME, ignore.case = T), format = "%Y-%m-%d %H:%M:%S")

source(file.path(root_dir, "process_crime.R"))
process_df_filtered(df_filtered, time_col = "INCIDENTTIME", lat_col = "lat", lon_col = "lon", lon_low = -80.3, lon_high = -79.6, lat_low = 40.1, lat_high = 40.7, frame_type = "year", city = "pittsburgh")
process_df_filtered(df_filtered, time_col = "INCIDENTTIME", lat_col = "lat", lon_col = "lon", lon_low = -80.3, lon_high = -79.6, lat_low = 40.1, lat_high = 40.7, frame_type = "year_month", city = "pittsburgh")
