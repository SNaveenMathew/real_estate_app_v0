if(!exists("root_dir")) root_dir <- "."

process_df_filtered <- function(df_filtered, time_col = "INCIDENTTIME", lat_col = "lat", lon_col = "lon", lon_low = -80.3, lon_high = -79.6, lat_low = 40.1, lat_high = 40.7, frame_type = "year", city = "pittsburgh") {
  cols <- colnames(df_filtered)
  colnames(df_filtered)[cols == lat_col] <- "lat"
  colnames(df_filtered)[cols == lon_col] <- "lon"
  df_filtered$year <- format(as.Date(df_filtered[, time_col]), "%Y")
  year_summary <- df_filtered %>% dplyr::group_by(year) %>% dplyr::summarize(count=dplyr::n())
  year_summary <- year_summary[year_summary$count>=10, ]
  df_filtered <- merge(df_filtered, year_summary)
  df_filtered <- df_filtered[(df_filtered$lon <= lon_high) & (df_filtered$lon >= lon_low) & (df_filtered$lat >= lat_low) & (df_filtered$lat <= lat_high), ]
  rownames(df_filtered) <- NULL
  
  if(frame_type == "year") {
    dots <- lapply(c("year", "lat", "lon"), as.symbol)
    df_filtered <- data.frame(df_filtered %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize(count = dplyr::n()))
    saveRDS(df_filtered, file.path(root_dir, paste0("df_filtered_", city, ".Rds")))
    yrs <- sort(unique(df_filtered$year))
    frames <- lapply(yrs, function(yr) {
      df_ <- df_filtered[df_filtered$year == yr, ]
      rownames(df_) <- NULL
      return(rCharts::toJSONArray2(na.omit(df_[, colnames(df_)[colnames(df_) != "year"]]), json = F, names = F))
    })
    names(frames) <- yrs
  } else if(frame_type == "year_month") {
    df_filtered$year_month <- format(as.Date(df_filtered[, time_col]), "%Y-%m")
    dots <- lapply(c("year_month", "lat", "lon"), as.symbol)
    df_filtered <- data.frame(df_filtered %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize(count = dplyr::n()))
    saveRDS(df_filtered, file.path(root_dir, paste0("df_filtered_year_month_", city, ".Rds")))
    yr_mnths <- sort(unique(df_filtered$year_month))
    frames <- lapply(yr_mnths, function(yr_mnth) {
      df_ <- df_filtered[df_filtered$year_month == yr_mnth, ]
      rownames(df_) <- NULL
      return(rCharts::toJSONArray2(na.omit(df_[, colnames(df_)[colnames(df_) != "year_month"]]), json = F, names = F))
    })
    names(frames) <- yr_mnths
  }
  
  frame_data <- setNames(frames, names(frames))
  lats <- unlist(sapply(frames, function(x) {sapply(x, function(y) y[[1]])}))
  lons <- unlist(sapply(frames, function(x) {sapply(x, function(y) y[[2]])}))
  mn_lat <- mean(df_filtered$lat, na.rm = T)
  mn_lon <- mean(df_filtered$lon, na.rm = T)
  
  if(frame_type == "year") {
    saveRDS(frame_data, file.path(root_dir, paste0("frame_data_", city, ".Rds")))
  } else if(frame_type == "year_month") {
    saveRDS(frame_data, file.path(root_dir, paste0("frame_data_year_month_", city, ".Rds")))
  }
  saveRDS(c(mn_lat, mn_lon), file.path(root_dir, paste0("mn_lat_lon_", city, ".Rds")))
}

process_last_year <- function(month_frame_data, city = "pittsburgh") {
  years <- sapply(strsplit(names(month_frame_data), "-"), function(x) x[1])
  years <- as.numeric(years)
  last_year_month_frame_data <- month_frame_data[names(month_frame_data)[years == max(years)]]
  
  last_full_year <- table(years)
  last_full_year <- last_full_year[last_full_year == 12]
  last_full_year <- max(names(last_full_year))
  last_year_df <- readRDS(file.path(root_dir, paste0("df_filtered_", city, ".Rds")))
  last_year_df <- last_year_df[last_year_df$year == last_full_year, ]
  ret <- list(last_year_month_frame_data, last_year_df)
  names(ret) <- c("last_year_month_frame_data", "last_year_df")
  return(ret)
}
