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

crime_data <- data.table::fread(file.path(root_dir, "Crime", "Boston", "Boston_Incidents_View_3136327856209597499.csv"))
crime_data$`Report Date` <- parse_date_time(crime_data$`Report Date`, '%m/%d/%Y %I:%M:%S %p')
crime_data$`From Date` <- parse_date_time(crime_data$`From Date`, '%m/%d/%Y %I:%M:%S %p')
crime_data$`To Date` <- parse_date_time(crime_data$`To Date`, '%m/%d/%Y %I:%M:%S %p')
crime_data$`From Date`[is.na(crime_data$`From Date`)] <- crime_data$`Report Date`[is.na(crime_data$`From Date`)]
crime_data$`From Date`[is.na(crime_data$`From Date`)] <- crime_data$`To Date`[is.na(crime_data$`From Date`)]

crime_data <- crime_data[!is.na(crime_data$`From Date`), ]
crime_data <- crime_data[crime_data$`Block Address`!="", ]
rownames(crime_data) <- NULL

address <- paste(crime_data$`Block Address`, crime_data$City, crime_data$`Zip Code`)
address <- gsub(pattern = "[\ ]{1,}", replacement = " ", x = address)
address <- gsub(pattern = " ", replacement = "%20", x = address)
uniq_address <- unique(address)
block_address_condition <- grepl(pattern = "^[0-9]", x = uniq_address, ignore.case = T)
intersections <- uniq_address[!block_address_condition]
block_addresses <- uniq_address[block_address_condition]

##############################################################################################
# PTV API query is not required because lat, lon is already available in the Redfin CSV dump #
##############################################################################################

ptv_api_key <- Sys.getenv("PTV_API_KEY", unset = "")
if(ptv_api_key == "") {
  stop("PTV_API_KEY must be set in .Renviron.local or the environment before running process_crime_boston.R")
}

get_lat_lon <- function(address_) {
  tryCatch({
    ptv_query <- paste0("https://api.myptv.com/geocoding/v1/locations/by-text?searchText=", address_, "&apiKey=", ptv_api_key)
    ptv_results <- httr::GET(ptv_query)
    ptv_list_results <- jsonlite::fromJSON(content(ptv_results, as = "text"))$locations
    lat <- round(ptv_list_results$referencePosition$latitude, digits = 4)
    lon <- round(ptv_list_results$referencePosition$longitude, digits = 4)
    return(c(lat, lon))
  }, error = function(e) return(c(0.0, 0.0)))
}

block_lat_lon <- sapply(block_addresses, get_lat_lon)
num_lat_lons <- sapply(block_lat_lon, length)/2
table(num_lat_lons)

no_lat_lons <- sapply(block_lat_lon, function(x) (x[1]==0) & (x[2]==0))
no_lat_lon_addresses <- block_lat_lon[no_lat_lons]
no_lat_lon_addresses <- names(no_lat_lon_addresses)
locs <- data.frame(lat = rep(0.0, length(no_lat_lon_addresses)), lon = rep(0.0, length(no_lat_lon_addresses)))
locs$address <- no_lat_lon_addresses
locs[1, c(1, 2)] <- c(42.325439, -71.054189)
locs[2, c(1, 2)] <- c(42.332010, -71.052274)
locs[3, c(1, 2)] <- c(42.326867, -71.055493)
locs[4, c(1, 2)] <- c(42.280429, -71.170778)
locs[5, c(1, 2)] <- c(42.343346, -71.036678)
block_lat_lon <- block_lat_lon[!no_lat_lons]
finals <- sapply(block_lat_lon, length)
final_lat_long <- block_lat_lon[finals == 2]
final_lat_lon <- data.frame(lat = sapply(final_lat_long, function(x) x[1]),
                            lon = sapply(final_lat_long, function(x) x[2]))
final_lat_lon$address <- rownames(final_lat_lon)
rownames(final_lat_lon) <- NULL
final_lat_lon <- rbind.fill(final_lat_lon, locs[1:5, ])
nrow(final_lat_lon[(final_lat_lon$lat>=43) | (final_lat_lon$lat<=42), ])# There are some errors
final_lat_lon$address <- gsub(pattern = "%20", replacement = " ", x = final_lat_lon$address, fixed = T)
crime_data$`Block Address` <- gsub(pattern = "  ", replacement = " ", x = crime_data$`Block Address`)
crime_data$address <- paste(crime_data$`Block Address`, crime_data$City, crime_data$`Zip Code`)

crime_data_ <- merge(crime_data, final_lat_lon[(final_lat_lon$lat<=43) & (final_lat_lon$lat>=42), ])
mn_lon <- mean(crime_data_$lon)
mn_lat <- mean(crime_data_$lat)
leaflet(data = crime_data_) %>% addTiles() %>% addHeatmap(radius = 10, group = "heatmap") %>% setView(lng = mn_lon, lat = mn_lat, zoom = 12)

