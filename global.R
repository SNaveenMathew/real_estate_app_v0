library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(jsonlite)
library(rCharts)
library(shiny)
library(unpack)
library(sf)
library(dplyr)

################
# Redfin homes #
################

# previously_processed_rows <- readRDS("previously_processed_rows.Rds")
latest_info <- readRDS("latest_info_without_sold.Rds")
latest_info_with_price <- latest_info
y <- strsplit(latest_info_with_price$price, " - ")
latest_info_with_price <- latest_info_with_price[sapply(y, length) == 1, ]
latest_info_with_price$price <- as.numeric(latest_info_with_price$price)
latest_info_with_price <- latest_info_with_price[(latest_info_with_price$price>=100000) &
                                                   (latest_info_with_price$price<=700000) &
                                                   (!is.na(latest_info_with_price$nearest_big_city)), ]
rownames(latest_info_with_price) <- NULL
latest_info_with_price <- latest_info_with_price[(latest_info_with_price$price>=100000) &
                                                   (latest_info_with_price$price<=700000) &
                                                   (!is.na(latest_info_with_price$nearest_big_city)), ]
rownames(latest_info_with_price) <- NULL
statuses <- unique(latest_info$status)
states <- unique(latest_info$state)
latest_info_with_price$nearest_big_city[latest_info_with_price$city == "Baltimore"] <- "Baltimore"
latest_info$nearest_big_city[latest_info$city == "Baltimore"] <- "Baltimore"
nearest_big_cities <- unique(latest_info$nearest_big_city)
property_types <- unique(latest_info$property_type)
latest_info_with_price$city[(latest_info_with_price$city == "Mount Oliver") &
                              (latest_info_with_price$nearest_big_city == "Pittsburgh")] <- "Mt Oliver"
latest_info_with_price$city[(latest_info_with_price$city == "Melrose Park") &
                              (latest_info_with_price$nearest_big_city == "Philadelphia")] <- "Elkins Park"

# Handling city = "Allegheny" or "Carson"
tmp_df <- merge(
  latest_info_with_price[, c("city", "address", "zip")],
  latest_info_with_price[(sapply(latest_info_with_price$city, function(x) x %in% c("Allegheny", "Carson"))) &
                           (latest_info_with_price$nearest_big_city == "Pittsburgh"), c("address", "zip")]
)
tmp_df1 <- tmp_df[sapply(tmp_df$city, function(x) x %in% c("Allegheny", "Carson")), ]
tmp_df2 <- tmp_df[sapply(tmp_df$city, function(x) !(x %in% c("Allegheny", "Carson"))), ]
colnames(tmp_df2) <- c("address", "zip", "replacement_city")
tmp_df <- merge(tmp_df1, tmp_df2)
latest_info_with_price <- merge(latest_info_with_price, tmp_df, all = TRUE)
latest_info_with_price$city[!is.na(latest_info_with_price$replacement_city)] <-
  latest_info_with_price$replacement_city[!is.na(latest_info_with_price$replacement_city)]
latest_info_with_price$replacement_city <- NULL
latest_info_with_price$city[((latest_info_with_price$city == "South Side") &
                              (sapply(latest_info_with_price$address, function(x)
                                x %in% c("444 William St", "446 William St"))) &
                              (latest_info_with_price$zip == 15211)) |
                              (latest_info_with_price$city == "Mount Washington")] <- "Mt Washington"

# Replacing city names with the distinct value that's not equal to the nearest_big_city
replacement_city <- latest_info_with_price %>% group_by(address, nearest_big_city, state, zip) %>%
  summarize(replacement_city = paste0(unique(city[city != nearest_big_city]), collapse = ""))
latest_info_with_price <- merge(latest_info_with_price, replacement_city, all = T)
latest_info_with_price$city[latest_info_with_price$replacement_city != ""] <-
  latest_info_with_price$replacement_city[latest_info_with_price$replacement_city != ""]
latest_info_with_price$replacement_city <- NULL

# Replacing walk, bike and transit score with the max values
x <- latest_info_with_price %>% group_by(address, nearest_big_city, state, zip) %>%
  summarize(count1 = n_distinct(walk_score[!is.na(walk_score)]), count2 = n_distinct(city),
            count3 = n_distinct(lat[!is.na(lat)]), count4 = n_distinct(lon[!is.na(lon)]),
            count5 = n_distinct(redfin_link[!is.na(redfin_link)]),
            count6 = n_distinct(zillow_link[!is.na(zillow_link)]),
            count7 = n_distinct(realtor_link[!is.na(realtor_link)]),
            count8 = n_distinct(bike_score[!is.na(bike_score)]),
            count9 = n_distinct(transit_score[!is.na(transit_score)]))
dedup <- function(latest_info_with_price, col = "walk_", count_col = "count1") {
  latest_info_with_price0 <- merge(latest_info_with_price, x[x[, count_col]==0, c("address", "nearest_big_city", "state", "zip")])
  latest_info_with_price2 <- merge(latest_info_with_price[, c("address", "nearest_big_city", "state", "zip",
                                                              paste0(col, "score"))],
                                   x[x[, count_col]>=1, c("address", "nearest_big_city", "state", "zip")])
  colOfInterest <- paste0(col, "score")
  latest_info_with_price2 <- latest_info_with_price2 %>% group_by(address, nearest_big_city, state, zip) %>%
    summarize(walk_score_r = max(.data[[colOfInterest]], na.rm = T))
  latest_info_with_price2 <- merge(latest_info_with_price2, latest_info_with_price)
  latest_info_with_price2[, paste0(col, "score")] <- latest_info_with_price2$walk_score_r
  latest_info_with_price2$walk_score_r <- NULL
  latest_info_with_price <- plyr::rbind.fill(latest_info_with_price0,
                                             latest_info_with_price2)
  return(latest_info_with_price)
}
latest_info_with_price <- dedup(latest_info_with_price, col = "walk_", count_col = "count1")
latest_info_with_price <- dedup(latest_info_with_price, col = "bike_", count_col = "count8")


latest_info_with_price$walk_score_r <- latest_info_with_price$bike_score_r <-
  latest_info_with_price$transit_score_r <- NULL
latest_info_with_price$walk_desc <- sapply(latest_info_with_price$walk_score, function(x) {
  if(!is.na(x)) {
  if((x>=0) & (x<50)) {
    return("Car dependent")
  } else if((x>=50) & (x<70)) {
    return("Somewhat walkable")
  } else if((x>=70) & (x<90)) {
    return("Very walkable")
  } else {
    return("Walker's paradise")
  }
  } else {
    return(NA)
  }
})
latest_info_with_price$bike_desc <- sapply(latest_info_with_price$bike_score, function(x) {
  if(!is.na(x)) {
    if((x>=0) & (x<50)) {
      return("Somewhat bikeable")
    } else if((x>=50) & (x<70)) {
      return("Bikeable")
    } else if((x>=70) & (x<90)) {
      return("Very bikeable")
    } else {
      return("Biker's paradise")
    }
  } else {
    return(NA)
  }
})
latest_info_with_price$transit_desc <- sapply(latest_info_with_price$transit_score, function(x) {
  if(!is.na(x)) {
    if((x>=0) & (x<25)) {
      return("Minimal Transit")
    } else if((x>=25) & (x<50)) {
      return("Some Transit")
    } else if((x>=50) & (x<70)) {
      return("Good Transit")
    } else if((x>=70) & (x<90)) {
      return("Excellent Transit")
    } else {
      return("Rider’s Paradise")
    }
  } else {
    return(NA)
  }
})

# Replacing lat, lon with the max values
latest_info_with_price1 <- merge(latest_info_with_price, x[x$count4==1, c("address", "nearest_big_city", "state", "zip")])
latest_info_with_price2 <- merge(latest_info_with_price[, c("address", "nearest_big_city", "state", "zip",
                                                            "lat", "lon")],
                                 x[x$count4>1, c("address", "nearest_big_city", "state", "zip")])
latest_info_with_price2 <- latest_info_with_price2 %>% group_by(address, nearest_big_city, state, zip) %>%
  summarize(lat_r = max(lat, na.rm = T), lon_r = max(lon, na.rm = T))
latest_info_with_price2 <- merge(latest_info_with_price2, latest_info_with_price)
latest_info_with_price2$lat <- latest_info_with_price2$lat_r
latest_info_with_price2$lon <- latest_info_with_price2$lon_r
latest_info_with_price2$lat_r <- latest_info_with_price2$lon_r <- NULL

latest_info_with_price <- plyr::rbind.fill(latest_info_with_price1,
                                           latest_info_with_price2)
latest_info_with_price <- latest_info_with_price[
  order(latest_info_with_price$address, latest_info_with_price$city,
        latest_info_with_price$state, latest_info_with_price$zip,
        latest_info_with_price$date, latest_info_with_price$status), ]
rownames(latest_info_with_price) <- NULL

latest_info_with_price1 <- merge(latest_info_with_price, x[x$count5==0, c("address", "nearest_big_city", "state", "zip")])
latest_info_with_price2 <- unique(
  merge(latest_info_with_price[, c("address", "nearest_big_city", "state", "zip",
                                   "redfin_link")],
                                 x[x$count5==1, c("address", "nearest_big_city", "state", "zip")]))
latest_info_with_price2$redfin_link_r <- latest_info_with_price2$redfin_link
latest_info_with_price2$redfin_link <- NULL
latest_info_with_price2 <- latest_info_with_price2[!is.na(latest_info_with_price2$redfin_link_r), ]
latest_info_with_price2 <- merge(latest_info_with_price, latest_info_with_price2)
latest_info_with_price <- plyr::rbind.fill(latest_info_with_price1,
                                           latest_info_with_price2)
latest_info_with_price$redfin_link_r <- NULL
latest_info_with_price <- latest_info_with_price[
  order(latest_info_with_price$address, latest_info_with_price$city,
        latest_info_with_price$state, latest_info_with_price$zip,
        latest_info_with_price$date, latest_info_with_price$status), ]
rownames(latest_info_with_price) <- NULL
latest_info_with_price$sq_ft <- as.numeric(latest_info_with_price$sq_ft)

#####################
# Market heat index #
#####################

# heat_idx <- data.table::fread()

####################
# Pittsburgh crime #
####################

month_frame_data_pittsburgh <- readRDS("frame_data_year_month_pittsburgh.Rds")
year_frame_data_pittsburgh <- readRDS("frame_data_pittsburgh.Rds")
tmp <- readRDS("mn_lat_lon_pittsburgh.Rds")
mn_lat_pittsburgh <- tmp[1]
mn_lon_pittsburgh <- tmp[2]

source("process_crime.R")
last_year_df_pittsburgh <- process_last_year(month_frame_data_pittsburgh, city = "pittsburgh")
last_year_month_frame_data_pittsburgh <- last_year_df_pittsburgh[['last_year_month_frame_data']]
last_year_df_pittsburgh <- last_year_df_pittsburgh[['last_year_df']]

#################
# Chicago crime #
#################

month_frame_data_chicago <- readRDS("frame_data_year_month_chicago.Rds")
year_frame_data_chicago <- readRDS("frame_data_chicago.Rds")
tmp <- readRDS("mn_lat_lon_chicago.Rds")
mn_lat_chicago <- tmp[1]
mn_lon_chicago <- tmp[2]

last_year_df_chicago <- process_last_year(month_frame_data_chicago, city = "chicago")
last_year_month_frame_data_chicago <- last_year_df_chicago[['last_year_month_frame_data']]
last_year_df_chicago <- last_year_df_chicago[['last_year_df']]

################
# Boston crime #
################

month_frame_data_boston <- readRDS("frame_data_year_month_boston.Rds")
year_frame_data_boston <- readRDS("frame_data_boston.Rds")
tmp <- readRDS("mn_lat_lon_boston.Rds")
mn_lat_boston <- tmp[1]
mn_lon_boston <- tmp[2]

last_year_df_boston <- process_last_year(month_frame_data_boston, city = "boston")
last_year_month_frame_data_boston <- last_year_df_boston[['last_year_month_frame_data']]
last_year_df_boston <- last_year_df_boston[['last_year_df']]

###################
# Baltimore crime #
###################

month_frame_data_baltimore <- readRDS("frame_data_year_month_baltimore.Rds")
year_frame_data_baltimore <- readRDS("frame_data_baltimore.Rds")
tmp <- readRDS("mn_lat_lon_baltimore.Rds")
mn_lat_baltimore <- tmp[1]
mn_lon_baltimore <- tmp[2]

last_year_df_baltimore <- process_last_year(month_frame_data_baltimore, city = "baltimore")
last_year_month_frame_data_baltimore <- last_year_df_baltimore[['last_year_month_frame_data']]
last_year_df_baltimore <- last_year_df_baltimore[['last_year_df']]

######################
# Indianapolis crime #
######################

month_frame_data_indianapolis <- readRDS("frame_data_year_month_indianapolis.Rds")
year_frame_data_indianapolis <- readRDS("frame_data_indianapolis.Rds")
tmp <- readRDS("mn_lat_lon_indianapolis.Rds")
mn_lat_indianapolis <- tmp[1]
mn_lon_indianapolis <- tmp[2]

last_year_df_indianapolis <- process_last_year(month_frame_data_indianapolis, city = "indianapolis")
last_year_month_frame_data_indianapolis <- last_year_df_indianapolis[['last_year_month_frame_data']]
last_year_df_indianapolis <- last_year_df_indianapolis[['last_year_df']]

#################
# Buffalo crime #
#################

month_frame_data_buffalo <- readRDS("frame_data_year_month_buffalo.Rds")
year_frame_data_buffalo <- readRDS("frame_data_buffalo.Rds")
tmp <- readRDS("mn_lat_lon_buffalo.Rds")
mn_lat_buffalo <- tmp[1]
mn_lon_buffalo <- tmp[2]

last_year_df_buffalo <- process_last_year(month_frame_data_buffalo, city = "buffalo")
last_year_month_frame_data_buffalo <- last_year_df_buffalo[['last_year_month_frame_data']]
last_year_df_buffalo <- last_year_df_buffalo[['last_year_df']]

######################
# Philadelphia crime #
######################

month_frame_data_philadelphia <- readRDS("frame_data_year_month_philadelphia.Rds")
year_frame_data_philadelphia <- readRDS("frame_data_philadelphia.Rds")
tmp <- readRDS("mn_lat_lon_philadelphia.Rds")
mn_lat_philadelphia <- tmp[1]
mn_lon_philadelphia <- tmp[2]

last_year_df_philadelphia <- process_last_year(month_frame_data_philadelphia, city = "philadelphia")
last_year_month_frame_data_philadelphia <- last_year_df_philadelphia[['last_year_month_frame_data']]
last_year_df_philadelphia <- last_year_df_philadelphia[['last_year_df']]

#####################
# Minneapolis crime #
#####################

month_frame_data_minneapolis <- readRDS("frame_data_year_month_minneapolis.Rds")
year_frame_data_minneapolis <- readRDS("frame_data_minneapolis.Rds")
tmp <- readRDS("mn_lat_lon_minneapolis.Rds")
mn_lat_minneapolis <- tmp[1]
mn_lon_minneapolis <- tmp[2]

last_year_df_minneapolis <- process_last_year(month_frame_data_minneapolis, city = "minneapolis")
last_year_month_frame_data_minneapolis <- last_year_df_minneapolis[['last_year_month_frame_data']]
last_year_df_minneapolis <- last_year_df_minneapolis[['last_year_df']]

####################
# Pittsburgh flood #
####################

shapeData_pittsburgh <- sf::read_sf("Flood/Pittsburgh/S_FLD_HAZ_AR.shp")

########################
# Pittsburgh walk/bike #
########################

bike_lanes <- st_read("BikePGH/Bike Lanes/Bike Lanes.shp")
bike_lanes <- st_transform(bike_lanes, 4326)
sidewalks <- st_read("BikePGH/Bikeable Sidewalks/Bikeable_Sidewalks.shp")
sidewalks <- st_transform(sidewalks, 4326)
caution <- st_read("BikePGH/Cautionary Bike Route/Cautionary Bike Route.shp")
caution <- st_transform(caution, 4326)
on_street <- st_read("BikePGH/On Street Bike Route/On Street Bike Route.shp")
on_street <- st_transform(on_street, 4326)
protected <- st_read("BikePGH/Protected Bike Lanes/Protected Bike Lane.shp")
protected <- st_transform(protected, 4326)
sharrows <- st_read("BikePGH/Sharrows/Sharrows.shp")
sharrows <- st_transform(sharrows, 4326)
trail <- st_read("BikePGH/Trails/Trails.shp")
trail <- st_transform(trail, 4326)

# df <- sf::st_read("Smart Location/SmartLocationDatabase.gdb/a00000009.gdbtable")
df <- readRDS("SLD.Rds")
df1 <- df %>% filter(CSA_Name == "Pittsburgh-New Castle-Weirton, PA-OH-WV")
# df1 <- df1[df1$CSA_Name == "Pittsburgh-New Castle-Weirton, PA-OH-WV", ]
# class(df1) <- c("sf", "data.frame")
df1_1 <- st_transform(df1, 4326)
pal <- colorNumeric(palette = "viridis", domain = df1_1$NatWalkInd)
