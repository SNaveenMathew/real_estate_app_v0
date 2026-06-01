library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(openxlsx)

remove_previously_processed_rows <- function(house_data, previously_processed_rows) {
  previously_processed_rows <- previously_processed_rows[!is.na(previously_processed_rows$address), ]
  if(nrow(previously_processed_rows) > 0) {
    for(i in 1:nrow(previously_processed_rows)) {
      house_data <- house_data[house_data$ADDRESS != previously_processed_rows$address[i] |
                                 house_data$CITY != previously_processed_rows$city[i] |
                                 house_data$STATE.OR.PROVINCE != previously_processed_rows$state[i] |
                                 house_data$ZIP.OR.POSTAL.CODE != previously_processed_rows$zip[i] |
                                 house_data$PRICE != previously_processed_rows$price[i] |
                                 house_data$PROPERTY.TYPE != previously_processed_rows$property_type[i] |
                                 house_data$SQUARE.FEET != previously_processed_rows$sq_ft[i] |
                                 house_data$STATUS != previously_processed_rows$status[i], ]
    }
  }
  return(house_data)
}

walk_score <- NA
walk_desc <- NA
bike_score <- NA
bike_desc <- NA
transit_score <- NA
transit_desc <- NA

excel_files <- list.files(".", full.names = T)
excel_files <- sort(excel_files[grepl(excel_files, pattern = "\\.csv$", ignore.case = T) &
                      grepl(excel_files, pattern = "redfin", ignore.case = F)])
walkscore_api_key <- "dd9c1880465ecfd970388a5a454d0ec9"
walkscore_api_query_link <- "https://api.walkscore.com/score?format=json&address="

#####################################
# Loading previously processed rows #
#####################################

previously_processed_rows <- data.frame(stringsAsFactors = F)
tryCatch({
  previously_processed_rows <<- readRDS("previously_processed_rows.Rds")
}, error = function(e) {
  NULL
}, warning = function(e) {
  NULL
})
previously_processed_rows <- previously_processed_rows[!is.na(previously_processed_rows$address), ]
rownames(previously_processed_rows) <- NULL


#########################
# Looping through files #
#########################

for(file in excel_files) {
  # file <- excel_files[1]
  
  ################################################
  # Reading data, removing redundant information #
  ################################################
  
  house_data <- read.csv(file)
  house_data$STATE.OR.PROVINCE[house_data$ADDRESS == "714 Godwin Ct" &
                                 house_data$CITY == "Raleigh"] <- "NC"
  house_data <- remove_previously_processed_rows(house_data, previously_processed_rows)
  house_data <- house_data[!is.na(house_data$ADDRESS), ]
  rownames(house_data) <- NULL
  url_col <- colnames(house_data)[grep(pattern = "^URL", colnames(house_data), ignore.case = T)]
  
  tmp_df <- data.frame(
    date = rep(NA, nrow(house_data)),
    address = rep(NA, nrow(house_data)),
    city = rep(NA, nrow(house_data)),
    state = rep(NA, nrow(house_data)),
    zip = rep(NA, nrow(house_data)),
    lat = rep(NA, nrow(house_data)),
    lon = rep(NA, nrow(house_data)),
    price = rep(NA, nrow(house_data)),
    property_type = rep(NA, nrow(house_data)),
    sq_ft = rep(NA, nrow(house_data)),
    status = rep(NA, nrow(house_data)),
    walk_score = rep(NA, nrow(house_data)),
    walk_desc = rep(NA, nrow(house_data)),
    bike_score = rep(NA, nrow(house_data)),
    bike_desc = rep(NA, nrow(house_data)),
    transit_score = rep(NA, nrow(house_data)),
    transit_desc = rep(NA, nrow(house_data)),
    redfin_link = rep(NA, nrow(house_data)),
    zillow_link = rep(NA, nrow(house_data)),
    realtor_link = rep(NA, nrow(house_data)),
    stringsAsFactors = F
  )
  # i <- 1
  if(nrow(house_data) > 0) {
    for(i in 1:nrow(house_data)) {
      
      #############################################
      # Setting up the Redfin Walkscore API query #
      #############################################
      
      address <- paste(house_data$ADDRESS[i], house_data$CITY[i], house_data$STATE.OR.PROVINCE[i])
      # address <- "840 Wallridge Dr, Wake Forest, NC 27587"
      address <- gsub(gsub(address, pattern = " ", replacement = "%20"), pattern = ",", replacement = "")
      lon <- house_data$LONGITUDE[i]
      lat <- house_data$LATITUDE[i]
      date <- strsplit(x = file, split = "_")[[1]][2]
      date <- as.Date(paste0(strsplit(x = date, split = "-")[[1]][1:3], collapse = "-"))
      
      ####################################################################
      # Skip Walkscore API query if the information is already available #
      ####################################################################
      
      prev_house_rows <- previously_processed_rows[previously_processed_rows$address == house_data$ADDRESS[i] &
                                                     previously_processed_rows$city == house_data$CITY[i] &
                                                     previously_processed_rows$state == house_data$STATE.OR.PROVINCE[i], ]
      prev_house_rows <- prev_house_rows[!is.na(prev_house_rows$address), ]
      
      if(nrow(prev_house_rows) == 0) {
        
        ###########################################################
        # Information not available; Query Redfin's Walkscore API #
        ###########################################################
        
        lat_lon <- paste0("&lat=", lat, "&lon=", lon)
        scores <- "&transit=1&bike=1"
        walkscore_apikey <- paste0("&wsapikey=", walkscore_api_key)
        walkscore_query <- paste0(walkscore_api_query_link, address, lat_lon, scores, walkscore_apikey)
        walkscore_results <- httr::GET(walkscore_query)
        walkscore_list_results <- jsonlite::fromJSON(content(walkscore_results, as = "text"))
        
        tryCatch({
          walk_score <<- ifelse(is.null(walkscore_list_results$walkscore), NA, walkscore_list_results$walkscore)
        }, error = function(e) {
          walk_score <<- NA
        })
        tryCatch({
          walk_desc <<- ifelse(is.null(walkscore_list_results$description), NA, walkscore_list_results$description)
        }, error = function(e) {
          walk_desc <<- NA
        })
        
        tryCatch({
          bike <<- walkscore_list_results$bike
          tryCatch({
            bike_score <<- ifelse(is.null(bike$score), NA, bike$score)
          }, error = function(e) {
            bike_score <<- NA
          })
          tryCatch({
            bike_desc <<- ifelse(is.null(bike$description), NA, bike$description)
          }, error = function(e) {
            bike_desc <<- NA
          })
        }, error = function(e) {
          bike_score <<- NA
          bike_desc <<- NA
        })
        
        tryCatch({
          transit <<- walkscore_list_results$transit
          tryCatch({
            transit_score <<- ifelse(is.null(transit$score), NA, transit$score)
          }, error = function(e) {
            transit_score <<- NA
          })
          tryCatch({
            transit_desc <<- ifelse(is.null(transit$description), NA, transit$description)
          }, error = function(e) {
            transit_desc <<- NA
          })
        }, error = function(e) {
          transit_desc <<- NA
          transit_desc <<- NA
        })
      } else {
        
        ##############################################################
        # Avoids calling the API if the address was already searched #
        ##############################################################
        
        latest_row <<- previously_processed_rows[nrow(previously_processed_rows), ]
        walk_score <<- latest_row$walk_score
        walk_desc <<- latest_row$walk_desc
        bike_score <<- latest_row$bike_score
        bike_desc <<- latest_row$bike_desc
        transit_score <<- latest_row$transit_score
        transit_desc <<- latest_row$transit_desc
      }
      
      ###################################################
      # Gathering all the relevant information together #
      ###################################################
      
      tmp_df$date[i] <- as.Date(date)
      tmp_df$address[i] <- house_data$ADDRESS[i]
      tmp_df$city[i] <- house_data$CITY[i]
      tmp_df$state[i] <- house_data$STATE.OR.PROVINCE[i]
      tmp_df$zip[i] <- house_data$ZIP.OR.POSTAL.CODE[i]
      tmp_df$lat[i] <- house_data$LATITUDE[i]
      tmp_df$lon[i] <- house_data$LONGITUDE[i]
      tmp_df$price[i] <- house_data$PRICE[i]
      tmp_df$property_type[i] <- house_data$PROPERTY.TYPE[i]
      tmp_df$sq_ft[i] <- house_data$SQUARE.FEET[i]
      tmp_df$status[i] <- house_data$STATUS[i]
      tmp_df$walk_score[i] <- walk_score
      tmp_df$walk_desc[i] <- walk_desc
      tmp_df$bike_score[i] <- bike_score
      tmp_df$bike_desc[i] <- bike_desc
      tmp_df$transit_score[i] <- transit_score
      tmp_df$transit_desc[i] <- transit_desc
      tmp_df$redfin_link[i] <- house_data[i, url_col]
    }
  }
  
  ############################################################
  # Appending new information into previously_processed_rows #
  ############################################################
  
  tmp_df$date <- as.Date(tmp_df$date, origin = "1970-01-01")
  previously_processed_rows <- plyr::rbind.fill(previously_processed_rows, tmp_df)
}

######################################################
# Removing redundant rows and saving the information #
######################################################

previously_processed_rows <- previously_processed_rows[!is.na(previously_processed_rows$address), ]
rownames(previously_processed_rows) <- NULL

##################################
# Data corrections - if required #
##################################

previously_processed_rows$state[previously_processed_rows$address == "714 Godwin Ct" &
                                  previously_processed_rows$city == "Raleigh"] <- "NC"
previously_processed_rows <- unique(previously_processed_rows)
saveRDS(previously_processed_rows, "previously_processed_rows.Rds")

##################
# Postprocessing #
##################

source("utils.R")

previously_processed_rows$zip <- substr(previously_processed_rows$zip, start = 1, stop = 5)
previously_processed_rows <- previously_processed_rows[
  order(previously_processed_rows$address,
        previously_processed_rows$city,
        previously_processed_rows$state,
        previously_processed_rows$zip,
        -as.integer(previously_processed_rows$date)), ]
previously_processed_rows <- previously_processed_rows[l
  !is.na(previously_processed_rows$lon) &
    !is.na(previously_processed_rows$lat),
]

#####################################################
# Getting full history by address, city, state, zip #
#####################################################

nested_ <- previously_processed_rows %>% group_by(address, city, state, zip) %>% group_nest()
popup_df <- lapply(
  nested_$data,
  function(df) table_to_popup_df(df))
popup_df <- sapply(popup_df, function(rows) {
  x <- "<table><tbody><tr><td><b>Date</b></td><td><b>Price</b></td><td><b>Status</b></td></tr>"
  z <- "</tbody></table>"
  paste0(x, paste0(rows, collapse = ""), z)
})

latest_info <- data.frame(
  previously_processed_rows %>% group_by(address, city, state, zip) %>%
    arrange(-as.integer(date)) %>% slice(1),
  stringsAsFactors = F)

latest_info$popup_df <- unlist(popup_df)
latest_info <- latest_info[(latest_info$address != "") & (latest_info$city != "") & (latest_info$state != ""), ]
rownames(latest_info) <- NULL

##########################
# Appending nearest city #
##########################

nearest_city_df <- read.xlsx(xlsxFile = "House.xlsx", sheet = "City")
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "City")] <- "city"
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "State")] <- "state"
for(i in 1:nrow(nearest_city_df)) {
  if(is.na(nearest_city_df$state[i]))
    nearest_city_df$state[i] <- nearest_city_df$state[i - 1]
}
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "Nearest.Big.City")] <- "nearest_big_city"
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "Crime.City")] <- "crime_city"
# This should be NULL! Else fill the Google sheet manually
x <- setdiff(latest_info$city, nearest_city_df$city)
print(x)

# This can have a non-NULL value
setdiff(nearest_city_df$city, latest_info$city)

#########################################################
# Save only if nearest big city is appended to each row #
#########################################################

x <- anti_join(latest_info, nearest_city_df[, c("city", "state", "nearest_big_city")])
print(unique(x[, c("city", "state")]))
if(nrow(x) == 0) {
  latest_info <- merge(latest_info, nearest_city_df[, c("city", "state", "nearest_big_city", "crime_city")])
  saveRDS(latest_info, "latest_info.Rds")
}
