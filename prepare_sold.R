library(geosphere)
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)

latest_info_without_sold <- readRDS("latest_info_without_sold.Rds")
sold_places <- readRDS("sold_places.Rds")
df1 <- latest_info_without_sold[latest_info_without_sold$nearest_big_city == "Pittsburgh",
                                c("address", "city", "state", "zip", "lat", "lon")]
rownames(df1) <- NULL
df2 <- sold_places[sold_places$nearest_big_city == "Pittsburgh",
                   c("ADDRESS", "CITY", "STATE.OR.PROVINCE", "ZIP.OR.POSTAL.CODE", "LATITUDE", "LONGITUDE")]
rownames(df2) <- NULL
df1$full_address <- apply(df1[, c("address", "city", "state", "zip")],
                          FUN = function(x) paste0(x, collapse = " - "), MARGIN = 1)
df2$full_address <- apply(df2[, c("ADDRESS", "CITY", "STATE.OR.PROVINCE", "ZIP.OR.POSTAL.CODE")],
                          FUN = function(x) paste0(x, collapse = " - "), MARGIN = 1)
df1 <- data.frame(df1 %>% group_by(full_address) %>% filter(row_number()==1))
df2 <- data.frame(df2 %>% group_by(full_address) %>% filter(row_number()==1))
distance_pairs <- readRDS("distance_pairs.Rds")[, c("address1", "address2", "distance")]
# Old
uniq1 <- unique(distance_pairs$address1)
uniq2 <- unique(distance_pairs$address2)

# New
new1 <- setdiff(df1$full_address, uniq1)
new2 <- setdiff(df2$full_address, uniq2)

# New
new_df1 <- df1[sapply(df1$full_address, function(x) x %in% new1), ]
new_df2 <- df2[sapply(df2$full_address, function(x) x %in% new2), ]
rownames(new_df1) <- rownames(new_df2) <- NULL

# Old
df1 <- df1[sapply(df1$full_address, function(x) x %in% uniq1), ]
df2 <- df2[sapply(df2$full_address, function(x) x %in% uniq2), ]
rownames(df1) <- rownames(df2) <- NULL

rws <- nrow(new_df1)*nrow(df2) + nrow(new_df2)*nrow(df1) + nrow(new_df1)*nrow(new_df2)

if(rws > 0) {
  new_distance_pairs <- data.frame(address1 = rep("", rws),
                                   address2 = rep("", rws),
                                   distance = rep(0, rws),
                                   stringsAsFactors = F)
  row_num <- 1
  # for(i in 1:nrow(df1)) {
  #   for(j in 1:nrow(df2)) {
  # while(row_num <= rws) {
  #   i <- as.integer(row_num/nrow(df2))
  #   j <- row_num - i*nrow(df2)
  #   if(j != 0) {
  #     i <- i + 1
  #   } else {
  #     j <- nrow(df2)
  #   }
  #   print(c(i, j))
  #   dist_calc <- distm(c(df1$lat[i], df1$lon[i]), c(df2$LATITUDE[j], df2$LONGITUDE[j]), distHaversine)
  #   new_distance_pairs$address1[row_num] <- df1$full_address[i]
  #   new_distance_pairs$address2[row_num] <- df2$full_address[j]
  #   new_distance_pairs$distance[row_num] <- dist_calc
  #   row_num <- row_num + 1
  # }
  #   }
  # }
  
  if(nrow(new_df1)>0) {
  for(j in 1:nrow(new_df1)) {
    print(c(j, nrow(new_df1)))
    for(i in 1:nrow(df2)) {
      new_distance_pairs$address1[row_num] <- new_df1$full_address[j]
      new_distance_pairs$address2[row_num] <- df2$full_address[i]
      dist_calc <- distm(c(new_df1$lat[j], new_df1$lon[j]), c(df2$LATITUDE[i], df2$LONGITUDE[i]), distHaversine)
      new_distance_pairs$distance[row_num] <- dist_calc
      row_num <- row_num + 1
    }
  }
  }
  
  if(nrow(new_df2)>0) {
  for(j in 1:nrow(new_df2)) {
    print(c(j, nrow(new_df2)))
    for(i in 1:nrow(df1)) {
      new_distance_pairs$address2[row_num] <- new_df2$full_address[j]
      new_distance_pairs$address1[row_num] <- df1$full_address[i]
      dist_calc <- distm(c(new_df2$LATITUDE[j], new_df2$LONGITUDE[j]), c(df1$lat[i], df1$lon[i]), distHaversine)
      new_distance_pairs$distance[row_num] <- dist_calc
      row_num <- row_num + 1
    }
  }
  }
  
  if((nrow(new_df1)>0) & (nrow(new_df2)>0)) {
  for(i in 1:nrow(new_df1)) {
    print(c(i, nrow(new_df1)))
    for(j in 1:nrow(new_df2)) {
      new_distance_pairs$address2[row_num] <- new_df2$full_address[j]
      new_distance_pairs$address1[row_num] <- new_df1$full_address[i]
      dist_calc <- distm(c(new_df2$LATITUDE[j], new_df2$LONGITUDE[j]), c(new_df1$lat[i], new_df1$lon[i]), distHaversine)
      new_distance_pairs$distance[row_num] <- dist_calc
      row_num <- row_num + 1
    }
  }
  }
  
  distance_pairs <- rbind.fill(distance_pairs, new_distance_pairs)
  distance_pairs <- distance_pairs[order(distance_pairs$address1, distance_pairs$distance), ]
  rownames(distance_pairs) <- NULL
  saveRDS(distance_pairs, "distance_pairs.Rds")
}

latest_info <- readRDS("latest_info.Rds")
latest_info$address1 <- apply(latest_info[, c("address", "city", "state", "zip")],
                              FUN = function(x) paste0(x, collapse = " - "), MARGIN = 1)

df1 <- data.frame(address1 = unique(distance_pairs$address1))
df1_1 <- unique(latest_info[, c("address1", "lat", "lon", "price", "property_type", "sq_ft", "status", "walk_score", "bike_score", "transit_score", "popup_df")])
df1 <- merge(df1, df1_1)

df2 <- data.frame(address2 = unique(distance_pairs$address2))
sold_places$address2 <- apply(sold_places[, c("ADDRESS", "CITY", "STATE.OR.PROVINCE", "ZIP.OR.POSTAL.CODE")],
                              FUN = function(x) paste0(x, collapse = " - "), MARGIN = 1)
df2_1 <- unique(sold_places[, c("address2", "SALE.TYPE", "SOLD.DATE", "PROPERTY.TYPE", "PRICE", "BEDS", "BATHS", "LOCATION", "SQUARE.FEET", "X..SQUARE.FEET", "HOA.MONTH", "LATITUDE", "LONGITUDE")])
df2 <- merge(df2, df2_1)

x_df <- data.frame(sold_places %>% group_by(address2) %>% summarize(n = n()))
table(x_df$n)
dup_df <- merge(sold_places, x_df[x_df$n>1, ])

get_col_diff <- function(x_df) {
  cols <- colnames(x_df)
  row1 <- x_df[1, ]
  other_df <- x_df[-c(1), ]
  diff_cols <- c()
  for(col in cols) {
    if(is.na(row1[, col])) {
      if(sum(!is.na(other_df[, col]))>0) {
        diff_cols <- c(diff_cols, col)
      }
    }
    else if(sum(is.na(other_df[, col])) > 0) {
      diff_cols <- c(diff_cols, col)
    }
    else if(sum(other_df[, col] != row1[, col]) > 0) {
      diff_cols <- c(diff_cols, col)
    }
  }
  return(data.frame(address2 = row1$address2[1], col_diffs = paste0(diff_cols, collapse = ", ")))
}

# data.frame(dup_df) %>% group_by(address2) %>% summarize(col_diffs = get_col_diff(SALE.TYPE, SOLD.DATE, PROPERTY.TYPE, PRICE, BED, BATHS, SQUARE.FEET, LOT.SIZE, X..SQUARE.FEET, HOA.MONTH, STATUS, URL..SEE.https...www.redfin.com.buy.a.home.comparative.market.analysis.FOR.INFO.ON.PRICING., LATITUDE, LONGITUDE))

dup_summary_df <- rbind.fill(dup_df[, c("address2", "SALE.TYPE", "SOLD.DATE", "PROPERTY.TYPE", "PRICE", "BEDS", "BATHS", "SQUARE.FEET", "LOT.SIZE", "X..SQUARE.FEET", "HOA.MONTH", "STATUS", "URL..SEE.https...www.redfin.com.buy.a.home.comparative.market.analysis.FOR.INFO.ON.PRICING.", "LATITUDE", "LONGITUDE")] %>%
  group_by(address2) %>% group_map(~ get_col_diff(.x), .keep = T))

sold_places <- merge(sold_places, x_df[x_df$n==1, ])
dup_df <- merge(dup_df, dup_summary_df[dup_summary_df$col_diffs %in% c("LATITUDE, LONGITUDE", "LONGITUDE, LATITUDE", ""), ])
dup_df <- dup_df %>% group_by(address2) %>% filter(row_number() == 1)
dup_df <- dup_df[, colnames(sold_places)]
sold_places <- rbind.fill(sold_places, dup_df)
saveRDS(sold_places, "sold_places.Rds")

df2_1 <- unique(sold_places[, c("address2", "SALE.TYPE", "SOLD.DATE", "PROPERTY.TYPE", "PRICE", "BEDS", "BATHS", "LOCATION", "SQUARE.FEET", "X..SQUARE.FEET", "HOA.MONTH", "LATITUDE", "LONGITUDE")])
df2 <- merge(df2, df2_1)

distance_pairs_with_lat_long <- merge(distance_pairs, df1)
distance_pairs_with_lat_long <- merge(distance_pairs_with_lat_long, df2)
distance_pairs_with_lat_long <- distance_pairs_with_lat_long[order(distance_pairs_with_lat_long$address1, distance_pairs_with_lat_long$distance), ]
rownames(distance_pairs_with_lat_long) <- NULL

saveRDS(distance_pairs_with_lat_long, "distance_pairs.Rds")
