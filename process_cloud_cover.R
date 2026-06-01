library(httr)
library(jsonlite)
library(anytime)

lat_pitt <- 40.44; lon_pitt <- -79.99  # Pittsburgh
lat_bos <- 42.3555; lon_bos <- -71.0565  # Boston
lat_sea <- 47.6061; lon_sea <- -122.3328  # Seattle
get_df <- function(lat_sea, lon_sea) {
  resp <- GET("https://archive-api.open-meteo.com/v1/archive",
              query=list(latitude=lat_sea, longitude=lon_sea,
                         start_date="2000-01-01",
                         end_date="2025-06-27",
                         hourly="cloudcover"))
  
  df_sea <- fromJSON(content(resp, "text"))$hourly
  df_sea$time <- anytime::anytime(gsub(pattern = "T", replacement = " ", df_sea$time))
  df_sea$hour <- hour(df_sea$time)
  df_sea <- data.frame(df_sea)
  daytime_sea <- df_sea[(df_sea$hour>=8) & (df_sea$hour<=19), ]
  lst <- list()
  lst[["daytime"]] <- daytime_sea
  lst[["all"]] <- df_sea
  return(lst)
}
lst_sea <- get_df(lat_sea, lon_sea)
daytime_sea <- lst_sea[["daytime"]]
lst_bos <- get_df(lat_bos, lon_bos)
daytime_bos <- lst_bos[["daytime"]]
lst_pitt <- get_df(lat_pitt, lon_pitt)
daytime_pitt <- lst_pitt[["daytime"]]

sapply(0:10, function(i) mean(daytime_sea$cloudcover>=(10*i), na.rm = T))
sapply(0:10, function(i) mean(daytime_pitt$cloudcover>=(10*i), na.rm = T))
sapply(0:10, function(i) mean(daytime_bos$cloudcover>=(10*i), na.rm = T))
