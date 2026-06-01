library(data.table)
library(anytime)
library(plyr)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(broom)
library(plotly)
library(quantreg)

dirs <- list.dirs("Sold/Pittsburgh/", recursive = T)
files <- strsplit(dirs, "/")
dirs <- dirs[sapply(files, length) == 4]
cities <- sapply(strsplit(dirs, "/"), function(x) x[2])
city_lst <- list()

for(i in 1:length(cities)) {
  df <- data.frame()
  city <- cities[i]
  dir <- dirs[i]
  location <- strsplit(dir, "/")[[1]][4]
  files <- list.files(dir, full.names = T)
  for(j in 1:length(files)) {
    file <- files[j]
    tmp_df <- fread(file)
    cols <- read.csv(file, nrows = 1)
    colnames(tmp_df) <- colnames(cols)
    if(j == 1) {
      df <- tmp_df
    } else {
      df <- rbind.fill(df, tmp_df)
    }
  }
  df$LOCATION1 <- location
  df$nearest_big_city <- city
  tryCatch({
    df$SOLD.DATE <- anydate(df$SOLD.DATE)
  }, error = function(e) {
    df$SOLD.DATE <- as.Date(NA)
  })
  df <- unique(df)
  df <- df[!is.na(df$SOLD.DATE) | !is.na(df$PRICE), ]
  df$non_nulls <- apply(X = df[, c("SOLD.DATE", "PRICE")], MARGIN = 1, FUN = function(x) sum(!is.na(x)))
  df <- df[order(df$ADDRESS, df$CITY, df$STATE.OR.PROVINCE, df$non_nulls), ]
  df <- data.frame(df %>% group_by(ADDRESS, CITY, STATE.OR.PROVINCE, SOLD.DATE) %>% slice_max(order_by = non_nulls, n = 1))
  tryCatch({
    city_lst[[city]] <- rbind.fill(city_lst[[city]], df)
    city_lst[[city]] <- data.frame(city_lst[[city]] %>% group_by(ADDRESS, CITY, STATE.OR.PROVINCE, SOLD.DATE) %>% slice_max(order_by = non_nulls, n = 1))
  }, error = function(e) {
    city_lst[[city]] <- df
  })
}

nrow(city_lst[[city]])

sold_places <- unique(rbind.fill(city_lst))
nearest_city_df <- read.xlsx(xlsxFile = "House.xlsx", sheet = "City")
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "City")] <- "CITY"
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "State")] <- "STATE.OR.PROVINCE"
for(i in 1:nrow(nearest_city_df)) {
  if(is.na(nearest_city_df$STATE.OR.PROVINCE[i]))
    nearest_city_df$STATE.OR.PROVINCE[i] <- nearest_city_df$STATE.OR.PROVINCE[i - 1]
}
colnames(nearest_city_df)[which(colnames(nearest_city_df) == "Nearest.Big.City")] <- "nearest_big_city"

x <- anti_join(sold_places[, c("CITY", "STATE.OR.PROVINCE")], nearest_city_df[, c("CITY", "STATE.OR.PROVINCE")])
# x <- anti_join(nearest_city_df[, c("CITY", "STATE.OR.PROVINCE")], sold_places[, c("CITY", "STATE.OR.PROVINCE")])

latest_info <- readRDS("latest_info.Rds")
latest_info_without_sold <- latest_info[latest_info$status != "Sold", ]
rownames(latest_info_without_sold) <- NULL

removes <- c()
for(i in 1:nrow(latest_info_without_sold)) {
  matched_row <- sold_places[(sold_places$CITY == latest_info_without_sold$city[i]) &
                               (sold_places$STATE.OR.PROVINCE == latest_info_without_sold$state[i]) & 
                               (sold_places$ADDRESS == latest_info_without_sold$address[i]), ]
  if(nrow(matched_row) > 0) {
    removes <- c(removes, i)
  }
}

latest_info_without_sold <- latest_info_without_sold[-removes, ]
rownames(latest_info_without_sold) <- NULL
saveRDS(latest_info_without_sold, "latest_info_without_sold.Rds")
saveRDS(sold_places, "sold_places.Rds")








############
# Analysis #
############

city_lst[[city]]$PRICE_PER_SQ_FT <- city_lst[[city]]$PRICE/city_lst[[city]]$SQUARE.FEET
city_lst[[city]]$SOLD.MONTH <- city_lst[[city]]$SOLD.DATE - as.integer(format(city_lst[[city]]$SOLD.DATE, format = "%d"))+1
city_lst[[city]]$CITY[city_lst[[city]]$CITY == "Mount Washington"] <- "Mt Washington"
city_lst[[city]]$MONTH <- as.integer((month(city_lst[[city]]$SOLD.MONTH)-2)/3)*3 + 1
city_lst[[city]]$YEAR <- year(city_lst[[city]]$SOLD.MONTH)
city_lst[[city]]$Quarter <- as.Date(paste0(city_lst[[city]]$YEAR, "-", city_lst[[city]]$MONTH, "-1"))
city_lst[[city]]$Quarter_month <- month(city_lst[[city]]$Quarter)

table(city_lst[[city]]$Quarter_month[(city_lst[[city]]$YEAR<max(city_lst[[city]]$YEAR, na.rm = T)) & (city_lst[[city]]$YEAR>min(city_lst[[city]]$YEAR, na.rm = T))])

###################
# Monthly medians #
###################

plot_df <- data.frame(city_lst[[city]] %>% dplyr::group_by(LOCATION1, SOLD.MONTH) %>%
                        dplyr::summarize(med_sq_ft = median(PRICE_PER_SQ_FT, na.rm = T),
                                         sample_size = length(PRICE_PER_SQ_FT[!is.na(PRICE_PER_SQ_FT)])))
plot_df <- plot_df[order(plot_df$SOLD.MONTH, plot_df$LOCATION1, plot_df$med_sq_ft), ]
rownames(plot_df) <- NULL
ggplotly(ggplot(data = plot_df, aes(x=SOLD.MONTH, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))) + geom_point(), tooltip = c("text"))
ggplotly(ggplot(data = plot_df) + geom_line(aes(x=SOLD.MONTH, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))))

x_df <- plot_df %>% dplyr::group_by(LOCATION1, SOLD.MONTH) %>% dplyr::summarize(count = length(sample_size))
x_df[x_df$count != 1, ]

#####################
# Quarterly medians #
#####################

plot_df1 <- data.frame(city_lst[[city]] %>% dplyr::group_by(LOCATION1, Quarter) %>%
  dplyr::summarize(med_sq_ft = median(PRICE_PER_SQ_FT, na.rm = T),
                            sample_size = length(PRICE_PER_SQ_FT)))
plot_df1 <- plot_df1[order(plot_df1$Quarter, plot_df1$LOCATION1, plot_df1$med_sq_ft), ]
plot_df1 <- plot_df1[plot_df1$sample_size>=10, ]
rownames(plot_df1) <- NULL

x_df <- plot_df1 %>% dplyr::group_by(LOCATION1, Quarter) %>% dplyr::summarize(count = length(sample_size))
x_df[x_df$count != 1, ]

ggplotly(ggplot(data = plot_df1, aes(x=Quarter, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))) + geom_point(), tooltip = c("text"))
ggplotly(ggplot(data = plot_df1[order(plot_df1$LOCATION1, plot_df1$Quarter), ], aes(x=Quarter, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))) + geom_line())

ggplotly(ggplot(data = city_lst[[city]][city_lst[[city]]$LOCATION1 == "Swissvale", ], aes(x = factor(Quarter), y = PRICE_PER_SQ_FT)) + geom_violin())
ggplotly(ggplot(data = city_lst[[city]][city_lst[[city]]$LOCATION1 == "Edgewood", ], aes(x = factor(Quarter), y = PRICE_PER_SQ_FT)) + geom_violin())

condition <- city_lst[[city]]$LOCATION1 == "Edgewood"
ggplotly(ggplot() + geom_histogram(data = city_lst[[city]][condition, ], aes(x = PRICE_PER_SQ_FT)) + geom_vline(xintercept = median(city_lst[[city]]$PRICE_PER_SQ_FT[condition], na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.75, na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.25, na.rm = T)))
condition <- city_lst[[city]]$LOCATION1 == "Edgewood" & city_lst[[city]]$SOLD.DATE>=as.Date("2024-01-01")
ggplotly(ggplot() + geom_histogram(data = city_lst[[city]][condition, ], aes(x = PRICE_PER_SQ_FT)) + geom_vline(xintercept = median(city_lst[[city]]$PRICE_PER_SQ_FT[condition], na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.75, na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.25, na.rm = T)))
condition <- city_lst[[city]]$LOCATION1 == "Edgewood" & city_lst[[city]]$SOLD.DATE>=as.Date("2025-01-01")
ggplotly(ggplot() + geom_histogram(data = city_lst[[city]][condition, ], aes(x = PRICE_PER_SQ_FT)) + geom_vline(xintercept = median(city_lst[[city]]$PRICE_PER_SQ_FT[condition], na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.75, na.rm = T)) + geom_vline(xintercept = quantile(city_lst[[city]]$PRICE_PER_SQ_FT[condition], 0.25, na.rm = T)))
######################################
# Quarterly medians (confirmed only) #
######################################

city_lst[[city]]$confirmed <- apply(city_lst[[city]], MARGIN = 1, FUN = function(x) {
  grepl(pattern = x["LOCATION1"], x = x["LOCATION"], ignore.case = T)
})

plot_df2 <- data.frame(city_lst[[city]][city_lst[[city]]$confirmed, ] %>% dplyr::group_by(LOCATION1, Quarter) %>%
                         dplyr::summarize(med_sq_ft = median(PRICE_PER_SQ_FT, na.rm = T),
                                          sample_size = length(PRICE_PER_SQ_FT)))
plot_df2 <- plot_df2[order(plot_df2$Quarter, plot_df2$LOCATION1, plot_df2$med_sq_ft), ]
rownames(plot_df2) <- NULL

x_df <- plot_df2 %>% dplyr::group_by(LOCATION1, Quarter) %>% dplyr::summarize(count = length(sample_size))
x_df[x_df$count != 1, ]

ggplotly(ggplot(data = plot_df2, aes(x=Quarter, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))) + geom_point(), tooltip = c("text"))
ggplotly(ggplot(data = plot_df2[order(plot_df1$LOCATION1, plot_df1$Quarter), ], aes(x=Quarter, y = med_sq_ft, color = LOCATION1, group = LOCATION1, text = paste('Sample size: ', sample_size))) + geom_line())


# Market report URL: https://rocket.com/homes/market-reports/pa/pittsburgh; https://www.movoto.com/pittsburgh-pa/market-trends/

library(shiny)
library(shinydashboard)

city_ <- unique(city_lst[[city]]$LOCATION1)
city <- "Pittsburgh"

ui <- dashboardPage(
  dashboardHeader(title = "Price per sq ft"),
  dashboardSidebar(
    selectInput(
      inputId = "city_dropdown",
      label = "Select city:",
      choices = city_,
      selected = NA
    )#,
    # actionButton(
    #   inputId = "build_model",
    #   label = "Analyze"
    # )
  ),
  dashboardBody(
    plotlyOutput("city_violin_plot"),
    h1("Linear regression:"),
    tableOutput("reg_table"),
    h1("Quantile regression 0.25:"),
    tableOutput("qr_table_025"),
    h1("Quantile regression 0.5:"),
    tableOutput("qr_table_05"),
    h1("Quantile regression 0.75:"),
    tableOutput("qr_table_075")
  )
)

median_ <- function(x) {
  return(median(x, na.rm = T))
}

get_formula <- function(df1) {
  quarters <- unique(df1$Quarter_month)
  years <- unique(df1$YEAR)
  quarter_year <- unique(df1$Quarter)
  # if(nrow(df1) > length(quarters) + length(years) + length(quarter_year)) {
  #   formula <- "PRICE_PER_SQ_FT ~ factor(Quarter_month) + factor(YEAR) + factor(Quarter)"
  # } else {
  if(nrow(df1) > length(quarters) + length(years)) {
    formula <- "PRICE_PER_SQ_FT ~ factor(Quarter_month) + factor(YEAR)"
  } else if(nrow(df1) > length(years)) {
    formula <- "PRICE_PER_SQ_FT ~ factor(YEAR)"
  } else {
    formula <- "PRICE_PER_SQ_FT ~ 1"
  }
  # }
  return(formula)
}

server <- function(input, output, session) {
  output$city_violin_plot <- renderPlotly({
    if(is.null(input$city_dropdown)) {
      df1 <- data.frame(city_lst[[city]])
    } else {
      df1 <- data.frame(city_lst[[city]])
      df1 <- df1[df1$LOCATION1 == input$city_dropdown, ]
      rownames(df1) <- NULL
    }
    summary_df1 <- data.frame(df1 %>% dplyr::group_by(Quarter) %>% dplyr::summarize(med_price_per_sq_ft = median(PRICE_PER_SQ_FT, na.rm = T)))
    summary_df1$Quarter <- as.character(summary_df1$Quarter)
    df1$Quarter <- as.character(df1$Quarter)
    # p <- ggplot(data = df1, aes(x = Quarter, y = PRICE_PER_SQ_FT)) + geom_violin()# +
      # geom_boxplot(width=.1)# +
    # stat_summary(
    #   fun = median_,
    #   geom = 'line',
    #   # aes(group = Quarter),
    #   position = position_dodge(width = 0.85) 
    # )
    # return(p)
    fig <- df1 %>% plot_ly(x = ~Quarter, y = ~PRICE_PER_SQ_FT, type = 'violin', box = list(visible = T), meanline = list(visible = T))
  })
  
  get_df1 <- reactive({
    if(is.null(input$city_dropdown)) {
      df1 <- data.frame(city_lst[[city]])
    } else {
      df1 <- data.frame(city_lst[[city]])
      df1 <- df1[df1$LOCATION1 == input$city_dropdown, ]
      # df1 <- df1[(df1$YEAR > min(df1$YEAR, na.rm = T)) & (df1$YEAR < max(df1$YEAR, na.rm = T)), ]
      # rownames(df1) <- NULL
    }
    df1 <- df1[!is.na(df1$PRICE_PER_SQ_FT), ]
    df1 <- df1[!is.na(df1$SOLD.DATE), ]
    df1 <- df1[(df1$YEAR > min(df1$YEAR, na.rm = T)) & (df1$YEAR < max(df1$YEAR, na.rm = T)), ]
    rownames(df1) <- NULL
    return(df1)
  })
  
  observeEvent(input$city_dropdown, {
    df1 <- get_df1()
    formula <- get_formula(df1)
    
    output$reg_table <- renderTable({
      model <- lm(formula = formula, data = df1)
      tidy(model)
    })
    
    output$qr_table_025 <- renderTable({
      rqfit_0_25 <- rq(formula = formula, data = df1, tau = 0.25)
      tidy(rqfit_0_25)
    })
    
    output$qr_table_05 <- renderTable({
      rqfit_0_5 <- rq(formula = formula, data = df1, tau = 0.5)
      tidy(rqfit_0_5)
    })
    
    output$qr_table_075 <- renderTable({
      rqfit_0_75 <- rq(formula = formula, data = df1, tau = 0.75)
      tidy(rqfit_0_75)
    })
  })
}

shinyApp(ui, server)



# 
# 
# 
# 
# library(RJSONIO)
# library(plyr)
# 
# get_ <- function(i, lastSalePrice) {
#   sash_type_name <- sashTypeName[[i]]
#   last_sales_date <- lastSalePrice[[i]]
#   last_sales_date <- last_sales_date[tolower(sash_type_name) == "sold"]
#   if(length(last_sales_date) == 0) {
#     return("")
#   }
#   return(last_sales_date)
# }
# 
# null_with_empty_str <- function(x) {
#   if(length(x) == 0) {
#     return("")
#   } else if(is.na(x)) {
#     return("")
#   } else {
#     return(x)
#   }
# }
# 
# folder <- "Zillow/Sold/Garfield/"
# files <- list.files(folder, pattern = ".json")
# out_df <- data.frame()
# for(file in files) {
#   df <- RJSONIO::fromJSON(gsub(pattern = "\\{\\}&&", replacement = "", readLines(paste0(folder, file))))
#   df <- df[["payload"]][["homes"]]
#   lastSalesDate <- sapply(df, function(x) {
#     sapply(1:length(x$sashes), function(i) x$sashes[[i]]$lastSaleDate)
#   })
#   lastSalePrice <- sapply(df, function(x) {
#     sapply(1:length(x$sashes), function(i) x$sashes[[i]]$lastSalePrice)
#   })
#   sashTypeName <- sapply(df, function(x) {
#     sapply(1:length(x$sashes), function(i) x$sashes[[i]]$sashTypeName)
#   })
#   lastSalesDate <- sapply(1:length(sashTypeName), function(i) {
#     sash_type_name <- sashTypeName[[i]]
#     last_sales_date <- lastSalesDate[[i]]
#     last_sales_date <- last_sales_date[tolower(sash_type_name) == "sold"]
#     if(length(last_sales_date) == 0) {
#       return("")
#     }
#     return(last_sales_date)
#   })
#   soldDate <- sapply(df, function(x) {
#     if(length(x$soldDate)>0) {
#       return(x$soldDate)
#     } else {
#       return(NA)
#     }
#   })
#   
#   lastSalePrice <- as.numeric(sapply(1:length(sashTypeName), function(i) get_(i, lastSalePrice)))
#   lastSalesDate <- anytime::anydate(sapply(1:length(sashTypeName), function(i) get_(i, lastSalesDate)))
#   mlsStatus <- sapply(df, function(x) null_with_empty_str(x$mlsStatus))
#   latitude <- as.numeric(sapply(df, function(x) x$latLong$value['latitude']))
#   longitude <- as.numeric(sapply(df, function(x) x$latLong$value['longitude']))
#   remarks <- sapply(df, function(x) null_with_empty_str(x$listingRemarks))
#   streetLine <- unlist(sapply(df, function(x) null_with_empty_str(x$streetLine['value'])))
#   unitNumber <- unlist(sapply(df, function(x) null_with_empty_str(x$unitNumber['value'])))
#   city <- unlist(sapply(df, function(x) null_with_empty_str(x$city)))
#   state <- unlist(sapply(df, function(x) null_with_empty_str(x$state)))
#   zip <- unlist(sapply(df, function(x) null_with_empty_str(x$zip)))
#   postalCode <- unlist(sapply(df, function(x) null_with_empty_str(x$postalCode['value'])))
#   hoa <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$hoa['value']))))
#   price <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$price['value']))))
#   hideSalePrice <- unlist(sapply(df, function(x) null_with_empty_str(x$hideSalePrice)))
#   sqFt <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$sqFt['value']))))
#   pricePerSqFt <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$pricePerSqFt['value']))))
#   isHoaFrequencyKnown <- unlist(sapply(df, function(x) null_with_empty_str(x$isHoaFrequencyKnown)))
#   lotSize <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$lotSize['value']))))
#   beds <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$beds))))
#   baths <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$baths))))
#   fullBaths <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$fullBaths))))
#   partialBaths <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$partialBaths))))
#   timeOnRedfin <- as.numeric(unlist(sapply(df, function(x) null_with_empty_str(x$timeOnRedfin['value']))))
#   listingType <- unlist(sapply(df, function(x) null_with_empty_str(x$listingType)))
#   propertyId <- unlist(sapply(df, function(x) null_with_empty_str(x$propertyId)))
#   names(lastSalePrice) <- names(mlsStatus) <- names(latitude) <- names(longitude) <-
#     names(remarks) <- names(streetLine) <- names(unitNumber) <- names(city) <-
#     names(state) <- names(zip) <- names(postalCode) <- names(hoa) <- names(hideSalePrice) <-
#     names(sqFt) <- names(pricePerSqFt) <- names(isHoaFrequencyKnown) <- names(lotSize) <- 
#     names(beds) <- names(baths) <- names(fullBaths) <- names(partialBaths) <-
#     names(timeOnRedfin) <- names(lastSalesDate) <- names(propertyId) <- NULL
#   tmp_df <- data.frame(
#     lastSalePrice = lastSalePrice,
#     lastSalesDate = lastSalesDate,
#     mlsStatus = mlsStatus,
#     latitude = latitude,
#     longitude = longitude,
#     remarks = remarks,
#     streetLine = streetLine,
#     unitNumber = unitNumber,
#     city = city,
#     state = state,
#     zip = zip,
#     postalCode = postalCode,
#     hoa = hoa,
#     price = price,
#     hideSalePrice = hideSalePrice,
#     sqFt = sqFt,
#     pricePerSqFt = pricePerSqFt,
#     isHoaFrequencyKnown = isHoaFrequencyKnown,
#     lotSize = lotSize,
#     beds = beds,
#     baths = baths,
#     fullBaths = fullBaths,
#     partialBaths = partialBaths,
#     timeOnRedfin = timeOnRedfin,
#     listingType = listingType,
#     propertyId = propertyId,
#     stringsAsFactors = F
#   )
#   out_df <- rbind.fill(out_df, tmp_df)
# }
# 
# uniq_out_df <- unique(out_df)
