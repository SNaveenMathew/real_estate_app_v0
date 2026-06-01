library(shiny)
library(bs4Dash)
library(leaflet)
library(shinyBS)
library(data.table)
library(plotly)
library(bslib)
library(httr)
library(openxlsx)
library(jsonlite)
library(dplyr)

distance_pairs_with_info <- readRDS("distance_pairs.Rds")
distance_pairs_with_info$address1 <- tolower(distance_pairs_with_info$address1)
distance_pairs_with_info$address2 <- tolower(distance_pairs_with_info$address2)
files <- list.files()
source("utils.R")

if(!("df1.Rds" %in% files)) {
  df1 <- unique(distance_pairs_with_info[, c("address1", "lat", "lon", "price", "property_type", "sq_ft", "status", "walk_score", "bike_score", "transit_score", "popup_df")])
  df1$favorite_flag <- FALSE
  df1$tract1 <- NA
  for(i in 1:nrow(df1)) {
    x <- df1[i, ]
    tryCatch({
      df1$tract1[i] <- jsonlite::fromJSON(content(GET(paste0("https://geo.fcc.gov/api/census/block/find?latitude=", as.character(x$lat[1]), "&longitude=", as.character(x$lon[1]), "&format=json")), "text"))$Block$FIPS
    }, error = function(e) {
      NULL
    })
  }
  df1$id <- 1:nrow(df1)
  df1$tract1_full <- df1$tract1
  df1$tract1 <- substr(df1$tract1, start = 1, stop = 11)
  df1 <- df1 %>% group_by(lat, lon) %>%
    summarize(address1 = process_address(address1),
              price = process_price(price, popup_df),
              property_type = most_frequent(property_type),
              sq_ft = max(sq_ft, na.rm = T),
              status = process_status(status, popup_df),
              walk_score = max(walk_score, na.rm = T),
              bike_score = max(bike_score, na.rm = T),
              transit_score = max(transit_score, na.rm = T),
              popup_df = process_popup(popup_df),
              favorite_flag = max(favorite_flag),
              tract1 = min(tract1),
              id = min(id),
              tract1_full = min(tract1_full))
  df1$price <- as.numeric(df1$price)
  df1$id <- 1:nrow(df1)
  df1 <- df1[, c("address1", "lat", "lon", "price", "property_type", "sq_ft", "status", "walk_score", "bike_score", "transit_score", "popup_df", "favorite_flag", "tract1", "id", "tract1_full")]
  saveRDS(df1, "df1.Rds")
} else {
  df1 <- readRDS("df1.Rds")
  tmp_df1 <- unique(distance_pairs_with_info[, c("address1", "lat", "lon", "price", "property_type", "sq_ft", "status", "walk_score", "bike_score", "transit_score", "popup_df")])
  tmp_df1$price <- as.numeric(tmp_df1$price)
  tmp_df1$sq_ft <- as.numeric(tmp_df1$sq_ft)
  tmp_df1$lat <- as.numeric(tmp_df1$lat)
  tmp_df1$lon <- as.numeric(tmp_df1$lon)
  df1$price <- as.numeric(df1$price)
  df1$sq_ft <- as.numeric(df1$sq_ft)
  df1$lat <- as.numeric(df1$lat)
  df1$lon <- as.numeric(df1$lon)
  new_df1 <- anti_join(tmp_df1, df1[, colnames(tmp_df1)])
  if(nrow(new_df1) > 0) {
    new_df1$favorite_flag <- FALSE
    new_df1$tract1 <- NA
    for(i in 1:nrow(new_df1)) {
      x <- new_df1[i, ]
      tryCatch({
        new_df1$tract1[i] <- jsonlite::fromJSON(content(GET(paste0("https://geo.fcc.gov/api/census/block/find?latitude=", as.character(x$lat[1]), "&longitude=", as.character(x$lon[1]), "&format=json")), "text"))$Block$FIPS
      }, error = function(e) {
        NULL
      })
    }
    new_df1$id <- seq(from = (nrow(df1) + 1), to = (nrow(df1) + nrow(new_df1)), by = 1)
    new_df1$tract1_full <- new_df1$tract1
    new_df1$tract1 <- substr(new_df1$tract1, start = 1, stop = 11)
    df1 <- rbind.fill(df1, new_df1)
  }
  rm(tmp_df1)
  rm(new_df1)
  gc()
  df1 <- df1 %>% group_by(lat, lon) %>%
    summarize(address1 = process_address(address1),
              price = process_price(price, popup_df),
              property_type = most_frequent(property_type),
              sq_ft = max(sq_ft, na.rm = T),
              status = process_status(status, popup_df),
              walk_score = max(walk_score, na.rm = T),
              bike_score = max(bike_score, na.rm = T),
              transit_score = max(transit_score, na.rm = T),
              popup_df = process_popup(popup_df),
              favorite_flag = max(favorite_flag),
              tract1 = min(tract1),
              id = min(id),
              tract1_full = min(tract1_full))
  df1$price <- as.numeric(df1$price)
  df1$id <- 1:nrow(df1)
  df1 <- df1[, c("address1", "lat", "lon", "price", "property_type", "sq_ft", "status", "walk_score", "bike_score", "transit_score", "popup_df", "favorite_flag", "tract1", "id", "tract1_full")]
  saveRDS(df1, "df1.Rds")
}

if(!("df2.Rds" %in% files)) {
  df2 <- unique(distance_pairs_with_info[, c("address2", "SALE.TYPE", "SOLD.DATE", "PROPERTY.TYPE", "PRICE", "BEDS", "BATHS", "LOCATION", "SQUARE.FEET", "X..SQUARE.FEET", "HOA.MONTH", "LATITUDE", "LONGITUDE")])
  df2$tract2 <- NA
  for(i in 1:nrow(df2)) {
    x <- df2[i, ]
    tryCatch({
      df2$tract2[i] <- jsonlite::fromJSON(content(GET(paste0("https://geo.fcc.gov/api/census/block/find?latitude=", as.character(x$LATITUDE[1]), "&longitude=", as.character(x$LONGITUDE[1]), "&format=json")), "text"))$Block$FIPS
    }, error = function(e) {
      NULL
    })
  }
  df2$tract2_full <- df2$tract2
  df2$tract2 <- substr(df2$tract2, start = 1, stop = 11)
  saveRDS(df2, "df2.Rds")
} else {
  df2 <- readRDS("df2.Rds")
  tmp_df2 <- unique(distance_pairs_with_info[, c("address2", "SALE.TYPE", "SOLD.DATE", "PROPERTY.TYPE", "PRICE", "BEDS", "BATHS", "LOCATION", "SQUARE.FEET", "X..SQUARE.FEET", "HOA.MONTH", "LATITUDE", "LONGITUDE")])
  new_df2 <- anti_join(tmp_df2, df2[, colnames(tmp_df2)])
  if(nrow(new_df2) > 0) {
    new_df2$tract2 <- NA
    for(i in 1:nrow(new_df2)) {
        x <- new_df2[i, ]
        tryCatch({
          new_df2$tract2[i] <- jsonlite::fromJSON(content(GET(paste0("https://geo.fcc.gov/api/census/block/find?latitude=", as.character(x$LATITUDE[1]), "&longitude=", as.character(x$LONGITUDE[1]), "&format=json")), "text"))$Block$FIPS
        }, error = function(e) {
          NULL
        })
      }
    new_df2$tract2_full <- new_df2$tract2
    new_df2$tract2 <- substr(new_df2$tract2, start = 1, stop = 11)
    df2 <- rbind.fill(df2, new_df2)
  }
  rm(tmp_df2)
  rm(new_df2)
  gc()
}
rownames(df1) <- rownames(df2) <- NULL
all_tracts <- unique(c(df1$tract1, df2$tract2))

tract_hpi_df <- openxlsx::read.xlsx("C:/Users/dbznf/Downloads/hpi_at_tract.xlsx", sheet = 2)
tract_hpi_df$tract <- as.character(tract_hpi_df$tract)
tract_hpi_df <- tract_hpi_df[sapply(tract_hpi_df$tract, function(x) x %in% all_tracts), ]

red_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41
)

blue_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41
)

heart_icon <- awesomeIcons(
  icon = "heart",         # heart icon for all
  library = "fa",
  iconColor = "red"
)

server <- function(input,output, session) {
  output$map <- renderLeaflet({
    df1_ <- get_data()
    icons <- awesomeIcons(
      icon = ifelse(df1_$favorite_flag, "heart", "Home"),
      library = "fa",
      markerColor = ifelse(df1_$favorite_flag, "red", "blue"),
      iconColor = "white"
    )
    m <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        data = df1_,
        lng = ~lon,
        lat = ~lat,
        layerId = ~id,
        icon = icons,
        popup = ~paste0("<i id='heart_", id, "'
                            class='fa fa-heart'
                            style='font-size:24px; color:", ifelse(favorite_flag, "red", "grey"), "; cursor:pointer;'
                            onclick='
                              var el = document.getElementById(\"heart_", id, "\");
                              if(el.style.color===\"red\"){el.style.color=\"grey\";} else {el.style.color=\"red\";};
                              Shiny.setInputValue(\"fav_click\", ", id, ", {priority:\"event\"});
                            '>
                         </i></br><b>Full Address:</b> ", address1,
                        "</br><b>Property type:</b> ", property_type,
                        "</br><b>Price:</b> ", price,
                        "</br><b>Sq.ft:</b> ", sq_ft,
                        "</br><b>Walk score:</b> ", walk_score,
                        "</br><b>Bike score:</b> ", bike_score,
                        "</br><b>Transit score:</b> ", transit_score,
                        "</br><b>History:</b></br>", popup_df,
                        "</br><button id='comps'onclick='Shiny.setInputValue(\"popup_btn\", ", id, ", {priority:\"event\"})'>Get Comps</button>"),
        group = "all_markers",
        clusterOptions = markerClusterOptions(iconCreateFunction = JS("
        function(cluster) {
        return new L.DivIcon({
        html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
        className: 'marker-cluster'
        });
        }"))
      )
    return(m)
  })
  
  get_data <- reactive({
    return(df1)
  })
  
  onStop(function() {
    saveRDS(df1, "df1.Rds")
    message("Saved data and closed app")
  })
  
  observeEvent(input$fav_click, {
    id <- as.character(input$fav_click)
    tmp_df1 <- df1
    tmp_df1$favorite_flag[tmp_df1$id == id] <- !(tmp_df1$favorite_flag[tmp_df1$id == id])
    df1 <<- tmp_df1
    showNotification(paste0(ifelse(tmp_df1$favorite_flag[tmp_df1$id == id], "F", "Unf"), "avorited address: ", tmp_df1$address1[tmp_df1$id == id]))
    leafletProxy("map") %>%
      clearGroup("all_markers") %>%
      addAwesomeMarkers(
        data = tmp_df1,
        lng = ~lon,
        lat = ~lat,
        layerId = ~id,
        icon = awesomeIcons(
          icon = "heart",
          library = "fa",
          markerColor = ifelse(tmp_df1$favorite_flag, "red", "blue"),
          iconColor = "white"
        ),
        group = "all_markers",
        clusterOptions = markerClusterOptions(iconCreateFunction = JS("
          function(cluster) {
          return new L.DivIcon({
          html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
          className: 'marker-cluster'
          });
          }")
        ),
        popup = ~paste0("<i id='heart_", id, "'
                          class='fa fa-heart'
                          style='font-size:24px; color:", ifelse(favorite_flag, "red", "grey"), "; cursor:pointer;'
                          onclick='
                            var el = document.getElementById(\"heart_", id, "\");
                            if(el.style.color===\"red\"){el.style.color=\"grey\";} else {el.style.color=\"red\";};
                            Shiny.setInputValue(\"fav_click\", ", id, ", {priority:\"event\"});
                          '>
                         </i></br><b>Full Address:</b> ", address1,
                          "</br><b>Property type:</b> ", property_type,
                          "</br><b>Price:</b> ", price,
                          "</br><b>Sq.ft:</b> ", sq_ft,
                          "</br><b>Walk score:</b> ", walk_score,
                          "</br><b>Bike score:</b> ", bike_score,
                          "</br><b>Transit score:</b> ", transit_score,
                          "</br><b>History:</b></br>", popup_df,
                          "</br><button id='comps'onclick='Shiny.setInputValue(\"popup_btn\", ", id, ", {priority:\"event\"})'>Get Comps</button>")
      )
  })
  
  observeEvent(input$popup_btn, ignoreInit = TRUE, {
    click <- input$map_marker_click
    clicked_df <- df1[df1$id == input$map_marker_click$id, ]
    print(clicked_df)
    reqd_hpi_df <- tract_hpi_df[tract_hpi_df$tract == clicked_df$tract1, ]
    rownames(reqd_hpi_df) <- NULL
    print(reqd_hpi_df)
    clicked_df2 <- distance_pairs_with_info[(distance_pairs_with_info$address1 == clicked_df$address1[1]) &
                                              (distance_pairs_with_info$distance <= 1000 * input$max_dist), ]
    c1 <- is.na(clicked_df2$X..SQUARE.FEET)
    clicked_df2$X..SQUARE.FEET[c1] <- clicked_df2$PRICE[c1] / clicked_df2$SQUARE.FEET[c1]
    clicked_df2$SOLD.MONTH <- clicked_df2$SOLD.DATE - as.integer(format(clicked_df2$SOLD.DATE, format = "%d"))+1
    if("CITY" %in% colnames(clicked_df2)) {
      c1 <- clicked_df2$CITY == "Mount Washington"
      if(sum(c1) > 0) {
        clicked_df2$CITY[c1] <- "Mt Washington"
      }
    }
    clicked_df2$MONTH <- as.integer((month(clicked_df2$SOLD.MONTH)-2)/3)*3 + 1
    clicked_df2$YEAR <- year(clicked_df2$SOLD.MONTH)
    print(clicked_df2)
    clicked_df2$Quarter <- as.Date(paste0(clicked_df2$YEAR, "-", clicked_df2$MONTH, "-1"))
    clicked_df2$Quarter_month <- month(clicked_df2$Quarter)
    print(clicked_df2)
    plot_df <- data.frame(clicked_df2 %>% dplyr::group_by(LOCATION, SOLD.MONTH) %>%
                            dplyr::summarize(med_sq_ft = median(X..SQUARE.FEET, na.rm = T),
                                             sample_size = length(X..SQUARE.FEET[!is.na(X..SQUARE.FEET)])))
    plot_df <- plot_df[order(plot_df$SOLD.MONTH, plot_df$LOCATION, plot_df$med_sq_ft), ]
    rownames(plot_df) <- NULL
    ggplotly(ggplot(data = plot_df, aes(x=SOLD.MONTH, y = med_sq_ft, color = LOCATION, group = LOCATION, text = paste('Sample size: ', sample_size))) + geom_point(), tooltip = c("text"))
    ggplotly(ggplot(data = plot_df) + geom_line(aes(x=SOLD.MONTH, y = med_sq_ft, color = LOCATION, group = LOCATION, text = paste('Sample size: ', sample_size))))
    
    x_df <- plot_df %>% dplyr::group_by(LOCATION, SOLD.MONTH) %>% dplyr::summarize(count = length(sample_size))
    
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = clicked_df,
        lng = ~lon,
        lat = ~lat,
        layerId = ~id,
        icon = red_icon,
        popup = ~paste0("<b>Full Address:</b> ", address1,
                        "</br><b>Property type:</b> ", property_type,
                        "</br><b>Price:</b> ", price,
                        "</br><b>Sq.ft:</b> ", sq_ft,
                        "</br><b>Walk score:</b> ", walk_score,
                        "</br><b>Bike score:</b> ", bike_score,
                        "</br><b>Transit score:</b> ", transit_score,
                        "</br><b>History:</b></br>", popup_df,
                        "</br><button id='comps'onclick='Shiny.setInputValue(\"popup_btn\", ", id, ", {priority:\"event\"})'>Get Comps</button>"),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS("
        function(cluster) {
        return new L.DivIcon({
        html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
        className: 'marker-cluster'
        });
        }"))
      ) %>%
      addCircles(
        data = clicked_df,
        lng = ~lon, lat = ~lat,
        radius = input$max_dist * 1000,         # radius in meters
        color = "red",        # circle border
        weight = 2,           # border thickness
        fillColor = "red",
        fillOpacity = 0.2
      ) %>%
      addMarkers(
        data = clicked_df2,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        icon = blue_icon,
        popup = ~paste0("<b>Full Address:</b> ", address2,
                        "</br><b>Property type:</b> ", PROPERTY.TYPE,
                        "</br><b>Price:</b> ", PRICE,
                        "</br><b>Sq.ft:</b> ", SQUARE.FEET,
                        "</br><b>Price per sq.ft:</b> ", X..SQUARE.FEET,
                        "</br><b>HOA:</b> ", HOA.MONTH),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS("
          function(cluster) {
          return new L.DivIcon({
          html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
          className: 'marker-cluster'
          });
          }"))
      )
    
    output$comp_map <- renderLeaflet({
      m
    })
    
    observeEvent(input$comp_map_bounds, {
      frame_df <- clicked_df2[(clicked_df2$LATITUDE <= input$comp_map_bounds$north) &
                                (clicked_df2$LATITUDE >= input$comp_map_bounds$south) &
                                (clicked_df2$LONGITUDE >= input$comp_map_bounds$west) &
                                (clicked_df2$LONGITUDE <= input$comp_map_bounds$east), ]
      plot_df1 <- data.frame(frame_df %>% dplyr::group_by(LOCATION, Quarter) %>%
                               dplyr::summarize(med_sq_ft = median(X..SQUARE.FEET, na.rm = T),
                                                sample_size = length(X..SQUARE.FEET)))
      plot_df1 <- plot_df1[order(plot_df1$Quarter, plot_df1$LOCATION, plot_df1$med_sq_ft), ]
      rownames(plot_df1) <- NULL
      
      output$comp_median_vs_quarter_line <- renderPlotly({
        ggplotly(
          ggplot(
            data = plot_df1[order(plot_df1$LOCATION, plot_df1$Quarter), ],
            aes(x=Quarter, y = med_sq_ft, color = LOCATION, group = LOCATION, text = paste('Sample size: ', sample_size))
          ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line()
        )
      })
      
      output$comp_price_vs_quarter_violin <- renderPlotly({
        ggplotly(ggplot(data = frame_df, aes(x = factor(Quarter), y = X..SQUARE.FEET)) +
                   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_violin())
      })
    })
    
    if(nrow(reqd_hpi_df) > 0) {
      output$tract_line <- renderPlotly({
        return(
          if(sum(!is.na(reqd_hpi_df$hpi))>0) {
            return(
              ggplotly(ggplot(data = reqd_hpi_df, aes(x = year, y = hpi)) +
                       theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line())
            )
          } else if(sum(!is.na(reqd_hpi_df$hpi1990))>0) {
            return(
              ggplotly(ggplot(data = reqd_hpi_df, aes(x = year, y = hpi1990)) +
                         theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line())
            )
          } else if(sum(!is.na(reqd_hpi_df$hpi2000))>0) {
            return(
              ggplotly(ggplot(data = reqd_hpi_df, aes(x = year, y = hpi2000)) +
                         theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line())
            )
          } else {
            output$tract_line <- renderPlotly({
              return(NULL)
            })
          }
        )
      })
    } else {
      output$tract_line <- renderPlotly({
        return(NULL)
      })
    }
    
    showModal(modalDialog(
      title = paste0("Comps for ", clicked_df$address1[1]),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "xl",
      tagList(
        leafletOutput("comp_map"),
        plotlyOutput("comp_median_vs_quarter_line"),
        plotlyOutput("comp_price_vs_quarter_violin"),
        plotlyOutput("tract_line")
      )
    ))
  })
}

ui <- tagList(
  # Inject Bootstrap 5 theme
  bs_theme_dependencies(bs_theme(version = 5, bootswatch = "minty")),
  bs4DashPage(
    header = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
      sliderInput(
        inputId = "max_dist",
        label = "Max distance for comps (km)",
        min = 0,
        max = 20,
        value = 10
      )
    ),
    bs4DashBody(
      leafletOutput("map")
    )
  )
)

shinyApp(ui, server)


