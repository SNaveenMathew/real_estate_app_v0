library(leaflet)

server <- function(input,output, session) {
  data <- reactive({
    x <- latest_info_with_price
    if(!is.null(input$nearest_big_city)) {
      x <- x[sapply(x$nearest_big_city, function(l) l %in% input$nearest_big_city), ]
    }
    if(!is.null(input$status)) {
      x <- x[sapply(x$status, function(l) l %in% input$status), ]
    }
    if(!is.null(input$property_type)) {
      x <- x[sapply(x$property_type, function(l) l %in% input$property_type), ]
    }
    return(x)
  })
  
  observeEvent(input$update, {
    data <- eventReactive(input$update, {
      x <- latest_info_with_price
      if(!is.null(input$nearest_big_city)) {
        x <- x[sapply(x$nearest_big_city, function(l) l %in% input$nearest_big_city), ]
      }
      if(!is.null(input$status)) {
        x <- x[sapply(x$status, function(l) l %in% input$status), ]
      }
      if(!is.null(input$property_type)) {
        x <- x[sapply(x$property_type, function(l) l %in% input$property_type), ]
      }
      return(x)
    })
  })
  
  observeEvent(input$remove_sold, {
    data <- eventReactive(input$remove_sold, {
      x <- latest_info_with_price
      if(!is.null(input$nearest_big_city)) {
        x <- x[sapply(x$nearest_big_city, function(l) l %in% input$nearest_big_city), ]
      }
      if(!is.null(input$status)) {
        x <- x[sapply(x$status, function(l) l %in% input$status), ]
      }
      if(!is.null(input$property_type)) {
        x <- x[sapply(x$property_type, function(l) l %in% input$property_type), ]
      }
      x <- x[tolower(x$status) != "sold", ]
      rownames(x) <- NULL
      return(x)
    })
  })
  
  output$map_type_input_ui <- renderUI({
    if(!is.null(input$nearest_big_city)) {
      if(length(input$nearest_big_city) == 1) {
        if(input$nearest_big_city %in% c("Pittsburgh", "Chicago", "Baltimore", "Indianapolis", "Buffalo", "Philadelphia", "Boston", "Minneapolis")) {
          return(
            fluidRow(
              selectInput(
                inputId = "map_type_input",
                label = "Type of map:",
                choices = c("Crime" = "Crime", "Walk" = "Walk")
              )
            )
          )
        }
      }
    }
  })
  
  output$city_insights <- renderUI({
    if(!is.null(input$nearest_big_city)) {
      if(input$nearest_big_city %in% c("Pittsburgh")) {
        return(actionButton(
          inputId = "get_city_insights",
          label = "Get City Insights"
        ))
      }
    }
  })
  
  observeEvent(input$get_city_insights, {
    if(input$nearest_big_city == "Pittsburgh") {
      url <- a("Redfin market insights", href="https://www.redfin.com/city/15702/PA/Pittsburgh/housing-market")
      return(showModal(modalDialog(tagList("Redfin insights:", url))))
    }
  })
  
  crime_frame_data <- reactive({
    if(input$crime_map_type == "hist_month") {
      df <- eval(parse(text = paste0("month_frame_data_", tolower(input$nearest_big_city))))
      return(df)
    } else if(input$crime_map_type == "hist_year") {
      df <- eval(parse(text = paste0("year_frame_data_", tolower(input$nearest_big_city))))
      return(df)
    } else if(input$crime_map_type == "last_year_month") {
      df <- eval(parse(text = paste0("last_year_month_frame_data_", tolower(input$nearest_big_city))))
      return(df)
    } else {
      return()
    }
  })
  
  last_year_data <- reactive({
    if(input$crime_map_type == "last_year_static") {
      df <- eval(parse(text = paste0("last_year_df_", tolower(input$nearest_big_city))))
      return(df)
    } else {
      return()
    }
  })
  
  output$crime_map_type_input <- renderUI({
    if(!is.null(input$map_type_input)) {
      if(input$map_type_input == "Crime") {
        return(
          selectInput(
            inputId = "crime_map_type",
            label = "Crime map type: ",
            choices = c("Animate history by month" = "hist_month",
                        "Animate history by year" = "hist_year",
                        "Animate last year by month" = "last_year_month",
                        "Static last year" = "last_year_static"),
            selected = "last_year_static"
          )
        )
      }
    }
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = df,
        lng = ~lon,
        lat = ~lat,
        popup = ~paste0("<b>Address:</b> ", address,
                        "</br><b>City:</b> ", city,
                        "</br><b>Sq.ft:</b> ", sq_ft,
                        "</br><b>Property type:</b> ", property_type,
                        "</br><b>Walk score:</b> ", walk_score,
                        "</br><b>Bike score:</b> ", bike_score,
                        "</br><b>Transit score:</b> ", transit_score,
                        ifelse(!is.na(redfin_link), paste0("</br><a href='", redfin_link, "'>Redfin</a>"), ""),
                        "</br><b>History:</b></br>", popup_df),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS("
        function(cluster) {
        return new L.DivIcon({
        html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
        className: 'marker-cluster'
        });
        }")))
    
    # m <- leaflet(data = df) %>%
    #   addTiles() %>%
    #   addMarkers(
    #     lng = ~lon,
    #     lat = ~lat,
    #     popup = ~paste0("<b>Address:</b> ", address,
    #                     "</br><b>City:</b> ", city,
    #                     "</br><b>Sq.ft:</b> ", sq_ft,
    #                     "</br><b>Property type:</b> ", property_type,
    #                     "</br><b>Walk score:</b> ", walk_score,
    #                     "</br><b>Bike score:</b> ", bike_score,
    #                     "</br><b>Transit score:</b> ", transit_score,
    #                     "</br><a href='", redfin_link, "'>Redfin</a>",
    #                     "</br><b>History:</b></br>", popup_df),
    #     clusterOptions = markerClusterOptions(iconCreateFunction = JS("
    #     function(cluster) {
    #     return new L.DivIcon({
    #     html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
    #     className: 'marker-cluster'
    #     });
    #     }")))
    if(is.null(input$nearest_big_city)) {
      return(m)
    } else {
      if(length(input$nearest_big_city) == 1) {
        # tryCatch({
        if(!is.null(input$map_type_input)) {
        if(input$map_type_input == "Crime") {
          if(!is.null(input$crime_map_type)) {
            if(input$crime_map_type %in% c("hist_month", "hist_year", "last_year_month")) {
              frame_data_ <- crime_frame_data()
              mn_lon <- eval(parse(text = paste0("mn_lon_", tolower(input$nearest_big_city))))
              mn_lat <- eval(parse(text = paste0("mn_lat_", tolower(input$nearest_big_city))))
              m <- m %>%
                addHeatmap(lng = mn_lon, lat = mn_lat, intensity = 0, radius = 10, group = "heatmap") %>%
                setView(lng = mn_lon, lat = mn_lat, zoom = 12)
              m <- htmlwidgets::onRender(m, sprintf("
                function(el, x) {
                  var map = this;
              
                  // Frames: a named object of name -> points[]
                  var frames = %s;
                  var frameNames = Object.keys(frames);
                  var current = 0;
                  var heatLayer;
              
                  // Frame label UI
                  var frameControl = L.control({position: 'bottomleft'});
                  frameControl.onAdd = function(map) {
                    var div = L.DomUtil.create('div', 'frame-label');
                    div.style.backgroundColor = 'rgba(255,255,255,0.8)';
                    div.style.padding = '4px 8px';
                    div.style.font = 'bold 14px sans-serif';
                    div.style.borderRadius = '6px';
                    div.innerHTML = '';
                    return div;
                  };
                  frameControl.addTo(map);
              
                  function updateHeatmap() {
                    var name = frameNames[current];
                    var points = frames[name];
              
                    if (!heatLayer) {
                      heatLayer = L.heatLayer(points, {radius: 10}).addTo(map);
                    } else {
                      heatLayer.setLatLngs(points);
                    }
              
                    document.querySelector('.frame-label').innerHTML = 'Year: ' + name;
              
                    current = (current + 1) %% frameNames.length;
                  }
              
                  updateHeatmap();
                  setInterval(updateHeatmap, 1000);
                }
              ", toJSON(frame_data_, auto_unbox = TRUE))
              )
              return(m)
            } else if(input$crime_map_type == "last_year_static") {
              df <- last_year_data()
              return(m %>% addHeatmap(data = df, radius = 10, group = "heatmap"))
            } else {
              return(m)
            }
          }
          } else if(!is.null(input$map_type_input == "Walk")) {
            return(
              m %>%
                addPolygons(data = df1_1, fillColor = ~pal(NatWalkInd), stroke = NA, fillOpacity = .5, popup = ~NatWalkInd) %>%
                # addPolygons(data = df1_1, fillColor = ~pal(D3BPO4), weight = 0.3, color = "white", fillOpacity = 0.7) %>%
                addPolylines(data = sidewalks, color = "lightblue", weight = 5, label = "Bikeable Sidewalks") %>%
                addPolylines(data = bike_lanes, color = "steelblue", weight = 5, label = "Bike Lane") %>%
                addPolylines(data = on_street, color = "lightgreen", weight = 5, label = "On Street Bike Lane") %>%
                addPolylines(data = on_street, color = "darkgreen", weight = 5, label = "Protected Bike Lane") %>%
                addPolylines(data = sharrows, color = "orange", weight = 5, label = "Sharrows") %>%
                addPolylines(data = caution, color = "red", weight = 5, label = "Cautionary Bike Lane") %>%
                addPolylines(data = trail, color = "pink", weight = 5, label = "Trail") %>%
                addLegend(data = df1_1, pal = pal, values = ~NatWalkInd)
            )
          }
        }
        # }, error = function(e) {
        #   return(m)
        # })
      }
    }
  })
  
}