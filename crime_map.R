library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(jsonlite)
library(rCharts)
library(shiny)
library(unpack)

frame_data <- readRDS("frame_data_year_month.Rds")
# frame_data <- readRDS("frame_data.Rds")
marker_locations <- readRDS("latest_info.Rds")
c[mn_lat, mn_lon] <- readRDS("mn_lat_lon.Rds")
marker_locations <- marker_locations[marker_locations$nearest_big_city == "Pittsburgh", ]
rownames(marker_locations) <- NULL
# Heatmap animation frames (lat, lng, intensity)

ui <- fluidPage(
  # sliderInput(inputId = "time",
  #             label = "date",
  #             min = min(df_filtered$year), 
  #             max = max(df_filtered$year),
  #             value = min(df_filtered$year),
  #             step=1,
  #             animate=T),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    m <- leaflet(height=2000, width=1250) %>%
      addTiles() %>%
      setView(lng = mn_lon, lat = mn_lat, zoom = 12) %>%
      addMarkers(data = marker_locations, clusterOptions = markerClusterOptions()) %>%
      addHeatmap(lng = mn_lon, lat = mn_lat, intensity = 0, radius = 10, group = "heatmap")
    
    # Add animation logic + frame number display
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
    ", toJSON(frame_data, auto_unbox = TRUE)))
    # Show map
    m
  })
}

shinyApp(ui, server)
