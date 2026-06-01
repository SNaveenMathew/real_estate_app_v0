library(sf)
library(dplyr)
library(leaflet)

df <- sf::st_read("Walkability/Natl_WI.gdb/a00000009.gdbtable")
df1 <- df %>% filter(CSA_Name == "Pittsburgh-New Castle-Weirton, PA-OH-WV")
df1_1 <- st_transform(df1, 4326)
pal <- colorNumeric(palette = "viridis", domain = df1_1$NatWalkInd)
leaflet(df1_1) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(NatWalkInd), stroke = NA, fillOpacity = .5, popup = ~NatWalkInd) %>%
  addLegend(pal = pal, values = ~NatWalkInd)


df <- sf::st_read("Smart Location/SmartLocationDatabase.gdb/a00000009.gdbtable")
df1 <- df %>% filter(CSA_Name == "Pittsburgh-New Castle-Weirton, PA-OH-WV")
df1_1 <- st_transform(df1, 4326)
pal <- colorNumeric(palette = "viridis", domain = df1_1$D3BPO4)
leaflet(df1_1) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(D3BPO4), stroke = NA, fillOpacity = .5, popup = ~D3BPO4) %>%
  addLegend(pal = pal, values = ~D3BPO4)

lanes <- st_read("BikePGH/Bike Lanes/Bike Lanes.shp")
lanes <- st_transform(lanes, 4326)
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
pal <- colorNumeric(palette = "viridis", domain = df1_1$NatWalkInd)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = df1_1, fillColor = ~pal(NatWalkInd), stroke = NA, fillOpacity = .5, popup = ~NatWalkInd) %>%
  addMarkers(
    data = latest_info_with_price1,
    lng = ~lon,
    lat = ~lat,
    popup = ~paste0("<b>Address:</b> ", address,
                    "</br><b>City:</b> ", city,
                    "</br><b>Sq.ft:</b> ", sq_ft,
                    "</br><b>Property type:</b> ", property_type,
                    "</br><b>Walk score:</b> ", walk_score,
                    "</br><b>Bike score:</b> ", bike_score,
                    "</br><b>Transit score:</b> ", transit_score,
                    "</br><a href='", redfin_link, "'>Redfin</a>",
                    "</br><b>History:</b></br>", popup_df),
    clusterOptions = markerClusterOptions(iconCreateFunction = JS("
        function(cluster) {
        return new L.DivIcon({
        html: '<div style=\"background-color:rgba(0, 180, 0, 0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
        className: 'marker-cluster'
        });
        }"))) %>%
  # addPolygons(data = df1_1, fillColor = ~pal(D3BPO4), weight = 0.3, color = "white", fillOpacity = 0.7) %>%
  addPolylines(data = sidewalks, color = "lightblue", weight = 5, label = "Bikeable Sidewalks") %>%
  addPolylines(data = bike_lanes, color = "steelblue", weight = 5, label = "Bike Lane") %>%
  addPolylines(data = on_street, color = "lightgreen", weight = 5, label = "On Street Bike Lane") %>%
  addPolylines(data = on_street, color = "darkgreen", weight = 5, label = "Protected Bike Lane") %>%
  addPolylines(data = sharrows, color = "orange", weight = 5, label = "Sharrows") %>%
  addPolylines(data = caution, color = "red", weight = 5, label = "Cautionary Bike Lane") %>%
  addPolylines(data = trail, color = "pink", weight = 5, label = "Trail") %>%
  addLegend(data = df1_1, pal = pal, values = ~NatWalkInd)

