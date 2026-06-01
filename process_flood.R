library(sf)
library(leaflet)

shapeData <- sf::read_sf("Flood/Pittsburgh/S_FLD_HAZ_AR.shp")
leaflet() %>%
  addTiles() %>%
  addPolygons(data = shapeData,
              color = ~colorFactor("Set3", shapeData$FLD_ZONE)(FLD_ZONE),
              weight = 1,
              opacity = 0.7,
              fillOpacity = 0.5,
              popup = ~FLD_ZONE)
