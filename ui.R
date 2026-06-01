library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectizeInput(
      inputId = "status",
      label = "Select the statuses to display:",
      choices = statuses,
      selected = c(),
      multiple = T,
      options = list(
        plugins = list("remove_button")
      )
    ),
    selectizeInput(
      inputId = "nearest_big_city",
      label = "Select the nearest big city:",
      choices = nearest_big_cities,
      selected = c(),
      multiple = T,
      options = list(
        plugins = list("remove_button")
      )
    ),
    uiOutput("map_type_input_ui"),
    uiOutput("crime_map_type_input"),
    selectizeInput(
      inputId = "property_type",
      label = "Select the property types:",
      choices = property_types,
      selected = c(),
      multiple = T,
      options = list(
        plugins = list("remove_button")
      )
    ),
    fluidRow(
      column(width = 12,
        actionButton(
          inputId = "update",
          label = "Update"
        ),
        actionButton(
          inputId = "remove_sold",
          label = "Remove 'Sold'"
        )
      )
    )
  ),
  dashboardBody(
    uiOutput("city_insights"),
    leafletOutput("mymap", height = 850)
  )
)