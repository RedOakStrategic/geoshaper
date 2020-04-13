library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)

airports <- read.csv("Airport_Codes_mapped_to_Latitude_Longitude_in_the_United_States.csv")

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("US Airports"),
  fluidRow(column(
    8,
    leafletOutput("mymap")
  ))
)

# Define server logic
server <- function(input, output) {

  # longitudinal coordinates in dataset are off, reverse all to negative values
  # to place them in the western hemisphere
  airports$Longitude <- airports$Longitude - 2 * airports$Longitude

  # generate second set of unique location IDs for second layer of selected locations
  airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep = "")

  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())

  coordinates <- reactive({
    sf::st_as_sf(airports, coords = c("Longitude", "Latitude"))
  })

  # base map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircles(
        data = airports,
        radius = 1000,
        lat = airports$Latitude,
        lng = airports$Longitude,
        fillColor = "white",
        fillOpacity = 1,
        color = "hotpink",
        weight = 2,
        stroke = T,
        layerId = as.character(airports$locationID),
        highlightOptions = highlightOptions(
          color = "mediumseagreen",
          opacity = 1.0,
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      addDrawToolbar(
        targetGroup = "Selected",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(
          fillOpacity = 0,
          color = "white",
          weight = 3
        )),
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(
          fillOpacity = 0,
          color = "white",
          weight = 3
        )),
        circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(
          fillOpacity = 0,
          color = "white",
          weight = 3
        )),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())
      )
  })

  observeEvent(input$mymap_draw_new_feature, {
    # Only add new layers for bounded locations
    found_in_bounds <- findLocations(
      shape = input$mymap_draw_new_feature,
      location_coordinates = coordinates(),
      location_id_colname = "locationID"
    )

    # prevent re-adding selected ids from prior interactions
    for (id in found_in_bounds) {
      if (id %in% data_of_click$clickedMarker) {
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)
      }
    }

    # look up airports by ids found
    selected <- subset(airports, locationID %in% data_of_click$clickedMarker)

    proxy <- leafletProxy("mymap")
    proxy %>% addCircles(
      data = selected,
      radius = 1000,
      lat = selected$Latitude,
      lng = selected$Longitude,
      fillColor = "wheat",
      fillOpacity = 1,
      color = "mediumseagreen",
      weight = 3,
      stroke = T,
      layerId = as.character(selected$secondLocationID),
      highlightOptions = highlightOptions(
        color = "hotpink",
        opacity = 1.0,
        weight = 2,
        bringToFront = TRUE
      )
    )
  })

  observeEvent(input$mymap_draw_deleted_features, {
    # loop through list of one or more deleted features/ polygons
    for (feature in input$mymap_draw_deleted_features$features) {

      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(
        shape = feature,
        location_coordinates = coordinates(),
        location_id_colname = "secondLocationID"
      )


      # remove second layer representing selected locations
      proxy <- leafletProxy("mymap")
      proxy %>% removeShape(layerId = as.character(bounded_layer_ids))

      first_layer_ids <- subset(airports, secondLocationID %in% bounded_layer_ids)$locationID

      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
        %in% first_layer_ids]
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
