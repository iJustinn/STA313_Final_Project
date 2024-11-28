library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(readr)
library(here)
library(rmapshaper)
library(shiny)

province_data <- readRDS("province_data.rds")
provinces_with_data <- readRDS("provinces_with_data.rds")
aggregated_data <- readRDS("aggregated_data.rds")
scatter_data_sf <- readRDS("scatter_data_sf.rds")


# Define bounding box for Canada
canada_bounds <- list(
  lng1 = -141,  # Westernmost longitude
  lat1 = 41,    # Southernmost latitude
  lng2 = -52,   # Easternmost longitude
  lat2 = 83     # Northernmost latitude
)

# Shiny UI
ui <- fluidPage(
  leafletOutput("map", height = "80vh"),
  br(),
  tags$div(
    id = "province-info-box",
    style = "padding: 10px; border: 1px solid #ccc; background: #f9f9f9; display: inline-block; margin-right: 20px;",
    tags$p("Province: ", tags$span(uiOutput("province_name", inline = TRUE))),
    tags$p("Total Emission: ", tags$span(uiOutput("province_emission", inline = TRUE)))
  ),
  tags$div(
    id = "scatter-info-box",
    style = "padding: 10px; border: 1px solid #ccc; background: #f9f9f9; display: inline-block;",
    tags$p("Facility: ", tags$span(uiOutput("scatter_facility", inline = TRUE))),
    tags$p("Substance Info: ", tags$span(uiOutput("scatter_substance", inline = TRUE)))
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive values to store info for province and scatter points
  selected_province <- reactiveValues(name = "Select a province", emission = "N/A")
  selected_scatter <- reactiveValues(facility = "Select a facility", substance = "N/A")
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) %>%  # Set minimum zoom level
      addTiles() %>%
      setView(lng = -95, lat = 60, zoom = 3) %>%  # Default view
      setMaxBounds(
        lng1 = canada_bounds$lng1,
        lat1 = canada_bounds$lat1,
        lng2 = canada_bounds$lng2,
        lat2 = canada_bounds$lat2
      ) %>%
      addPolygons(
        data = provinces_with_data,
        fillColor = ~colorNumeric("YlOrRd", total_quantity)(total_quantity),
        color = "white", weight = 1, fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "blue", bringToFront = TRUE),
        layerId = ~PREABBR  # Assign unique IDs for provinces
      ) %>%
      addLayersControl(
        overlayGroups = c("scatter"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", domain = provinces_with_data$total_quantity),
        values = provinces_with_data$total_quantity,
        position = "bottomright",
        title = "Total Emissions (grams)"
      )
    
  })
  
  # Update the province info box dynamically
  output$province_name <- renderUI({ HTML(selected_province$name) })
  output$province_emission <- renderUI({HTML(selected_province$emission)})
  
  # Update the scatter info box dynamically
  output$scatter_facility <- renderUI({ HTML(selected_scatter$facility) })
  output$scatter_substance <- renderUI({ HTML(selected_scatter$substance) })
  
  # Handle click event on province polygons
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    province_clicked <- click$id  # Province ID (PREABBR)
    total_emission <- provinces_with_data %>% filter(PREABBR == province_clicked) %>% pull(total_quantity)
    
    # Update selected province details
    selected_province$name <- province_clicked
    selected_province$emission <- paste(total_emission, "grams")
    
    # Update scatter point details
    selected_scatter$facility <- "Select a facility"
    selected_scatter$substance <- "N/A"
    
    # Filter scatter data and show scatter points
    filtered_data <- aggregated_data %>% filter(province == province_clicked)
    scatter_data <- st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
    
    leafletProxy("map") %>%
      clearGroup("scatter") %>%
      addCircleMarkers(
        data = scatter_data,
        radius = ~log(total_quantity + 1) * 0.6,
        color = "#0073C2FF", stroke = FALSE, fillOpacity = 0.6,
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 18),
        layerId = ~facility,  # Set layer ID to facility for scatter info
        group = "scatter"
      ) %>%
      showGroup("scatter") %>%
      fitBounds(click$bbox[1], click$bbox[2], click$bbox[3], click$bbox[4])
  })
  
  # Handle click event on scatter points
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    facility_clicked <- click$id
    scatter_info <- aggregated_data %>% filter(facility == facility_clicked)
    
    # Update scatter point details
    selected_scatter$facility <- scatter_info$facility[1]
    selected_scatter$substance <- scatter_info$substances_info[1]
  })
}

# Run the Shiny app
shinyApp(ui, server)
