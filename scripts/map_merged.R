#### Setup ####
# Load Libraries
library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(readr)
library(here)
library(rmapshaper)
library(lubridate)
library(ggplot2)

#### Load Province Data ####
# Load the shapefile
shapefile_path <- here("lpr_000b16a_e", "lpr_000b16a_e.shp")
provinces <- st_read(shapefile_path)

# Simplify the shapefile geometry to improve performance
provinces <- ms_simplify(provinces, keep = 0.05, keep_shapes = TRUE)

# Transform the shapefile column for consistency
provinces <- provinces %>%
  mutate(
    PREABBR = toupper(PREABBR),
    PREABBR = case_when(
      PREABBR == "N.L." ~ "NL",
      PREABBR == "P.E.I." ~ "PE",
      PREABBR == "N.S." ~ "NS",
      PREABBR == "N.B." ~ "NB",
      PREABBR == "QUE." ~ "QC",
      PREABBR == "ONT." ~ "ON",
      PREABBR == "MAN." ~ "MB",
      PREABBR == "SASK." ~ "SK",
      PREABBR == "ALTA." ~ "AB",
      PREABBR == "B.C." ~ "BC",
      PREABBR == "Y.T." ~ "YT",
      PREABBR == "N.W.T." ~ "NT",
      PREABBR == "NVT." ~ "NU",
      TRUE ~ PREABBR
    )
  )

# Transform to WGS84 (Leaflet requires long-lat CRS)
provinces <- st_transform(provinces, crs = 4326)

# Load province data
province_data <- read_csv(here("data", "analysis_data", "map_by_province_data.csv"),
                          show_col_types = FALSE)

# Join shapefile with emissions data
provinces_with_data <- provinces %>%
  left_join(province_data, by = c("PREABBR" = "province"))

# Reduce columns to those needed for mapping to improve performance
provinces_with_data <- provinces_with_data %>%
  select(PREABBR, geometry, total_quantity)

# Create a color palette for the heatmap
palette <- colorNumeric(
  palette = "YlOrRd",
  domain = provinces_with_data$total_quantity
)

#### Load and Filter Data ####
# Load Data
coor_data <- read_csv(here("data", "analysis_data", "map_coordinate_data.csv"),
                      show_col_types = FALSE)

cleaned_data <- read_csv(here("data", "analysis_data", "map_cleaned_data.csv"),
                         show_col_types = FALSE)

# Integrate data
coor_data <- coor_data %>%
  select(-province)

cleaned_data <- cleaned_data %>%
  mutate(facility = tolower(facility)) %>%
  group_by(NPRI_id, province, unit, facility, substance) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

# Merge the datasets by NPRI_id
combined_data <- cleaned_data %>%
  inner_join(coor_data, by = "NPRI_id") %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Aggregate emissions by facility, province, latitude, and longitude, preserving substances
aggregated_data <- combined_data %>%
  group_by(facility, province, latitude, longitude) %>%
  summarise(
    substances_info = paste(
      "Substance: ", substance, "<br>",
      "Emission Quantity: ", quantity, unit, collapse = "<br><br>"
    ),
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = 'drop'
  )

# Convert the data to an sf object for spatial analysis
aggregated_data_sf <- st_as_sf(aggregated_data, coords = c("longitude", "latitude"), crs = 4326)

#### Combine and Save Maps ####
# Combine the two leaflet maps with conditional layer control for zoom levels
combined_map <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = provinces_with_data,
    fillColor = ~palette(total_quantity),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste(
      "<strong>Province:</strong> ", PREABBR, "<br>",
      "<strong>Total Emissions:</strong> ", total_quantity, " grams"
    ),
    group = "Heatmap"
  ) %>%
  addLegend(
    pal = palette,
    values = provinces_with_data$total_quantity,
    position = "bottomright",
    title = "Total Emissions (grams)",
    opacity = 0.7,
    group = "Heatmap"
  ) %>%
  addCircleMarkers(
    data = aggregated_data_sf,
    radius = ~log(total_quantity + 1) * 0.6,
    color = "#0073C2FF",
    stroke = FALSE,
    fillOpacity = 0.6,
    clusterOptions = markerClusterOptions(
      disableClusteringAtZoom = 8
    ),
    popup = ~paste(
      "Facility: ", facility, "<br>",
      "Province: ", province, "<br><br>",
      substances_info
    ),
    group = "Scatter"
  ) %>%
  addLayersControl(
    baseGroups = c("Heatmap", "Scatter"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save the combined map as an HTML file
saveWidget(combined_map, file = here("plots", "map_merged.html"), selfcontained = FALSE)
