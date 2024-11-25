library(tidyverse)
library(maps)
library(viridis)
library(patchwork)

# Load the industrial releases dataset
df <- read.csv("page3_top15_industries.csv") %>%
  group_by(industry) %>%
  summarise(total_quantity = sum(total_quantity)) %>%
  arrange(desc(total_quantity)) %>%
  slice(1:15) %>%
  mutate(
    total_quantity = total_quantity / 1e6, # Convert to millions
    industry = str_replace_all(industry, c(" ore mining" = "", " and " = " & ", "-Zinc" = "-Zn", "-zinc" = "-Zn"))
  )

# Map data (world map)
map <- map_data("world")

# Assign regions to industries for mapping
industry_region_mapping <- c(
  "Gold & silver" = "United States",
  "Lead-Zn" = "Canada",
  "Copper-Zn" = "Australia",
  "Gold" = "South Africa",
  "Silver" = "Mexico"
)

df <- df %>%
  mutate(region = recode(industry, !!!industry_region_mapping))

# Combine map data with the industrial dataset
map_data <- left_join(map, df, by = c("region" = "region"))

# Create map globe plot
map_plot <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_quantity)) +
  geom_polygon(color = "white") +
  scale_fill_viridis(option = "C", guide = "none", na.value = "gray80") +
  coord_map("ortho", orientation = c(10, -90, 0)) + # Adjust orientation as needed
  theme_void()

# Prepare data for bar circle plot
df <- df %>% arrange(desc(total_quantity))
df$id <- seq(1, nrow(df))
number_of_bars <- nrow(df)
angles <- 90 - 360 * (df$id - 0.5) / number_of_bars
df$hjust <- ifelse(angles < -90, 1, 0)
df$angle <- ifelse(angles < -90, angles + 180, angles)

# Adjust ylim to match the Earth's circle size
circle_radius <- 500 # Adjust this value for the desired alignment
bar_plot_industry <- ggplot(df, aes(x = as.factor(id), y = total_quantity, fill = total_quantity)) +
  geom_bar(stat = "identity", color = "white", size = 0.2) +
  geom_text(
    aes(
      x = as.factor(id),
      y = total_quantity + max(total_quantity) * 0.05,
      label = industry,
      hjust = hjust,
      angle = angle
    ),
    color = "black", alpha = 0.8, size = 3, inherit.aes = FALSE
  ) +
  scale_fill_viridis(option = "C", guide = guide_colorbar(title.position = "top", barwidth = 10)) +
  ylim(-circle_radius, circle_radius * 0.3) + # Adjust ylim to align with the Earth plot
  labs(
    title = "Top 15 Industrial Releases",
    subtitle = "Releases by Industry (in Millions)",
    caption = "Data Source: Provided Dataset",
    fill = "Quantity (Millions)"
  ) +
  theme_void() +
  coord_polar() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(t = 25, b = 10)
  )

# Combine the bar circle plot with the map plot using patchwork
combined_plot <- bar_plot_industry +
  inset_element(map_plot, left = 0.15, bottom = 0.15, right = 0.85, top = 0.85) # Adjust Earth plot position and size

# Display the plot
combined_plot

# Uncomment to save the plot
# ggsave("industrial_releases_globe_plot_aligned.jpeg", combined_plot, height = 12, width = 12)

