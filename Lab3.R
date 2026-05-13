# 1. dot density map 

# 2. using tmap, create a graduated symbol map of all young persons for a location of your choice
library(tidycensus)
library(tmap)
library(sf)
library(dplyr)

# 1. Fetch data: Population under 18 for Census Tracts in Denver County, CO
# Using 2020 SF1 Census Data (Table P1: Population Under 18)
young_denver <- get_decennial(
  geography = "tract",
  variables = "P1_004N", # Count of persons under 18
  state = "CO",
  county = "Denver",
  geometry = TRUE,
  year = 2020
)

# 2. Calculate Centroids for graduated symbols
young_centroids <- st_centroid(young_denver)

# 3. Create the graduated symbol map
tm_shape(young_denver) +
  tm_polygons(col = "white", border.col = "grey", alpha = 0.5) + # Base layer
  tm_shape(young_centroids) +
  tm_symbols(
    size = "value",
    col = "navy",
    alpha = 0.6,
    scale = 2,
    border.col = "white",
    title.size = "Under 18 Pop."
  ) +
  tm_layout(
    title = "Young Persons (Under 18) in Denver, CO",
    frame = FALSE,
    legend.position = c("right", "top")
  ) +
  tm_credits("Data Source: 2020 Census SF1 (Table P1_004N)\nCensus Tract Level", 
             position = c("right", "bottom"))








# 3. Make a choropleth map of income for your location of choice using either ggplot or tmap
library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE) # remember this step!

co_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CO",
  year = 2020,
  geometry = TRUE)

co_income # view data

# using ggplot
# option 1
ggplot(data = co_income, aes(fill = estimate)) +
  geom_sf()
# option 2
ggplot(data = co_income, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "PuBuGn",
                       direction = 1) +
  labs(title = "Median Income in CO, 2024",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()

# use tmap

# 1. use tidycensus to obtain race and ethnicity data from the 2020 decennial US Census using get_decennial()
# look at data on non_Hispanic white, non-Hispanic Black, Asian, and Hispanic populations 
# for Jefferson County, Colorado
co_income <- get_acs(
  geography = "county",
  state = "Colorado",
  variables = "B19013_001",
  year = 2024,
  geometry = TRUE
)

# 2. filter and visualize
library(tmap)
tm_shape(co_income) +
  tm_polygons()
# another option
tm_shape(co_income) +
  tm_polygons(col = "estimate")

# 3. Income by Quantile
hist(co_income$estimate)
tm_shape(co_income) +
  tm_polygons(col = "estimate",
              style = "jenks", # quantile, estimate are other options
              n = 5,
              palette = "Emrld",
              title = "2020 US Census") +
  tm_layout(title = "Colorado Income\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)

#4. Add histogram to the map
tm_shape(co_income) +
  tm_polygons(col = "estimate",
              style = "jenks",
              n = 5,
              palette = "Emrld",
              title = "2024 US ACS",
              legend.hist = TRUE) +
  tm_layout(title = "Colorado Income\nby Census tract",
            title.position = "top",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5,
            fontfamily = "Verdana")
