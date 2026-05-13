# using geometry = TRUE

library(tidycensus)
options(tigris_use_cache = TRUE) # remember this step!
dc_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "DC",
  year = 2020,
  geometry = TRUE)

dc_income

# plot 'estimate'
plot(dc_income["estimate"])




# obtain linked ACS and spatial data on median age by state for the 50 US states plus DC and Puerto Rico
# then use shift_geometry() in tigris to shift and rescale the non-continental areas for national mapping
library(tidycensus)
library(tidyverse)
library(tigris)
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()
# shifted and rescaled
plot(us_median_age$geometry)
# style state polygons using ggplot2 conventions and the geom_sf() function
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf()
# even more: edit the color pallete
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdPu",
                       direction = 1) +
  labs(title = " Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()


# using tmap for chloropleths
# 1. use tidycensus to obtain race and ethnicity data from the 2020 decennial US Census using get_decennial()
    # look at data on non_Hispanic white, non-Hispanic Black, Asian, and Hispanic populations 
    # for Jefferson County, Colorado
jeffco_race <- get_decennial(
  geography = "tract",
  state = "CO",
  county = "Jefferson",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

# 2. filter and visualize
install.packages("tmap")
library(tmap)
jeffco_black <- filter(jeffco_race, variable == "Black")
tm_shape(jeffco_black) +
  tm_polygons()
# another option
tm_shape(jeffco_black) +
  tm_polygons(col = "percent")

# 3. Percent black by quantile
hist(jeffco_black$percent)
tm_shape(jeffco_black) +
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Purples",
              title = "2020 US Census") +
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)

#4. Add histogram to the map
tm_shape(jeffco_black) +
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Purples",
              title = "2020 US Census",
              legend.hist = TRUE) +
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5,
            fontfamily = "Verdana")

# add a basemap
install.packages("mapboxapi")
library(mapboxapi)
# Replace with your token below
mb_access_token("pk.eyJ1IjoiYWxpbGxpZSIsImEiOiJjbW8wcmEzbHIwYm05MnFvZjV3dGF1cmx6In0._g0OVTju0ppgS49aiJnoNQ")

mb_access_token("pk.eyJ1IjoiYWxpbGxpZSIsImEiOiJjbW8wcmEzbHIwYm05MnFvZjV3dGF1cmx6In0._g0OVTju0ppgS49aiJnoNQ", install = TRUE)

# If you don't have a Mapbox style to use, replace style_id with "light-v9"
# and username with "mapbox". If you do, replace those arguments with your
# style ID and user name.
jeffco_tiles <- get_static_tiles(
  location = jeffco_black,
  zoom = 10,
  style_id = "light-v9",
  username = "pk.eyJ1IjoiYWxpbGxpZSIsImEiOiJjbW8wcmEzbHIwYm05MnFvZjV3dGF1cmx6In0._g0OVTju0ppgS49aiJnoNQ")



# more TMAP

# graduated symbols
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



# facet by a quality of the data
tm_shape(jeffco_race) +
  tm_facets(by = "variable", scale.factor = 4) +
  tm_fill(col = "percent",
          style = "quantile",
          n = 6,
          palette = "Blues",
          title = "Percent (2020 US Census)",) +
  tm_layout(bg.color = "grey",
            legend.position = c(-0.7, 0.15),
            panel.label.bg.color = "white")


# dot-density maps
jeffco_dots <- jeffco_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 100,
    group = "variable"
  )
background_tracts <- filter(jeffco_race, variable == "White")
tm_shape(background_tracts) +
  tm_polygons(col = "white",
              border.col = "grey") +
  tm_shape(jeffco_dots) +
  tm_dots(col = "variable",
          palette = "Set1",
          size = 0.1,
          title = "1 dot = 100 people") +
  tm_layout(legend.outside = TRUE,
            title = "Race/ethnicity,\n2020 US Census")
