# Walker Chapter 8.1
# Detroit, MI

library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

# Get Michigan tract data by race/ethnicity
mi_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "MI",
  geometry = TRUE,
  year = 2019)

# Use tidycensus to get urbanized areas by population with geometry,
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", MI Urbanized Area (2010)")))
# Compute an inner spatial join between the California tracts and the
# urbanized areas, returning tracts in the largest California urban
# areas with the urban_name column appended
mi_urban_data <- mi_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()


# Dissimilarity index: D
mi_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "Detroit") %>% dissimilarity(
           group = "variable",
           unit = "GEOID",
           weight = "estimate")
mi_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
                 dissimilarity(.x,
                               group = "variable",
                               unit = "GEOID",
                               weight = "estimate"
                 )) %>%
  arrange(desc(est))

# multi-group segregation indices
mutual_within(
  data = mi_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE)
detroit_local_seg <- mi_urban_data %>%
  filter(urban_name == "Detroit") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )
detroit_tracts_seg <- tracts("MI", cb = TRUE, year = 2019) %>%
  inner_join(detroit_local_seg, by = "GEOID")
detroit_tracts_seg %>%
  ggplot(aes(fill = ls)) + #
  geom_sf(color = NA) +
  coord_sf(crs = 26946) +
  scale_fill_viridis_c(option = "inferno") +
  theme_void() +
  labs(fill = "Local\nsegregation index")

# visualizing the diversity gradient
detroit_entropy <- mi_urban_data %>%
  filter(urban_name == "Detroit") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
    data = .x,
    group = "variable",
    weight = "estimate",
    base = 4)))
detroit_entropy_geo <- tracts("MI", cb = TRUE, year = 2019) %>%
  inner_join(detroit_entropy, by = "GEOID")