# Part 1: Find a core-based statistical area (micro or metro) that crosses state lines. 
    # Download the tracts for the states and the metropolitan boundaries separately. 
        # Filter out the tracts to the metropolitan boundary.
    # Provide an image of these tracts and metropolitan boundaries (different colors), 
        # and note down how many census tracts there are with a screenshot.

#use spatial overlay to identify geographic features that fall within a given metro area that falls over a state line
# using Charlotte-Concord-Gastonia, South and North Carolina
options(tigris_use_cache = TRUE)
# CRS used: NAD83(2011) need proper NC/SC coordinate system?
nc_sc_tracts <- map_dfr(c("NC", "SC"), ~{
  tracts(.x, cb = TRUE, year = 2020)}) %>%
  st_transform(8528)
charlotte_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Charlotte-Concord-Gastonia")) %>%
  st_transform(8528)
# states-wide map
ggplot() + 
  geom_sf(data = nc_sc_tracts, fill = "linen", color = "grey") +
  geom_sf(data = charlotte_metro, fill = NA, color = "royalblue4") +
  theme_void()
# filter to metro area
charlotte_tracts_within <- nc_sc_tracts %>%
  st_filter(charlotte_metro, .predicate = st_within)
ggplot() +
  geom_sf(data = charlotte_tracts_within, fill = "linen", color = "grey") +
  geom_sf(data = charlotte_metro, fill = NA, color = "royalblue4") +
  theme_void()




# Part 2: Replicate the erase_water() workflow for a different county with a significant water area. 
    # Be sure to transform your data to an appropriate projected coordinate system (selected with suggest_crs()) first.
    # You might choose a Great Lakes state or somewhere here in Oregon.
    # If the operation is too slow, try re-running with a higher area threshold or a smaller geography.
# use: Mackinac County, Michigan
library(tidycensus)
library(sf)
library(dplyr)
library(ggplot2)
mackinac <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MI",
  county = "Mackinac",
  geometry = TRUE,
  year = 2020,
  cb = FALSE #this designates TIGER/Line
) %>%
  st_transform(6494) # project to NAD83 (2011) Michigan central
ggplot(mackinac) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_distiller(palette = "PuBuGn", labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")

mackinac_erase <- erase_water(mackinac)
ggplot(mackinac_erase) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_distiller(palette = "PuBuGn", labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")

# or, for the whole state of MI:
mi_tracts <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MI",
  geometry = TRUE,
  year = 2020,
  cb = FALSE
) %>%
  st_transform(6494)
ggplot(mi_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_distiller(
    palette = "PuBuGn",
    labels = scales::label_dollar()
  ) +
  theme_void() +
  labs(fill = "Median household\nincome")
# erase water
mi_tracts_erase <- erase_water(mi_tracts)
ggplot(mi_tracts_erase) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_distiller(
    palette = "PuBuGn",
    labels = scales::label_dollar()
  ) +
  theme_void() +
  labs(fill = "Median household\nincome")


# Part 3: Reproduce a Getis-Ord hotspot analysis of any metropolitan area of your choice. 
    # Choose any variable too. 
    # Reproduce a map that is categorized with high and low clusters labelled.
    # Note down what trends you see, and why you think these areas have those identified clusters (200 words). 
# get tracts for Denver Metro area
denver_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Denver")) %>%
  st_transform(32138)
denver_tracts <- get_acs(
  geography = "tract",
  variables = "B19013_001", #B01002_001 is median age, B19013_001 is income
  state = "CO",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(32138) %>%
  st_filter(denver_metro, .predicate = st_within) %>%
  na.omit()
# graph median income
ggplot(denver_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()
# Local spatial autocorrelation: Getis Ord-G & Hot Spots
# For Gi*, re-compute the weights with `include.self()`
neighbors <- poly2nb(denver_tracts, queen = TRUE)
localg_weights <- nb2listw(include.self(neighbors))
denver_tracts$localG <- localG(denver_tracts$estimate, localg_weights)
# fix for ggplot (maybe need)
denver_tracts$localG <- as.numeric(localG(
  denver_tracts$estimate,
  localg_weights))
# plot
ggplot(denver_tracts) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(
    palette = "RdYlBu",
    limits = c(-max(abs(denver_tracts$localG)),
               max(abs(denver_tracts$localG))))+
  theme_void() +
  labs(fill = "Local Gi* statistic")
# results returned are z-scores
# choose hot spot thresholds in the statistic, calculate them with case_when(), then plot them accordingly
denver_tracts <- denver_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.56 ~ "High cluster",
    localG <= -2.56 ~ "Low cluster",
    TRUE ~ "Not significant"))
ggplot(denver_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  theme_void()
