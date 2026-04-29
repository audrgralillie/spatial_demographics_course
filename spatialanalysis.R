library(sf)
library(tigris)
library(tidycensus)
library(tidyverse)

# SPATIAL OVERLAY

#1. use spatial overlay to identify geographic features that fall within a given metro area that falls over a state line
    # using Kansas City, MO/KS
options(tigris_use_cache = TRUE)
# CRS used: NAD83(2011) Kansas Regional Coordinate System
# Zone 11 (for Kansas City)
ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)}) %>%
  st_transform(8528)
kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)
ggplot() + 
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()


#2. indentifying spatial subsets in the metro area (tracts)
# option 1
kc_tracts <- ks_mo_tracts[kc_metro, ]
ggplot() +
  geom_sf(data = kc_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()
# another option (option 2): tidyverse-style
kc_tracts_within <- ks_mo_tracts %>%
  st_filter(kc_metro, .predicate = st_within)
# Equivalent syntax:
# kc_metro2 <- kc_tracts[kc_metro, op = st_within]
ggplot() +
  geom_sf(data = kc_tracts_within, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()




# SPATIAL JOINS

# 1: Point-in-polygon

# a health data analyst in Gainesville, Florida needs to determine the percentage of residents age 65 and up who lack health insurance 
    # in patients’ neighborhoods. The analyst has a dataset of patients with patient ID along with longitude and latitude information.
library(tidyverse)
library(sf)
library(tidycensus)
library(mapview)
# grab patient info, lat, long
gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.361748, -82.374377,
                -82.38177, -82.259461, -82.367436, -82.404031,
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576,
               29.677201, 29.674923, 29.71099, 29.711587,
               29.648227, 29.624037))
# convert to actual spatial object
# CRS: NAD83(2011) / Florida North
gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440) # need to correct the projected coordinate system
# and then map points for each patient_id
mapview(
  gainesville_sf,
  col.regions = "red",
  legend = FALSE)
# get health insurance data
alachua_insurance <- get_acs(
  geography = "tract",
  variables = "DP03_0096P",
  state = "FL",
  county = "Alachua",
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate,
         pct_insured_moe = moe) %>%
  st_transform(6440)
# map it pre-spatial join to see what the relationships might look like
mapview(
  alachua_insurance,
  zcol = "pct_insured",
  layer.name = "% with health<br/>insurance") +
  mapview(
    gainesville_sf,
    col.regions = "red",
    legend = FALSE)
#then spatial join it!
gainesville_patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance)


# 2: Polygon-on-polygon

# we are interested in analyzing the distributions of neighborhoods (defined here as Census tracts) by Hispanic population 
    # for the four largest metropolitan areas in Texas
    # use the variable B01003_001 from the 2019 1-year ACS to acquire population data by core-based statistical area (CBSA) 
    # along with simple feature geometry which will eventually be used for the spatial join.
library(tidycensus)
library(tidyverse)
library(sf)
# CRS: NAD83(2011) / Texas Centric Albers Equal Area
tx_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)
# obtain % Hispanic data by tract from the ACS Data Profile (2015-19)
pct_hispanic <-  get_acs(
  geography = "tract",
  variables = "DP05_0071P",
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)
# join the new pct_hispanic data to the top four population metro areas (tx_cbsa)
hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE)
# visualize this using Kernel density
hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
  ggplot() +
  geom_density(aes(x = estimate_tracts), color = "navy", fill = "navy", alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~NAME_metro) +
  labs(title = "Distribution of Hispanic/Latino population by Census tract",
       subtitle = "Largest metropolitan areas in Texas",
       y = "Kernel density estimate",
       x = "Percent Hispanic/Latino in Census tract")
# visualise the four metro areas by median percentage hispanic
median_by_metro <- hispanic_by_metro %>%
  group_by(NAME_metro) %>%
  summarize(median_hispanic = median(estimate_tracts, na.rm = TRUE))
plot(median_by_metro[1,]$geometry)





# SMALL AREA TIME-SERIES ANALYSIS

# Gilbert, AZ: 2010 vs 2020
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)
# CRS: NAD 83 / Arizona Central
wfh_15 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2015,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  select(estimate) %>%
  st_transform(26949)
wfh_20 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2020,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  st_transform(26949)
# two types of interpolation:
# 1: area-weighted areal interpolation
wfh_interpolate_aw <- st_interpolate_aw(
  wfh_15,
  wfh_20,
  extensive = TRUE
) %>%
  mutate(GEOID = wfh_20$GEOID)
# 2: population-weighted areal interpolation
maricopa_blocks <- blocks(
  state = "AZ",
  county = "Maricopa",
  year = 2020)
wfh_interpolate_pw <- interpolate_pw(
  wfh_15,
  wfh_20,
  to_id = "GEOID",
  extensive = TRUE,
  weights = maricopa_blocks,
  weight_column = "POP20",
  crs = 26949)





# BETTER CARTOGRAPHY WITH SPATIAL OVERLAY
library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)
ny <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  year = 2020,
  geometry = TRUE
)
ggplot(ny) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")
# we want to get TIGER/Line shapefiles instead of cartographic boundaries and then erase water
# get TIGER/Line
ny2 <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  geometry = TRUE,
  year = 2020,
  cb = FALSE #this designates TIGER/Line
) %>%
  st_transform(6538)
# erase water
ny_erase <- erase_water(ny2)
ggplot(ny_erase) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")





# SPATIAL NEIGHBORHOODS AND SPATIAL WEIGHTS MATRICES

# how can an analyst can apply exploratory spatial data analysis (ESDA) to Census data?
    # acquire a dataset on median age by Census tract in the Dallas-Fort Worth, TX metropolitan area. 
    # Census tracts in the metro area will be identified using methods introduced earlier in this chapter
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(spdep)
options(tigris_use_cache = TRUE)
# CRS: NAD83 / Texas North Central
dfw <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Dallas")) %>%
  st_transform(32138)
dfw_tracts <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(32138) %>%
  st_filter(dfw, .predicate = st_within) %>%
  na.omit()
ggplot(dfw_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  theme_void()
# queen's case contiguity "neighborhod" definiton
neighbors <- poly2nb(dfw_tracts, queen = TRUE)
summary(neighbors)
# visualize neighbors
dfw_coords <- dfw_tracts %>%
  st_centroid() %>%
  st_coordinates()
plot(dfw_tracts$geometry)
plot(neighbors,
     coords = dfw_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)
# Get the row indices of the neighbors of the Census tract at row index 1
neighbors[[1]]
# Generate spatial weights matrix
weights <- nb2listw(neighbors, style = "W")
weights$weights[[1]]





# GLOBAL AND LOCAL SPATIAL AUTOCORRELATION
# 1. Spatial lags and Moran's I
dfw_tracts$lag_estimate <- lag.listw(weights, dfw_tracts$estimate)
ggplot(dfw_tracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(title = "Median age by Census tract, Dallas-Fort Worth TX",
       x = "Median age",
       y = "Spatial lag, median age",
       caption = "Data source: 2016-2020 ACS via the tidycensus R package.")
# test corelation using moran's i
moran.test(dfw_tracts$estimate, weights)

# 2. Local spatial autocorrelation: Getis Ord-G & Hot Spots
# For Gi*, re-compute the weights with `include.self()`
localg_weights <- nb2listw(include.self(neighbors))
dfw_tracts$localG <- localG(dfw_tracts$estimate, localg_weights)
ggplot(dfw_tracts) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() +
  labs(fill = "Local Gi* statistic")
# results returned are z-scores
# choose hot spot thresholds in the statistic, calculate them with case_when(), then plot them accordingly
dfw_tracts <- dfw_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.56 ~ "High cluster",
    localG <= -2.56 ~ "Low cluster",
    TRUE ~ "Not significant"))
ggplot(dfw_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  theme_void()

# 3. LISA
set.seed(1983)
dfw_tracts$scaled_estimate <- as.numeric(scale(dfw_tracts$estimate))
dfw_lisa <- localmoran_perm(
  dfw_tracts$scaled_estimate,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))
dfw_lisa_df <- dfw_tracts %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(dfw_lisa)
# GeoDa
dfw_lisa_clusters <- dfw_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"))
# LISA Quadrant plot
color_values <- c(`High-high` = "red",
                  `High-low` = "pink",
                  `Low-low` = "blue",
                  `Low-high` = "lightblue",
                  `Not significant` = "white")
ggplot(dfw_lisa_clusters, aes(x = scaled_estimate,
                              y = lagged_estimate,
                              fill = lisa_cluster)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(x = "Median age (z-score)",
       y = "Spatial lag of median age (z-score)",
       fill = "Cluster type")
# map
ggplot(dfw_lisa_clusters, aes(fill = lisa_cluster)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(fill = "Cluster type")