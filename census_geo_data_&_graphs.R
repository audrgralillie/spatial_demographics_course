#TIGRIS PACKAGE
install.packages("tigris")
library(tigris)

# states
st <- states()
# returns data for year 2024 (sends a message)

# look at classes
class(st)
# returns [1] "sf"         "data.frame"

# print first 10 rows
st
          # Simple feature collection with 56 features and 15 fields
          # Geometry type: MULTIPOLYGON
          # Dimension:     XY
          # Bounding box:  xmin: -179.2311 ymin: -14.60181 xmax: 179.8597 ymax: 71.43979
          # Geodetic CRS:  NAD83
          # First 10 features:
          #   REGION DIVISION STATEFP  STATENS GEOID     GEOIDFQ STUSPS           NAME LSAD MTFCC FUNCSTAT        ALAND
          # 1       3        5      54 01779805    54 0400000US54     WV  West Virginia   00 G4000        A  62266513826
          # 2       3        5      12 00294478    12 0400000US12     FL        Florida   00 G4000        A 138965379385
          # 3       2        3      17 01779784    17 0400000US17     IL       Illinois   00 G4000        A 143778206717
          # 4       2        4      27 00662849    27 0400000US27     MN      Minnesota   00 G4000        A 206244791203
          # 5       3        5      24 01714934    24 0400000US24     MD       Maryland   00 G4000        A  25151223822
          # 6       1        1      44 01219835    44 0400000US44     RI   Rhode Island   00 G4000        A   2677768885
          # 7       4        8      16 01779783    16 0400000US16     ID          Idaho   00 G4000        A 214050504522
          # 8       1        1      33 01779794    33 0400000US33     NH  New Hampshire   00 G4000        A  23190211616
          # 9       3        5      37 01027616    37 0400000US37     NC North Carolina   00 G4000        A 125935965771
          # 10      1        1      50 01779802    50 0400000US50     VT        Vermont   00 G4000        A  23872594714

#view the geometry column
plot(st$geometry)
#let it load for a second!


# return counties from a specific state
nm_counties <- counties("NM")
plot(nm_counties$geometry)
# and plot them

# you can request Census tract info boundaries for a given county in New Mexico 
  # with the corresponding tracts() function
la_tracts <- tracts("NM", "Los Alamos")
plot(la_tracts$geometry)


#users can request geographic things like roads/waterways
# request los alamos county water area using area_water()
la_water <- area_water("NM", "Los Alamos")
plot(la_water$geometry)



# tigris returns VECTOR DATA

# we can acquire landmark POINTS data that the Census uses for enumerations
dc_landmarks <- landmarks("DC", type = "point")
plot(dc_landmarks$geometry)

# or roads, which are LINE vector data
dc_roads <- primary_secondary_roads("DC")
plot(dc_roads$geometry)

# Block groups are a type of POLYGON vector data
dc_block_groups <- block_groups("DC")
plot(dc_block_groups$geometry)





# Plotting geographic data
library(ggplot2) # incldudes geom_sf

#default
ggplot(la_tracts) +
  geom_sf()

# strip gray grid and axes
ggplot(la_tracts) +
  geom_sf() +
  theme_void()

# for 'faceted' comparison plots: can use patchwork
library(patchwork)

la_block_groups <- block_groups("NM", "Los Alamos")

gg1 <- ggplot(la_tracts) +
  geom_sf() +
  theme_void() +
  labs(title = "Census tracts")
gg2 <- ggplot(la_block_groups) +
  geom_sf() +
  theme_void() +
  labs(title = "Block groups")

gg1 + gg2

# or, vertically:
gg1 / gg2



# map interaction
# the mapview package allows us to visualize geographic data on an interactive, zoomable map
install.packages("mapview")
library(mapview)
mapview(la_tracts)
# can move around in the output!! --->

# Census Bureau also produces CARTOGRAPHIC BOUNDARY SHAPEFILES
# that are often better than the TIGER/Line files
mi_counties <- counties("MI")
mi_counties_cb <- counties("MI", cb = TRUE)
mi_tiger_gg <- ggplot(mi_counties) +
  geom_sf() +
  theme_void() +
  labs(title = "TIGER/Line")
mi_cb_gg <- ggplot(mi_counties_cb) +
  geom_sf() +
  theme_void() +
  labs(title = "Cartographic boundary")
mi_tiger_gg + mi_cb_gg

# cache tigris data to store/download huge files
# computer cache instead of temporary 
options(tigris_use_cache = TRUE)
rappdirs::user_cache_dir("tigris")



# quantifying geographic changes over years
library(tidyverse)
library(patchwork)
library(glue)
options(tigris_use_cache = TRUE)
yearly_plots <- map(seq(1990, 2020, 10), ~{
  year_tracts <- tracts(state = "TX", county = "Tarrant", year = .x, cb = TRUE)
  ggplot(year_tracts) +
    geom_sf() +
    theme_void() +
    labs(title = glue("{.x}: {nrow(year_tracts)} tracts"))})
#facet the plots
(yearly_plots[[1]] + yearly_plots[[2]]) /
  (yearly_plots[[3]] + yearly_plots[[4]])




# counting total number of... 

# 2019 and later: get number of census blocks (or tracts, or places...)
us_bgs_2020 <- block_groups(cb = TRUE, year = 2020)
nrow(us_bgs_2020)
# [1] 242298

# 2018 and earlier?
state_codes <- c(state.abb, "DC", "PR")
us_bgs_2018 <- map_dfr(
  state_codes,
  ~block_groups(
    state = .x,
    cb = TRUE,
    year = 2018))
nrow(us_bgs_2018)
# [1] 220016


# COORDINATES
# Check coordinate reference system
library(sf)
fl_counties <- counties("FL", cb = TRUE)
st_crs(fl_counties)
      # Coordinate Reference System:
      #   User input: NAD83 
      # wkt:
      #   GEOGCRS["NAD83",
      #           DATUM["North American Datum 1983",
      #                 ELLIPSOID["GRS 1980",6378137,298.257222101,
      # ....

# have R suggest some proper CRSs
install.packages("crsuggest")
library(crsuggest)
fl_crs <- suggest_crs(fl_counties)
    # # A tibble: 10 × 6
    # crs_code crs_name                              crs_type  crs_gcs crs_units crs_proj4                         
    # <chr>    <chr>                                 <chr>       <dbl> <chr>     <chr>                             
    # 1 6439     NAD83(2011) / Florida GDL Albers      projected    6318 m         +proj=aea +lat_0=24 +lon_0=-84 +l…
    # 2 3513     NAD83(NSRS2007) / Florida GDL Albers  projected    4759 m         +proj=aea +lat_0=24 +lon_0=-84 +l…
    # 3 3087     NAD83(HARN) / Florida GDL Albers      projected    4152 m         +proj=aea +lat_0=24 +lon_0=-84 +l…
    # .....

# and then implement your choice using the crs code
fl_projected <- st_transform(fl_counties, crs = 3087)
head(fl_projected) # use to show!

# plot with coord_sf() to specify which coordinate system to use!
options(scipen = 999)
ggplot(fl_counties) +
  geom_sf() +
  coord_sf(crs = 3087, datum = 3087)




# working with geometries
# take a US states shapefile obtained with tigris at low resolution and use ggplot2 to visualize it in the default geographic CRS, NAD 1983
us_states <- states(cb = TRUE, resolution = "20m")
ggplot(us_states) +
  geom_sf() +
  coord_sf(crs = 'ESRI:102003') +
  theme_void()

# even better: use shift_geometry
us_states_shifted <- shift_geometry(us_states)
ggplot(us_states_shifted) +
  geom_sf() +
  theme_void()

# to preserve area:
us_states_outside <- shift_geometry(us_states,
                                    preserve_area = TRUE,
                                    position = "outside")
ggplot(us_states_outside) +
  geom_sf() +
  theme_void()



# polygons to points
# Texas: we want its cities (and its shape)

# polygon-cities
tx_places <- places("TX", cb = TRUE) %>%
  filter(NAME %in% c("Dallas", "Fort Worth", "Houston",
                     "Austin", "San Antonio", "El Paso")) %>%
  st_transform(6580)
tx_outline <- states(cb = TRUE) %>%
  filter(NAME == "Texas") %>%
  st_transform(6580)
ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_places, fill = "red", color = NA) +
  theme_void()

# point cities
tx_centroids <- st_centroid(tx_places)
ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_centroids, color = "red", size = 3) +
  theme_void()


# exploding multipolygon geometries into single parts
# lee county, florida
lee <- fl_projected %>%
  filter(NAME == "Lee")
mapview(lee)

# “cast” Lee County as a POLYGON object which will create a separate row for each non-contiguous area
lee_singlepart <- st_cast(lee, "POLYGON")
lee_singlepart

sanibel <- lee_singlepart[2,]
mapview(sanibel)
