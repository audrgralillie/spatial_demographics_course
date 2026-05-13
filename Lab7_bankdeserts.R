library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)

# Read in census tract data
ca.tracts <- get_acs(geography = "tract",
                     year = 2019,
                     variables = c(tpop = "B01003_001", tpopr = "B03002_001",
                                   nhwhite = "B03002_003", nhblk = "B03002_004",
                                   nhasn = "B03002_006", hisp = "B03002_012",
                                   medinc = "B19013_001"),
                     state = "CA",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)
# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
ca.tracts <- ca.tracts %>%
  rename_with(~ sub("E$", "", .x), everything()) %>% #removes the E
  mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr,
         pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,tpop, pnhwhite, pnhasn, pnhblk, phisp, medinc))
# Bring in city boundary data in CA
pl <- places(state = "CA", year = 2019, cb = TRUE)
# Keep LA city
la.city <- filter(pl, NAME == "Los Angeles")
#Clip tracts using LA boundary
la.city.tracts <- ms_clip(target = ca.tracts, clip = la.city, remove_slivers = TRUE)
#reproject to UTM NAD 83
la.city.tracts.utm <-st_transform(la.city.tracts,
                                  crs = "+proj=utm +zone=11 +datum=NAD83 +ellps=GRS80")
