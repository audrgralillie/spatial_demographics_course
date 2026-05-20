library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(rmapshaper)
library(flextable)

# Tract-level data
# Bring in 2006-2010 census tract data using the Census API
ca.tracts10 <- get_acs(geography = "tract",
                       year = 2010,
                       variables = c(medinc10 = "B19013_001", rent10 = "B25064_001",
                                     houseval10 = "B25077_001", bachm = "B15002_015",
                                     mastersm = "B15002_016", profm = "B15002_017",
                                     phdm = "B15002_018", bachf = "B15002_032",
                                     mastersf = "B15002_033", proff = "B15002_034",
                                     phdf = "B15002_035", totcol = "B15002_001"),
                       state = "CA",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE)
# Rename, calculate and keep essential vars.
ca.tracts10 <- ca.tracts10 %>%
  rename_with(~ sub("E$", "", .x), everything()) %>%
  mutate(pcol10 = 100*(bachm+mastersm+profm+phdm+bachf+mastersf+proff+phdf)/totcol) %>%
  select(c(GEOID,medinc10, rent10, houseval10, pcol10))

# keep only Fresno tracts
# Bring in city boundaries. We'll use the boundaries for the final year, 2019
pl <- places(state = "CA", year = 2019, cb = TRUE)
# Keep Fresno city
fresno <- pl %>%
  filter(NAME == "Fresno")
#Keep tracts in Fresno
fresno.tracts <- ms_clip(target = ca.tracts10, clip = fresno, remove_slivers = TRUE)

# same but for next set
# Bring in 2015-2019 census tract data using the Census API
ca.tracts19 <- get_acs(geography = "tract",
                       year = 2019,
                       variables = c(tpop = "B03002_001",
                                     white = "B03002_003", black = "B03002_004",
                                     asian = "B03002_006", hisp = "B03002_012",
                                     medinc19 = "B19013_001", rent19 = "B25064_001",
                                     houseval19 = "B25077_001", bach = "B15003_022",
                                     masters = "B15003_023", prof = "B15003_024",
                                     phd = "B15003_025", totcol = "B15003_001"),
                       state = "CA",
                       survey = "acs5",
                       output = "wide")
# Rename, calculate and keep essential vars.
ca.tracts19 <- ca.tracts19 %>%
  rename_with(~ sub("E$", "", .x), everything()) %>%
  mutate(pwhite19 = 100*(white/tpop), pasian19 = 100*(asian/tpop),
         pblack19 = 100*(black/tpop), phisp19 = 100*(hisp/tpop),
         pcol19 = 100*(bach+masters+prof+phd)/totcol) %>%
  select(c(GEOID,pwhite19, pasian19, pblack19, phisp19,
           medinc19, rent19, houseval19, pcol19))

# join two tract level data objects
fresno.tracts <- fresno.tracts %>%
  left_join(ca.tracts19, by = "GEOID")
glimpse(fresno.tracts)

# city-level data
# Bring in census tract data using the Census API
ca.places10 <- get_acs(geography = "place",
                       year = 2010,
                       variables = c(medincc10 = "B19013_001", rentc10 = "B25064_001",
                                     housevalc10 = "B25077_001", bachm = "B15002_015",
                                     mastersm = "B15002_016", profm = "B15002_017",
                                     phdm = "B15002_018", bachf = "B15002_032",
                                     mastersf = "B15002_033", proff = "B15002_034",
                                     phdf = "B15002_035", totcol = "B15002_001"),
                       state = "CA",
                       survey = "acs5",
                       output = "wide")
# Keep Fresno. Calculate and keep essential vars. Also take out zero population tracts
ca.places10 <- ca.places10 %>%
  filter(NAME == "Fresno city, California") %>%
  rename_with(~ sub("E$", "", .x), everything()) %>%
  mutate(pcolc10 = 100*(bachm+mastersm+profm+phdm+bachf+mastersf+proff+phdf)/totcol) %>%
  select(c(GEOID,medincc10, rentc10, housevalc10, pcolc10))
# Bring in census tract data using the Census API
ca.places19 <- get_acs(geography = "place",
                       year = 2019,
                       variables = c(medincc19 = "B19013_001", rentc19 = "B25064_001",
                                     housevalc19 = "B25077_001", bachc = "B15003_022",
                                     mastersc = "B15003_023", profc = "B15003_024",
                                     phdc = "B15003_025", totcolc = "B15003_001"),
                       state = "CA",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE)
# Keep Fresno. Calculate and keep essential vars. Also keep just Fresno
fresno.city <- ca.places19 %>%
  filter(NAME == "Fresno city, California") %>%
  rename_with(~ sub("E$", "", .x), everything()) %>%
  mutate(pcolc19 = 100*(bachc+mastersc+profc+phdc)/totcolc) %>%
  select(c(GEOID,medincc19, rentc19, housevalc19, pcolc19))

# join city-level objects
fresno.city <- fresno.city %>%
  left_join(ca.places10, by = "GEOID")
glimpse(fresno.city)

# join tract and city data
fresno.tracts <- fresno.tracts %>%
  st_join(fresno.city, left=FALSE)
names(fresno.tracts)
glimpse(fresno.tracts)


# measuring gentrification
# gentrification eligible tracts?
fresno.tracts <- fresno.tracts %>%
  mutate(eligible = ifelse(medinc10 < medincc10,
                           "Eligible",
                           "Not Eligible"))
# percent of neighborhoods eligible to gentrify?
fresno.tracts %>%
  group_by(eligible) %>%
  summarize(n = n()) %>%
  mutate(Percent = 100*(n / sum(n))) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  flextable() %>%
  colformat_double(digits = 2)
# actually gentrifying tracts (of the ones eligible)
    # gentrifies if its change between 2006-2010 and 2015-2019 in median gross rent or median housing value is greater than the
    # change in the city and the change in its percent of residents that have a college degree is more than the change in the city.
fresno.tracts <- fresno.tracts %>%
  mutate(rentch = rent19-rent10,
         housech = houseval19-houseval10,
         pcolch = pcol19-pcol10)
fresno.tracts <- fresno.tracts %>%
  mutate(rentchc = rentc19-rentc10,
         housechc = housevalc19-housevalc10,
         pcolchc = pcolc19-pcolc10)
fresno.tracts <- fresno.tracts %>%
  mutate(gent = ifelse(eligible == "Not Eligible", "Not Eligible",
                       ifelse(eligible == "Eligible" & pcolch > pcolchc &
                                (rentch > rentchc | housech > housechc), "Gentrifying",
                              "Not Gentrifying")))

# examining gentrification
# table:
fresno.tracts %>%
  group_by(gent) %>%
  summarize(n = n()) %>%
  mutate(Percent = 100*(n / sum(n))) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  flextable() %>%
  colformat_double(digits = 2)
# map it:
tm_shape(fresno.tracts, unit = "mi") +
  tm_polygons(fill = "gent",
              fill.scale = tm_scale(style = "cat",
                                    values = "paired"),
              fill.legend = tm_legend(title = "", frame = FALSE),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2), text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("2010-2019 Gentrification Status in Fresno") +
  tm_layout(frame = FALSE, scale = 0.7)
# mean percentages of residents that are Hispanic and non-Hispanic white, Black, and Asian in 2010 in each of
    #the gentrification categories?
fresno.tracts %>%
  st_drop_geometry() %>%
  filter(is.na(gent) == FALSE) %>%
  group_by(gent) %>%
  summarize("% White" = mean(pwhite19),
            "% Black" = mean(pblack19),
            "% Hispanic" = mean(phisp19),
            "% Asian" = mean(pasian19)) %>%
  flextable() %>%
  colformat_double(digits = 2)
