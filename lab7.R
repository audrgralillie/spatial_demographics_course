#Lab7:Brazil's Segregation but for Detroit, MI
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(rmapshaper)
library(flextable)

# Bring in 2019-2023 census tract data using the Census API
dtown.tracts <- get_acs(geography = "tract",
                     year = 2023,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "MI",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)
# Calculate, rename and keep essential vars.
dtown.tracts <- dtown.tracts %>%
  mutate(pwhite = 100*(whiteE/tpopE), pasian = 100*(asianE/tpopE),
         pblack = 100*(blackE/tpopE), phisp = 100*(hispE/tpopE)) %>%
  rename(white = whiteE, asian = asianE, black = blackE,
         hisp = hispE, tpop = tpopE) %>%
  select(GEOID,tpop, pwhite, pasian, pblack, phisp,
         white, asian, black, hisp)
# Bring in city boundaries
mi_pl <- places(state = "MI", year = 2023, cb = TRUE)
# Keep four large cities in CA
large.cities <- mi_pl %>%
  filter(NAME == "Detroit" |
           NAME == "Grand Rapids" | NAME == "Ann Arbor" |
           NAME == "Lansing")
#Clip tracts in large cities
large.tracts <- ms_clip(target = dtown.tracts,
                        clip = large.cities,
                        remove_slivers = TRUE)

glimpse(large.tracts)
names(large.tracts)
names(large.cities)

sf::sf_use_s2(FALSE)
large.tracts <- large.tracts %>%
  st_join(large.cities)
sf::sf_use_s2(TRUE)
names(large.tracts) # lots of new variables that we don't need

# remove duplicates (ending in .x and .y)
large.tracts <- large.tracts %>%
  select(-(STATEFP:GEOIDFQ), -(NAMELSAD:AWATER))
names(large.tracts) # check





# mapping



# % hispanic in Detroit
large.tracts %>%
  filter(NAME == "Detroit") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "phisp",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent Hispanic in Detroit City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)

# % black in Detroit
large.tracts %>%
  filter(NAME == "Detroit") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "pblack",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent Black in Detroit City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)

# % asian in Detroit
large.tracts %>%
  filter(NAME == "Detroit") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "pasian",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent Asian in Detroit City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)

# how do these compare to non-hispanic white?
large.tracts %>%
  filter(NAME == "Detroit") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "pwhite",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent White in Fresno City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)






# Dissimilarity Index
# calculate total population of race/ethnic group and for each city
large.tracts <- large.tracts %>%
  group_by(NAME) %>%
  mutate(whitec = sum(white), asianc = sum(asian),
         blackc = sum(black), hispc = sum(hisp),
         tpopc = sum(tpop))

# calculate tract-level contributions to the index
large.tracts <- large.tracts %>%
  mutate(d.wb = abs(black/blackc-white/whitec),
         d.wa = abs(asian/asianc-white/whitec),
         d.wh = abs(hisp/hispc-white/whitec))

# final index values
# Dissimilarity indices for 
#Black/White (BWD), 
#Asian/White (AWD), and 
#Hispanic/White (HWD).
sf::sf_use_s2(FALSE)
large.tracts %>%
  summarize(BWD = 0.5*sum(d.wb, na.rm=TRUE), AWD = 0.5*sum(d.wa, na.rm=TRUE),
            HWD = 0.5*sum(d.wh, na.rm=TRUE))

# (1) Drop the geometry column using
# st_drop_geometry() , which is a part of the sf package, thus making the object large.tracts no longer spatial; (2) use the
# flextable() function to make a nicely formatted table; and (3) save the resulting table in an object we named dis.table.
# The st_drop_geometry() function removes the geometry variable, and thus makes the object large.tracts no longer
# spatial. We save the table into an object named dis.table
dis.table <- large.tracts %>%
  summarize(BWD = 0.5*sum(d.wb, na.rm=TRUE), AWD = 0.5*sum(d.wa, na.rm=TRUE),
            HWD = 0.5*sum(d.wh, na.rm=TRUE)) %>%
  st_drop_geometry() %>%
  flextable()
dis.table %>%
  colformat_double(j = c("BWD", "AWD", "HWD"), digits = 3)

# Interaction Index, P*
int.table <-large.tracts %>%
  mutate(i.wb = (black/blackc)*(white/tpop),
         i.wa = (asian/asianc)*(white/tpop),
         i.wh = (hisp/hispc)*(white/tpop)) %>%
  summarize(BWI = sum(i.wb, na.rm=TRUE), AWI = sum(i.wa, na.rm=TRUE),
            HWI = sum(i.wh, na.rm=TRUE)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  flextable()
int.table %>%
  colformat_double(j = c("BWI", "AWI", "HWI"), digits = 3)
sf::sf_use_s2(TRUE)


#Location Quotient (LQRSS)
# neighborhood-level
dtown.tracts <- large.tracts %>%
  filter(NAME == "Detroit") %>%
  mutate(blklq = (black/tpop)/(blackc/tpopc),
         asnlq = (asian/tpop)/(asianc/tpopc),
         hisplq = (hisp/tpop)/(hispc/tpopc),
         whitelq = (white/tpop)/(whitec/tpopc))
# visualize: histogram
dtown.tracts %>%
  ggplot() +
  geom_histogram(mapping = aes(x=blklq), na.rm=TRUE) +
  xlab("Hispanic Location Quotient")
# map it
tmap_mode("view")
tm_shape(dtown.tracts, unit = "mi") +
  tm_polygons(fill = "asnlq",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = "Asian Location Quotient"))
