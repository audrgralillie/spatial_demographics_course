# Lab 2
# Audrey Lillie
# 04/08/2026

library(tidycensus)
library(tidyverse)
library(remotes)
library(dplyr)

# =============================================================================

total_pop_2020 <- read.csv("/Users/audreylillie/Desktop/spatialdemograph/R_Projects/Lab2/nhgis0001_csv/nhgis0001_ds258_2020_state.csv")
#visualize
total_pop_2020_bar <- ggplot() + geom_col(data = total_pop_2020, 
                    aes(x = reorder(STATE, U7H001), 
                        y = U7H001), 
                        fill = "steelblue") +  coord_flip() + labs(x = "State", 
                        y = "Total Population", 
                        title = "Total Population by State 2020")
# same data from tidycensus
total_pop_2020_TC <- get_decennial(geography = "state", variables = "P1_001N", year = 2020, geometry = FALSE)
# and visualize
total_pop_2020_bar_TC <- ggplot() + geom_col(data = total_pop_2020_TC, 
                                          aes(x = reorder(NAME, value), 
                                              y = value), 
                                              fill = "steelblue") +  coord_flip() + labs(x = "State", 
                                              y = "Total Population", 
                                              title = "Total Population by State 2020")
# P1: Bachelor's degree
# load proper variables
mi_college20 <- get_acs(
  geography = "county",
  variables = "DP02_0068P", # percent of population age 25 and up with a bachelor’s degree
  state = "MI",
  survey = "acs5",
  year = 2020)

# arrange by percentage (estimate)
arrange(mi_college20, estimate)
arrange(mi_college20, desc(estimate))

#id median of variable "estimate"
# Returns a single numeric value
mi_college20 %>% 
  summarize(estimate = median(estimate, na.rm = TRUE))


# P2: Average Household Income per county: B05006_150
# Learn a little bit about Michigan's counties!
michigan <- get_decennial(
  state = "Michigan",
  geography = "county",
  variables = c(totalpop = "P1_001N"),
  year = 2020) %>%
  arrange(desc(value))
# There are 83 counties in Michigan
# range from population 2046 to 1,793,561

# Get Income Data
michigan_income2020 <- get_acs(
  state = "Michigan",
  geography = "county",
  variables = c(hhincome = "B19013_001"), # number of residents
  year = 2020) %>%
  mutate(NAME = str_remove(NAME, " County, 4Michigan"))

# Plot Household Income
ggplot(michigan_income2020, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Counties in Michigan",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = label_dollar())

# Add Error Bars for MOE
michigan_income2020 %>%
  arrange(desc(moe))
# plot
ggplot(michigan_income2020, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "slateblue") +
  theme_minimal(base_size = 12.5) +
  labs(title = "2020 Median Household Income",
       subtitle = "Counties in Michigan",
       x = "2020 ACS estimate",
       y = "") +
  scale_x_continuous(labels = label_dollar())




# P3: Population pyramid
library(dplyr)
library(stringr)
# get data
colorado <- get_estimates(
  geography = "state",
  state = "CO",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019)

# filter out "both sexes"
colorado_filtered <- colorado %>%
  filter(str_detect(AGEGROUP, "^Age \\d+ to \\d+ years$"),
         SEX %in% c("Male", "Female")) %>%
  mutate(population = ifelse(SEX == "Male", -value, value))

# order ages
colorado_filtered <- colorado_filtered %>%
  mutate(age_start = as.numeric(str_extract(AGEGROUP, "\\d+"))) %>%
  arrange(age_start)

colorado_filtered$AGEGROUP <- factor(
  colorado_filtered$AGEGROUP,
  levels = unique(colorado_filtered$AGEGROUP))
#plot
library(ggplot2)

options(scipen = 999)
colorado__pyramid <- ggplot(colorado_filtered,
                            aes(x = AGEGROUP, y = population, fill = SEX)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_x_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  scale_fill_manual(values = c("lightgreen", "lightsalmon")) +
  labs(
    x = "Age Group",
    y = "Population",
    title = "2019 Population Structure in Colorado",
    caption = "Source: US Census Bureau"
  ) +
  theme_minimal()
colorado__pyramid