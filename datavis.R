# Data Visualization
#1. Get data
library(tidycensus)
# SINGLE VARIABLE
# Data on Michigan's median income and median age, 2020
mi_wide_income_age <- get_acs(
  geography = "county",
  state = "Michigan",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  output = "wide",
  year = 2020)

# plot median household income by county in Michigan
# y-axis: number of counties
# x-axis: median income
options(scipen = 999) # tells R to avoid using scientific notation
ggplot(mi_wide_income_age, aes(x = medincE)) +
  geom_histogram() # HISTOGRAM

# by default, ggplot organizes data into 30 bins (change with bins= parameter)
options(scipen = 999) # tells R to avoid using scientific notation
ggplot(mi_wide_income_age, aes(x = medincE)) +
  geom_histogram(bins = 15)  # HISTOGRAM, 15 BINS

# for box and whisker plot:
# y-axis: median income
ggplot(mi_wide_income_age, aes(y = medincE)) +
  geom_boxplot() # BOX AND WHISKER PLOT


# MULTIVARIATE RELATIONSHIPS
# scatter plot
# age versus income in Michigan, 2020
ggplot(mi_wide_income_age, aes(x = medageE, y = medincE)) +
  geom_point()


ggplot(mi_wide_income_age, aes(x = medageE, y = medincE)) +
  geom_point() +
  geom_smooth(method = "lm") # draws linear fit line




# CUSTOMIZING PLOTS
# Using the percent of commuters that take public transportation to work 
# for the largest metropolitan areas in the United States.
# Data: 2019 1-year ACS Data Profile, variable DP03_0021P
# Use tidyverse to sort data in descending order of a summary variable 
# representing total population and then retain the 
# 20 largest metro areas using slice_max()
library(tidycensus)
library(tidyverse)
metros <- get_acs(
  geography = "cbsa",
  variables = "DP03_0021P",
  summary_var = "B01003_001",
  survey = "acs1",
  year = 2019) %>%
  slice_max(summary_est, n = 20)

# For this data:
# bar chart
ggplot(metros, aes(x = NAME, y = estimate)) +
  geom_col()

# Make nicer:
# We want to convert "Atlanta-Sandy Springs-Roswell, GA Metro Area" to “Atlanta”
# Use str_remove(), found in the stringr package, using regular expressions
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col()
ggplot(metros, aes(x = NAME, y = estimate)) +
  geom_col()
# Add title, subtitle, modify X and Y lables
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Public transit commute share",
       subtitle = "2019 1-year ACS estimates",
       y = "",
       x = "ACS estimate",
       caption = "Source: ACS Data Profile; tidycensus R package")
ggplot(metros, aes(x = NAME, y = estimate)) +
  geom_col()
# More
library(scales)
metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
  geom_col(color = "navy", fill = "navy",
           alpha = 0.5, width = 0.85) +
  theme_minimal(base_size = 12, base_family = "Verdana") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Public transit commute share",
       subtitle = "2019 1-year ACS estimates",
       y = "",
       x = "ACS estimate",
       caption = "Source: ACS Data Profile variable; tidycensus R package")




# Visualizing Margins of Error (MOEs)
# Data Setup
# Comparing median household incomes of counties in Michigan (2016-2020) 
# Good place to start: learn about Michigan counties’ basic info 
#(number, total population)
michigan_income2020 <- get_acs(
  state = "Michigan",
  geography = "county",
  variables = c(hhincome = "B19013_001"), # number of residents
  year = 2020) %>%
  mutate(NAME = str_remove(NAME, " County, Michigan"))

# Plot Household Income
ggplot(michigan_income2020, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median Household Income",
       subtitle = "Counties in Michigan",
       x = "",
       y = "ACS estimate") +
  theme_minimal(base_size = 12.5) +
  scale_x_continuous(labels = label_dollar())




# VISUALIZING ACS ESTIMATES OVER TIME
# 1-year ACS
# set up data
years <- 2005:2019
names(years) <- years

jeffco_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "CO",
    county = "Jefferson",
    year = .x,
    survey = "acs1")
  
  # plot
  ggplot(jeffco_value, aes(x = year, y = estimate, group = 1)) +
    geom_line() +
    geom_point()
}, .id = "year")

# plot cleanup
ggplot(jeffco_value, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe),
              fill = "navy",
              alpha = 0.4) +
  geom_line(color = "navy") +
  geom_point(color = "navy", size = 2) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = label_dollar(scale = .001, suffix = "k")) +
  labs(title = "Median home value in Jefferson County, CO",
       x = "Year",
       y = "ACS estimate",
       caption = "Shaded area represents margin of error around the ACS estimate")






# POPULATION PYRAMIDS
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