install.packages("tidycensus")
library(tidycensus)

library(remotes)
install_github("Shelmith-Kariuki/rKenyaCensus")

# Set up census API key
census_api_key("93923511dc0d93634db36bcb883024d58b2a7703", install = TRUE)
readRenviron("~/.Renviron")

# Get total population by state from the 2010 Census (incl. DC and PR)
# Fetches data from Census Summary File 1 by default (sumfile = sf1)
total_population_10 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2010)

# Get 2020 Census Data
aian_2020 <- get_decennial(
  geography = "state",
  variables = "P1_005N",
  year = 2020,
  sumfile = "pl") #for PL 94-171 Redistricting summary file

# Get 2020 5-year ACS Data on the number of residents born in Mexico by state
born_in_mexico_2020 <- get_acs(
  geography = "state",
  variables = "B05006_150",
  year = 2020)

# Get 2019 1-year ACS Data on the number of residents born in Mexico by state
born_in_mexico_2019 <- get_acs(
  geography = "state",
  variables = "B05006_150",
  survey = "acs1",
  year = 2019)

# Get all variables associated with table B01001 (sex broken down by age)
# From 2016-2020 5 -year ACS
age_table_2020 <- get_acs(
  geography = "state",
  table = "B01001",
  year = 2020)



# Request data on median age from the 2016-2020 ACS for all countries in the USA
# get_acs() --> specify: geography = 'county' and state = NULL
median_age_2016_2020 <- get_acs(
  geography = "county",
  variables = "B01002_001",
  year = 2020)

# use dplyr's arrange() to sort values
arrange(median_age_2016_2020, estimate)
arrange(median_age_2016_2020, desc(estimate)) #descending order

# use dplyr's filter() to query a dataset for rows 
  # where a given condition evaluates to TRUE (similar to a WHERE clause)
filter(median_age_2016_2020, estimate >= 50)

# tidyr's separate() operates on column values
# in median_age_2016_2020, NAME is formatted as “X County, Y"
separate(
  median_age_2016_2020,
  NAME,
  into = c("county", "state"),
  sep = ", ")



# Normalizing estimated count data ffor countries that shouldn't be statistically compared
# I.e. aricopa County in Arizona is the state’s most populous
      # county with 4.3 million residents; the second-largest county, Pima, only has just over 1
      # million residents and six of the state’s 15 counties have fewer than 100,000 residents. In turn,
      # comparing Maricopa’s estimates with those of smaller counties in the state would often be
      # inappropriate.
# = dividing estimated count data by the overall population from which the sub-group is derived
# supply a variable ID to the summary_var parameter in both 
      # get_acs() and
      # get_decennial()
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012")
az_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = race_vars,
  summary_var = "B03002_001",
  year = 2020)
# and use dplyr's mutate() to calculate new column: percent
az_race_percent <- az_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)




# The split-apply-combine model for data analysis is important for demographic data analysis
# Read this function as: 
# Create a new dataset "largest group", by using the az_race_dataset
      # THEN grouping the dataset by the NAME column
      # THEN filtering for rows that are equal to the maximum percent value for each group
largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))
# pair with summarize() to condensed dataset
az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))

# to calulate new custom groups to address specific questions
# i.e. the B19001 represents banded incomes (< $10,000/yr, $10-000--$19,000/year...)
mn_hh_income <- get_acs(
  geography = "county",
  table = "B19001",
  state = "MN",
  year = 2016)
# this table includes household income categories for each county in the rows
# what if we need only three income categories? (ex. <$35k, $35k-$75k, >= $75k)
# add an 'incgroup' column to the dataset
mn_hh_income_recode <- mn_hh_income %>%
  filter(variable != "B19001_001") %>%
  mutate(incgroup = case_when( # add column IF:
    variable < "B19001_008" ~ "below35k", # assign 'below35k' to all rows with a variable value 
                                          # that comes before 'B19001_008' (in this case --> B19001_002 (income < $10k/yr))
    variable < "B19001_013" ~ "bw35kand75k", # for all rows not accounted for by the first condition
    TRUE ~ "above75k")) # "all other values"
# and then to group sums by income bands (by group by county)
mn_group_sums <- mn_hh_income_recode %>%
  group_by(GEOID, incgroup) %>%
  summarize(estimate = sum(estimate))




# Comparing ACS estimates over time
# Safest bet: use ACS Comparison Profile Tables
ak_income_compare <- get_acs(
  geography = "county",
  variables = c(income15 = "CP03_2015_062", # 'comparison year' = 2015
                income20 = "CP03_2020_062"),
  state = "AK",
  year = 2020)
# Detailed Tables may be even safer--they ensure that variable IDs remain consistent
# E.g. Colorado counties, 2010-2019
      # want estimates of populations age 25+ who have finished a 4-year degree or 
      # graduate degrees, by sex
college_vars <- c("B15002_015",
                  "B15002_016",
                  "B15002_017",
                  "B15002_018",
                  "B15002_032",
                  "B15002_033",
                  "B15002_034",
                  "B15002_035") #these are the proper variables for what we want
# the purrr package includes a variety of functions that are designed to integrate 
      # well in workflows that require iteration and use other
      # tidyverse tools
# the map() functions iterate over values and try to return a desired result
years <- 2010:2019 # numberic vector of years is defined
names(years) <- years
college_by_year_co <- map_dfr(years, ~{. # map_dfr will iterate over argument 'years'
  get_acs(
    geography = "county",
    variables = college_vars,
    state = "CO",
    summary_var = "B15002_001",
    survey = "acs1",
    year = .x)}, .id = "year") #'x' takes on each value of years sequentially
                               #'.id' creates a new column in the output df that contains values equivalent to the names of the input, in this case 'year'
                               #setting .id = 'year' tells map_dfr() to name the new column that will contain these values as 'year'
# view result:
college_by_year_co %>%
  arrange(NAME, variable, year)
# calculate percentage, spread the years across columns
# suitable for a spreadsheet
percent_college_by_year_co <- college_by_year %>%
  group_by(NAME, year) %>%
  summarize(numerator = sum(estimate),
            denominator = first(summary_est)) %>%
  mutate(pct_college = 100 * (numerator / denominator)) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              values_from = pct_college)








# ACS Margins of Error
# Default confidence level = 90%
get_acs(
  geography = "county",
  state = "Rhode Island",
  variables = "B19013_001",
  year = 2020)
# But you can manipulate it!
get_acs(
  geography = "county",
  state = "Rhode Island",
  variables = "B19013_001",
  year = 2020,
  moe_level = 99)