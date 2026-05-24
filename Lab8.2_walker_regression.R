# Walker Chapter 8.2: Regression modeling with US Census Data
# TOPIC: median home value by Census tract in the Denver Metro area
    # Denver-Aurora-Lakewood
library(tidycensus)
library(sf)
library(dplyr)
library(tidyverse)


denver_metro_counties <- c("Adams", "Arapahoe", "Broomfield",
                  "Clear Creek", "Denver", "Douglas", "Elbert",
                  "Gilpin", "Jefferson", "Park")

variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)
denver_metro_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "CO",
  county = denver_metro_counties,
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(26954) # EPSG:26954: Meters for Denver metro


# inspect the outcome variable (median home value)
library(patchwork)
# Corrected code
denver_mhv_map <- ggplot(denver_metro_data, aes(fill = median_valueE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(labels = scales::label_dollar()) + 
  theme_void() + 
  labs(fill = "Median home value")

denver_mhv_histogram <- ggplot(denver_metro_data, aes(x = median_valueE)) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) + 
  theme_minimal() + 
  # Note: label_number_si is deprecated; use label_number(scale_cut = cut_si())
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))) + 
  labs(x = "Median home value")
denver_mhv_map + denver_mhv_histogram



# denver data isn't horribly skewed, but let's try taking the data log anyway
denver_mhv_map_log <- ggplot(denver_metro_data, aes(fill = log(median_valueE))) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Median home\nvalue (log)")
denver_mhv_histogram_log <- ggplot(denver_metro_data, aes(x = log(median_valueE))) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous() +
  labs(x = "Median home value (log)")
denver_mhv_map_log + denver_mhv_histogram_log




# feature engineering
library(sf)
library(units)
denver_metro_data_for_model <- denver_metro_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.),
                                            "1/km2")), #calculate area of census tract
                                                      # divide total population by this 
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()




# regression model number 1
# fit the model
denver_regression_formula <- paste0("log(median_value) ~ median_rooms + median_income + ",
                  "pct_college + pct_foreign_born + pct_white + ",
                  "median_age + median_structure_age + ",
                  "percent_ooh + pop_density + total_population")
denver_model1 <- lm(formula = denver_regression_formula, data = denver_metro_data_for_model)
summary(denver_model1)
# model explains 0.6251 percent of median_value (multiple R-squared value)
# so, next:
# colinearity matrix
library(corrr)
denver_metro_estimates <- denver_metro_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()
denver_metro_correlations <- correlate(denver_metro_estimates, method = "pearson")
# plot it
network_plot(denver_metro_correlations)
# vif
library(car)
vif(denver_model1)
# vif of 1 = no collinearity
# vif > 5 = collinearity w/ problematic influence on model interpretation
# most problematic (highest collinearity) variable = pct_white (6.177743):
    # rerun model without it?
denver_regression_formula2 <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                   "pct_foreign_born + median_income + median_age + ",
                   "median_structure_age + percent_ooh + pop_density + ",
                   "total_population")
denver_model2 <- lm(formula = denver_regression_formula2, data = denver_metro_data_for_model)
summary(denver_model2)
# model still explains explains 0.6293 (not enough) percent of median_value (multiple R-squared value)
# rerun vif
vif(denver_model2)
# median_rooms and median_income still above 5
# drop median_rooms
denver_regression_formula3 <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                                     "pct_foreign_born + median_age + ",
                                     "median_structure_age + percent_ooh + pop_density + ",
                                     "total_population")
denver_model3 <- lm(formula = denver_regression_formula3, data = denver_metro_data_for_model)
summary(denver_model3)
vif(denver_model3)
# everything below 5
# can we add pct_white back in?
denver_regression_formula4 <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                                     "pct_foreign_born + median_age + pct_white +",
                                     "median_structure_age + percent_ooh + pop_density + ",
                                     "total_population")
denver_model4 <- lm(formula = denver_regression_formula4, data = denver_metro_data_for_model)
summary(denver_model4)
vif(denver_model4)




# Principle Component Analysis
den_pca <- prcomp(
  formula = ~.,
  data = denver_metro_estimates,
  scale. = TRUE,
  center = TRUE
)
summary(den_pca)
# create a tibble table
den_pca_tibble <- den_pca$rotation %>%
  as_tibble(rownames = "predictor")
# plot top five predictors
den_pc1 <- den_pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "purple4", color = "purple", alpha = 0.5) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL, x = "Value") +
  theme_minimal()
den_components <- predict(den_pca, denver_metro_estimates)
# and map principle component 1
den_pca <- denver_metro_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(den_components)
den_pca1_map <- ggplot(den_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()
