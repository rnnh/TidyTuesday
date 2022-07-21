# Loading libraries
library(tidytuesdayR)
library(countrycode)
library(tidyverse)
library(ggthemes)

# Loading data
tt <- tt_load("2022-07-19")

# Printing a summary of tt$technology
tt$technology

# Printing the distinct "variable" and "label" pairs for the "Energy" category
## This will be used as a reference to create the "energy_type" column/variable
tt$technology %>% filter(category == "Energy") %>% select(variable, label) %>%
  distinct()

# Setting a seed to make results reproducible
set.seed("20220719")
# Using sample() to select six rows of tt$technology at random
sample_rows <- sample(x = rownames(tt$technology), size = 6)
# Creating a subset using the random rows
technology_sample <- tt$technology[sample_rows, ]
# Printing a summary of the randomly sampled subset
technology_sample

# Adding continent and country name columns/variables to the sample subset,
# using the countrycode::countrycode() function
technology_sample <- technology_sample %>%
  mutate(continent = countrycode(iso3c, origin = "iso3c",
    destination = "continent"),
    country = countrycode(iso3c, origin = "iso3c", destination = "country.name"))
# Selecting the country ISO code, continent and country name of the sample
# subset, to confirm that countrycode() worked as intended
technology_sample %>% select(iso3c, continent, country)

# Adding the corresponding continent for each country in tt$technology;
# filtering to select for the "Energy" category; adding a more succinct
# "energy_type" variable; and dropping rows with missing values
energy_tbl <- tt$technology %>%
  mutate(continent = countrycode(iso3c, origin = "iso3c",
    destination = "continent")) %>%
  filter(category == "Energy") %>%
  mutate(energy_type = fct_recode(variable,
    "Consumption" = "elec_cons", "Coal" = "elec_coal", "Gas" = "elec_gas",
    "Hydro" = "elec_hydro", "Nuclear" = "elec_nuc", "Oil" = "elec_oil",
    "Other renewables" = "elec_renew_other", "Solar" = "elec_solar",
    "Wind" = "elec_wind", "Output" = "elecprod",
    "Capacity" = "electric_gen_capacity")) %>%
  drop_na()

# Printing a summary of energy_tbl
energy_tbl

# Filtering energy_table for fossil fuel rows
fossil_fuel_tbl <- energy_tbl %>%
  filter(energy_type != "Consumption" & energy_type != "Output" 
    & energy_type != "Capacity") %>% 
  filter(energy_type == "Coal" | energy_type == "Gas" | energy_type == "Oil")

# Printing a summary of the tibble
fossil_fuel_tbl

# Filtering energy_table for low-carbon energy source rows
low_carbon_tbl <- energy_tbl %>%
  filter(energy_type != "Consumption" & energy_type != "Output" 
    & energy_type != "Capacity") %>% 
  filter(energy_type != "Coal" & energy_type != "Gas" & energy_type != "Oil")

# Printing a summary of the tibble
low_carbon_tbl

# Plotting distributions of electricity produced from fossil fuels
fossil_fuel_tbl %>%
  ggplot(aes(x = fct_reorder(energy_type, value), y = value, fill = energy_type)) +
  geom_boxplot() +
  theme_solarized() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none") +
  scale_colour_discrete() +
  scale_y_log10() +
  facet_wrap(~continent, scales = "free") +
  labs(
    title = "Electricity generated from fossil fuels by continent",
    y = "Output in log terawatt-hours: log10(TWh)",
    x = "Source")

# Plotting distributions of electricity produced from low-carbon sources
low_carbon_tbl %>%
  ggplot(aes(x = fct_reorder(energy_type, value), y = value, fill = energy_type)) +
  geom_boxplot() +
  theme_solarized() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none") +
  scale_colour_discrete() +
  scale_y_log10() +
  facet_wrap(~continent, scales = "free") +
  labs(
    title = "Electricity generated from low-carbon sources by continent",
    y = "Output in log terawatt-hours: log10(TWh)",
    x = "Source")
