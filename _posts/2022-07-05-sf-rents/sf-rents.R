# Loading libraries
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(ggthemes)

# Loading data
tt <- tt_load("2022-07-05")

# Printing a summary of the San Francisco (SF) permits data frame
tt$sf_permits
# Printing a summary of the shape of the data frame
paste("tt$sf_permits has", nrow(tt$sf_permits), "rows and", ncol(tt$sf_permits),
  "columns.")

# Creating a tall/narrow data set of permits per street
permits_per_street <- tt$sf_permits %>%
  # Selecting variables/columns to keep
  select(permit_type_definition, street_name, permit_number) %>%
  # Grouping the permit numbers by type and street name for counting
  group_by(permit_type_definition, street_name) %>%
  # Counting/tallying the number of permits by type per street
  tally()

# Printing a summary of the permits per street data frame
permits_per_street
# Printing a summary of the shape of the data frame
paste("permits_per_street has", nrow(permits_per_street), "rows and",
  ncol(permits_per_street), "columns.")

# Creating a wider copy of the permits per street data frame
permits_per_street_wider <- permits_per_street %>%
  # Pivoting the street names wider (creating a column for each street) and
  # selecting the "n" variable for the values in this data frame
  pivot_wider(names_from = street_name, values_from = n)

# Printing the wider permits per street data frame
permits_per_street_wider

# Printing a summary of the shape of the data frame
paste("permits_per_street_wider has", nrow(permits_per_street_wider), "rows and",
  ncol(permits_per_street_wider), "columns.")

# Printing a summary of the new construction data frame
tt$new_construction
# Printing a summary of the shape of the data frame
paste("tt$new_construction has", nrow(tt$new_construction), "rows and",
  ncol(tt$new_construction), "columns.")

# Creating a taller/more narrow subset of production type per county
production_per_county <- tt$new_construction %>%
  # Selecting variables/columns from tt$new_construction
  select(county, year, totalproduction, sfproduction, mfproduction,mhproduction) %>%
  # "Lengthening" the data frame by selecting columns to be pivoted to a longer format
  pivot_longer(cols = c(totalproduction, sfproduction, mfproduction, mhproduction)) %>%
  # Creating a copy of the "name" column to the more descriptive "production_type", as the
  # pivoted columns all describe types of production, and removing the original "name"
  # column
  mutate(production_type = name, name = NULL) %>%
  # Changing "production_type" from a character to a factor variable, with more
  # descriptive factor levels
  mutate(production_type = fct_recode(production_type,
    "Total" = "totalproduction", "Single family" = "sfproduction",
    "Multi family" = "mfproduction", "Mobile home" = "mhproduction"))

# Printing a summary of the production per county data frame
production_per_county
# Printing a summary of the shape of the data frame
paste("production_per_county has", nrow(production_per_county), "rows and",
  ncol(production_per_county), "columns.")

# Plotting the top 20 streets with the total number of each permit category
permits_per_street %>%
  slice_max(order_by = n, n = 20) %>%
  mutate(street_name = reorder_within(street_name, n, permit_type_definition)) %>%
  ggplot(aes(x = n, y = street_name, fill = permit_type_definition)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme_solarized_2() +
  facet_wrap(~permit_type_definition, ncol = 2, scales = "free") +
  labs(title = "Count of construction permits by type per street",
    x = "Tally", y = "Street name")

# Plotting the annual construction by type per San Francisco county
production_per_county %>%
  ggplot(aes(x = year, y = value,
    colour = fct_reorder2(production_type, year, value))) +
  geom_line() +
  theme_clean() +
  facet_wrap(~county, scales = "free") +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(breaks =
      seq(min(production_per_county$year), max(production_per_county$year), 8)) +
  geom_vline(xintercept = 2008, linetype = 2, colour = "red", size = 0.4) +
  labs(colour = "Production type", x = "Year", y = "Units",
    title = "Annual construction by type per San Francisco county",
    subtitle = "Red vertical line marks 2008")

