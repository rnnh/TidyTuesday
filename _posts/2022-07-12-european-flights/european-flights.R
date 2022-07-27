# Loading libraries
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(ggthemes)

# Loading data
tt <- tt_load("2022-07-12")

# Printing a summary of the flights data frame
tt$flights
# Printing a summary of the shape of the data frame
paste("tt$flights has", nrow(tt$flights), "rows and", ncol(tt$flights),
  "columns.")

# Defining a function to tidy the flights data set
tidy_flights_per_airport <- function(input_flight_type){
  tt$flights %>% 
    # Selecting columns, including the column with the name "input_flight_type"
    ## "all_of()" is used for error handling: if a column with the name matching
    ## "input_flight_type" is not available in tt$flights, the function will return an error
    select(FLT_DATE, APT_NAME, all_of(input_flight_type)) %>% 
    # Adding a "flight_type" column, with "input_flight_type" as a string for each row
    mutate(flight_type = as.character(input_flight_type)) %>% 
    # Renaming the input "input_flight_type" column to "number_of_flights"
    rename("number_of_flights" = input_flight_type)
}

# Selecting column names with flight types (arrivals, departures, total flights)
flight_types <- colnames(tt$flights)[8:13]
# Printing the flight types
flight_types

# Applying the tidying function to the flight types vector using purrr::map()
tidy_flights_list <- map(flight_types, tidy_flights_per_airport)

# Binding the tidy version of each flight type by row using purrr::map_df
tidy_flights <- map_df(tidy_flights_list, rbind)

# Printing a summary of the tidy flights data frame
tidy_flights
# Printing a summary of the shape of the data frame
paste("tidy_flights has", nrow(tidy_flights), "rows and", ncol(tidy_flights),
  "columns.")

## Selecting the top 6 airports by total number of flights on the latest flight
## date
top_airports <- tidy_flights %>%
  filter(flight_type == "FLT_TOT_1") %>%
  filter(FLT_DATE == max(FLT_DATE)) %>%
  slice_max(order_by = number_of_flights, n = 6)

# Changing "flight_type" to a factor with descriptive levels
tidy_flights$flight_type <- as.factor(tidy_flights$flight_type)
levels(tidy_flights$flight_type) <- c("Arrivals", "Arrivals (Airport Operator)",
  "Departures", "Departures (Airport Operator)", "Total", "Total (Airport Operator")

# Plotting the distribution of arrivals and departures for the top airports
tidy_flights %>%
  filter(APT_NAME %in% top_airports$APT_NAME) %>%
  filter(flight_type %in% c("Arrivals", "Departures")) %>%
  ggplot(aes(x = APT_NAME, y = number_of_flights, colour = flight_type)) +
  geom_boxplot() +
  theme_solarized() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_discrete() +
  labs(title = "Distribution of daily arrivals and depatures across six airports",
    x = "Airport", y = "Flights", colour = "Flight type")

