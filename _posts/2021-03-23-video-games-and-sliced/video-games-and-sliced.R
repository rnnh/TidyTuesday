## Setup

# Loading libraries
library(tidyverse)

# Reading in the raw data from GitHub (I would use "tt_load", but I hit an API
# rate limt)
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

## Plotting Peak vs. Average number of players using 200 observations

games %>% 
  select(gamename, avg, peak) %>%
  # Filtering out Cyberpunk 2077 as it has only a single observation
  filter(gamename != "Cyberpunk 2077") %>%
  slice_max(order_by = avg, n = 200) %>%
  ggplot(aes(x = avg, y = peak, colour = gamename)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Peak vs. Average number of players online simultaneously",
       subtitle = "Top 200 observations for Average used",
       y = "Highest number of players at the same time",
       x = "Average number of players at the same time",
       colour = "Game")

## Combining "year" and "month" into a new variable

# Creating a "year_month" variable with the year and month of each observation
# using the lubridate "as_date()" function, by pasting together...
games$year_month <- lubridate::as_date(paste(
  # ...the "year" variable...
  games$year,
  # ...the number of each month, obtained by matching the month names to the
  # "month.name" built-in constant...
  match(games$month, month.name),
  # ...the number "1", as a dummy "day" value...
  1,
  # ...separated by a "-".
  sep = "-"))

# Printing the start of the "games" object with the new variable...
games

## Plotting the monthly player gains and losses for three of the most popular games

games %>% 
  # Selecting the variables
  select(gamename, year_month, gain) %>%
  # Filtering the data set for three of the most popular games
  filter(gamename == "Dota 2" |
           gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS" |
           gamename == "Counter-Strike: Global Offensive") %>%
  ggplot(aes(year_month, gain, fill = gamename)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  # Adding dashed lines to put facets into perspective
  geom_hline(yintercept = 100000, linetype = "dashed") +
  geom_hline(yintercept = -100000, linetype = "dashed") +
  # Faceting the plot for each game
  facet_wrap(~gamename, scales = "free") +
  labs(
    title = "Gains/Losses in average number of players online for three games",
    subtitle = "Dashed lines added at +/-100,000 players for each game",
    x = "Time",
    y = "Gains/Losses in average number of players"
  )
