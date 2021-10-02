## Setup

# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(sf)

# Loading data set
tt <- tt_load("2021-04-13")

# Loading USA shapefile
usa_shapefile <- read_sf("~/TidyTuesday/data/States_shapefile-shp/")

# Creating a filtered, tidy tibble with one row per post office with coordinates
post_office_est <- tt$post_offices %>%
  filter(coordinates == TRUE) %>%
  filter(!is.na(established)) %>%
  filter(established >= 1772 & established <= 2021) %>%
  filter(longitude <= 100) %>%
  filter(!is.na(stamp_index) & stamp_index <= 9) %>%
  select(established, latitude, longitude, stamp_index)
  
post_office_est$established <- post_office_est$established %>% as.integer()

## Plotting US post offices from 1772 to 2000

# Creating an animation with one point per post office from 1772 to 2000
p <- ggplot(usa_shapefile) +
  geom_sf() +
  geom_point(aes(longitude, latitude, colour = stamp_index),
             data = post_office_est) +
  scale_fill_continuous(type = "gradient") +
  transition_time(established, range = c(1772L, 2000L)) +
  ease_aes("linear") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Post offices established each year in the US",
       subtitle = "Lighter points indicate rarer postmarks. Year: {frame_time}")

# Rendering the animation as a .gif
animate(p, nframes = 400, fps = 5, renderer = magick_renderer())

# All the post offices on the USA shapefile in a static plot
ggplot(usa_shapefile) +
  geom_sf() +
  geom_point(aes(longitude, latitude, colour = stamp_index),
             data = post_office_est) +
  scale_fill_continuous(type = "gradient") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Post offices in the US",
       subtitle = "Lighter points indicate rarer postmarks")
