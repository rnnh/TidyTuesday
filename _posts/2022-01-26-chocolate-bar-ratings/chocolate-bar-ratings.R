# Loading libraries
library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(tidytext)

# Loading data
tt <- tt_load("2022-01-18")

# Counting how many times each characteristic is used
country_characteristics <- tt$chocolate %>%
  unnest_tokens(memorable_characteristic,
                most_memorable_characteristics, token = "regex",
                pattern = ",", to_lower = TRUE) %>%
  mutate(memorable_characteristic = str_squish(memorable_characteristic)) %>%
  count(country_of_bean_origin, memorable_characteristic, sort = TRUE)

# Counting the total number of characteristics used for each country of origin
total_country_characteristics <- country_characteristics %>%
  group_by(country_of_bean_origin) %>%
  summarise(total = sum(n))

# Joining these data frames
country_characteristics <- left_join(country_characteristics,
                                     total_country_characteristics,
                                     by = "country_of_bean_origin")

# Finding the six countries of origin with the most characteristics
top_countries <- total_country_characteristics %>%
  slice_max(n = 6, order_by = total) %>%
  select(country_of_bean_origin)

# Filtering the data
country_characteristics <- country_characteristics %>%
  filter(country_of_bean_origin %in% top_countries$country_of_bean_origin) %>%
  select(country_of_bean_origin, memorable_characteristic, n, total)

# Adding tf-idf
country_characteristics <- country_characteristics %>%
  bind_tf_idf(memorable_characteristic,
              country_of_bean_origin, n)

# Printing a summary of the data frame
country_characteristics

# Plotting the most used characteristics in the chocolate bar summaries
country_characteristics %>%
  group_by(country_of_bean_origin) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(country_of_bean_origin = as.factor(country_of_bean_origin),
         memorable_characteristic = reorder_within(memorable_characteristic,
                                                   n,
                                                   country_of_bean_origin)) %>%
  ggplot(aes(n, memorable_characteristic, fill = country_of_bean_origin)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme_solarized_2() +
  facet_wrap(~country_of_bean_origin, ncol = 2, scales = "free") +
  labs(title = "Most used Characteristics in Chocolate Bar Summaries",
       subtitle = "Summaries grouped by cocoa country of origin",
       x = "Characteristic count", y = NULL)

# Plotting the most important characteristics in the chocolate bar summaries
country_characteristics %>%
  group_by(country_of_bean_origin) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(country_of_bean_origin = as.factor(country_of_bean_origin),
         memorable_characteristic = reorder_within(memorable_characteristic,
                                                   tf_idf,
                                                   country_of_bean_origin)) %>%
  ggplot(aes(tf_idf, memorable_characteristic, fill = country_of_bean_origin)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  theme_solarized_2() +
  facet_wrap(~country_of_bean_origin, ncol = 2, scales = "free") +
  labs(title = "Important Characteristics in Chocolate Bar Summaries",
       subtitle = "Reviews grouped by cocoa country of origin",
       x = "Term frequency–inverse document frequency (tf-idf)", y = NULL)