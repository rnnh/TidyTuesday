## Setup and data preparation

# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(viridis)
library(tidytext)
library(forcats)
library(ggridges)

# Loading data set
tt <- tt_load("2021-03-30")

# Selecting the 14 brands with the most foundations in the data set as
# "top_brands"
top_brands <- tt$allShades %>%
  select(brand) %>%
  count(brand) %>%
  slice_max(order_by = n, n = 14)

# Selecting foundation names broken into individual words and lightness values 
# rounded to the nearest significant digit as "simplified_names"
simplified_names <- tt$allShades %>%
  mutate(rounded = signif(lightness, digits = 1)) %>%
  filter(!is.na(name)) %>%
  filter(rounded %in% c(0.2, 0.4, 0.6, 0.8, 1.0)) %>%
  select(name, rounded) %>%
  unnest_tokens(word, name) %>%
  count(rounded, word, sort = T)

# Counting the total number of words per rounded lightness value
total_words <- simplified_names %>%
  group_by(rounded) %>%
  summarise(total = sum(n))

# Added word count totals and tf-idf values to "simplified_names", and changing
# "rounded" to a factor variable with informative levels
simplified_names <- left_join(simplified_names, total_words, by = "rounded")
simplified_names <- simplified_names %>%
  bind_tf_idf(word, rounded, n)
simplified_names$rounded <- as.factor(simplified_names$rounded)
table(simplified_names$rounded)
levels(simplified_names$rounded) <- c("Lightness: 0.2, n = 28",
                                      "Lightness: 0.4, n = 148",
                                      "Lightness: 0.6, n = 221",
                                      "Lightness: 0.8, n = 217",
                                      "Lightness: 1.0, n = 50")
simplified_names

## Plotting foundations according to lightness

# Plotting all the foundations from "top_brands" according to lightness
tt$allShades %>%
  filter(brand %in% top_brands$brand) %>%
  ggplot(aes(lightness, brand, colour = hex)) +
  geom_jitter() +
  scale_colour_identity() +
  xlim(0, 1) +
  theme_classic() +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.50, linetype = "dashed") +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  labs(y = "", x = "Lightness",
       title = "Foundations from different brands plotted according to lightness",
       subtitle = "Each point represents the dominant colour of each foundation")

## Plotting distributions of foundation lightness

# Plotting the distribution of foundations from "top_brands" according to
# lightness
tt$allShades %>%
  filter(brand %in% top_brands$brand) %>%
  ggplot(aes(lightness, brand, fill = brand, group = brand)) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(discrete = TRUE) +
  xlim(0, 1) +
  theme_ridges() +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.50, linetype = "dashed") +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  theme(legend.position = "none") +
  labs(y = "Brands", x = "Lightness",
       title = "Foundation shade distributions",
       subtitle = "Distribution of foundations from different brands according to lightness")

## Plotting keywords associated with foundations of different shades

simplified_names %>%
  group_by(rounded) %>%
  slice_max(n = 5, order_by = tf_idf) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = rounded)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~rounded, ncol = 2, scales = "free") +
  theme_classic() +
  labs(x = "Term frequency-inverse document freqeuncy (tf-idf)", y = "Keywords",
       title = "Keywords associated with foundations of different lightnesses")
