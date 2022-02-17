# Loading libraries
library(geomtextpath) # For adding text to ggplot2 curves
library(tidytuesdayR) # For loading data set
library(ggbeeswarm) # For creating a beeswarm plot
library(tidyverse) # For the ggplot2, dplyr libraries
library(gganimate) # For plot animation
library(ggthemes) # For more ggplot2 themes
library(viridis) # For plot themes

# Loading data set
tt <- tt_load("2022-01-11")

# Creating subsets of the original bee colony data
colony_counts_overall <- tt$colony %>%
  filter(state == "United States")

colony_counts_per_state <- tt$colony %>%
  filter(state != "United States" & state != "Other states")

# Defining a function to tidy bee colony count data, which takes
# "messy_colony_data" as an argument
tidy_colony_data <- function(messy_colony_data){
  # Writing the result of the following piped steps to "tidied_colony_data"
  tidied_colony_data <- messy_colony_data %>%
    # Selecting variables
    select(year, colony_n, colony_lost, colony_added, colony_reno) %>%
    # Dropping rows with missing values
    drop_na() %>%
    # Changing columns to rows
    pivot_longer(!year, names_to = "type", values_to = "count") %>%
    # Setting "type" as a factor variable
    mutate(type = factor(type)) %>%
    # Recoding the levels of the "type" factor
    mutate(type = fct_recode(type,
                             "Total colonies" = "colony_n",
                             "Lost" = "colony_lost",
                             "Added" = "colony_added",
                             "Renovated" = "colony_reno")) %>%
    # Reordering "type" factor levels
    mutate(type = fct_relevel(type,
                              "Total colonies", "Lost", "Added", "Renovated"))
  # Returning "tidied_colony_data"
  return(tidied_colony_data)
}

# Using this function to tidy the subsets
tidied_colony_counts_overall <- tidy_colony_data(colony_counts_overall)

tidied_colony_counts_per_state <- tidy_colony_data(colony_counts_per_state)

# Printing a summary of the subsets before tidying...
colony_counts_overall
colony_counts_per_state

# ...and after tidying
tidied_colony_counts_overall
tidied_colony_counts_per_state

# Plotting Bee Colony observations using geom_beeswarm() from {ggbeeswarm}
tidied_colony_counts_per_state %>%
  ggplot(aes(x = type, y = count)) +
  geom_beeswarm(cex = 4, colour = "yellow") +
  scale_y_log10() +
  theme_solarized_2(light = FALSE) +
  facet_wrap(~type, scales = "free") +
  theme(legend.position="none", axis.text.x = element_blank()) +
  labs(title = "Bee Colonies Counted, Lost, Added, Renovated",
       subtitle = "Created using {ggbeeswarm}",
       x = NULL, y = "Number of bee colonies (log10)",
       fill = NULL)

# Defining an animation showing bee colony counts over time
p <- tidied_colony_counts_per_state %>%
  ggplot(aes(x = count, y = fct_reorder(type, count))) +
  geom_jitter(color = "yellow", alpha = 0.8) +
  geom_boxplot(width = 0.2, alpha = 0.8, notch = TRUE, colour = "cyan") +
  scale_x_log10() +
  theme_solarized_2(light = FALSE) +
  theme(legend.position="none", axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  transition_time(as.integer(year)) +
  labs(title = "Bee Colonies Counted, Lost, Added, Renovated, per year",
       subtitle = "Year: {frame_time}",
       x = "Number of bee colonies (log10)", y = NULL)

# Rendering the animation as a .gif
animate(p, nframes = 180, start_pause = 20,  end_pause = 20,
        renderer = magick_renderer())

# Creating a density plot for different observation types
tidied_colony_counts_overall %>%
  filter(type != "Total colonies") %>%
  ggplot(aes(x = count, colour = type, label = type)) +
  geom_textdensity(size = 7, fontface = 2, hjust = 0.89, vjust = 0.3,
                   linewidth = 1.2) +
  theme_solarized_2(light = FALSE) +
  theme(legend.position = "none") +
  labs(title = "Distribution of Bee Colony Counts",
       subtitle = "Distributions of Bee Colonies Addded, Renovated, Lost",
       x = "Number of bee colonies")