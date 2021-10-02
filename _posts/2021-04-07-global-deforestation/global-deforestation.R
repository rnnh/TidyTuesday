## Setup

# Loading libraries
library(tidyverse)
library(tidytuesdayR)

# Loading data
tt <- tt_load("2021-04-06")

# Creating a function to tidy the tt$brazil_loss data set
## The function "tidy_brazil_loss" takes a variable name as an argument.
## It returns that variable in a tidy format, with one row per observation and
## one column per variable
tidy_brazil_loss <- function(variable){
  tt$brazil_loss %>% 
    select(year, variable) %>% 
    mutate(cause = as.character(variable)) %>% 
    select(year, cause, variable) %>%
    rename("loss" = variable)
}

# Creating a list of the variables which will be run through the
# tidy_brazil_loss function
## The first three variables (entity, code and year) are skipped: entity and
## code are irrelevant as all the observations are from Brazil (i.e. one
## country) and the value for "year" will be included in each observation in the
## tidy data set
variable_names <- colnames(tt$brazil_loss)[4:14]
# Applying the tidy_loss_function to all the variable names using purr::map
tidy_brazil_list <- map(variable_names, tidy_brazil_loss)
# Binding the tidy version of each variable by row using purr::map_df
tidy_brazil <- map_df(tidy_brazil_list, rbind)

# Changing the "cause" variable in tidy_brazil to a factor variable
tidy_brazil$cause <- as.factor(tidy_brazil$cause)
# Printing the levels of tidy_brazil$cause
levels(tidy_brazil$cause)
# Setting more descriptive factor levels for tidy_brazil$cause
levels(tidy_brazil$cause) <- c("Commercial crops",
                               "Fire loss",
                               "Flooding due to dams",
                               "Mining",
                               "Natural disturbances",
                               "Infrastructure (not roads)",
                               "Pasture for livestock",
                               "Roads",
                               "Logging for lumber",
                               "Small scale clearing",
                               "Tree plantations")

# Printing the start of the tidied version of tt$brazil_loss
tidy_brazil

# Getting the six countries with the highest net forest conversion
net_forest_max <- tt$forest %>% 
  filter(entity != "World") %>%
  group_by(entity) %>% 
  summarise(total = sum(net_forest_conversion)) %>%
  slice_max(total, n = 6)

# Getting the six countries with the lowest net forest conversion
net_forest_min <- tt$forest %>% 
  filter(entity != "World") %>%
  group_by(entity) %>% 
  summarise(total = sum(net_forest_conversion)) %>%
  slice_min(total, n = 6)

# Binding the objects with the six highest and six lowest net forest conversions
net_forest_countries <- rbind(net_forest_max, net_forest_min)

## Plotting the causes of deforestation in Brazil

# Plotting the causes of deforestation in Brazil
tidy_brazil %>%
  ggplot(aes(year, loss, colour = cause)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 10000, linetype = 2, size = 0.5) +
  facet_wrap(~cause, scales = "free") +
  theme_classic() +
  guides(colour = FALSE) +
  labs(y = "Forest loss (hectares)", x = "Time",
       title = "Forest loss due to different causes in Brazil",
       subtitle = "Dashed line added at 10,000 hectares for perspective")

## Plotting net change in forest area across different countries

tt$forest %>%
  filter(entity %in% net_forest_countries$entity) %>%
  select(entity, year, net_forest_conversion) %>%
  ggplot(aes(year, net_forest_conversion, colour = entity)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 250000, linetype = 2, colour = "green", size = 0.4) +
  geom_hline(yintercept = -250000, linetype = 2, colour = "red", size = 0.4) +
  facet_wrap(~entity, scales = "free") +
  theme_classic() +
  guides(colour = FALSE) +
  labs(y = "Net forest conversion (hectares)", x = "Time",
       title = "Countries with highest and lowest net changes in forest area",
       subtitle = "Dashed lines at +250,000 hectares (green) and -250,000 hectares (red)")
