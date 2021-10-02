## Setup

# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(exploreR)

# Loading data
tt <- tt_load("2021-08-10")

## Plotting distribution of inflation-adjusted infrastructure investments

# Creating tbl_df with gross_inv_chain values
untransformed_tbl_df <- tibble(
  gross_inv_chain = tt$chain_investment$gross_inv_chain,
  transformation = "Untransformed"
  )

# Creating tbl_df with log10(gross_inv_chain) values
log10_tbl_df <- tibble(
  gross_inv_chain = log10(tt$chain_investment$gross_inv_chain),
  transformation = "Log10"
)

# Combining the above tibbles into one tbl_df
gross_inv_chain_tbl_df <- rbind(untransformed_tbl_df, log10_tbl_df)

# Plotting distribution of inflation-adjusted infrastructure investments
gross_inv_chain_tbl_df %>%
  ggplot(aes(x = gross_inv_chain, fill = transformation)) +
  geom_histogram(show.legend = FALSE, position = "identity",
                 bins = 12, colour = "black") +
  facet_wrap(~transformation, scales = "free") +
  labs(fill.position = "none", y = NULL,
       x = "Gross infrastructure investments adjusted for inflation (millions USD)",
       title = "Distributions of untransformed and log transformed infrastructure investments",
       subtitle = "Log transformed investments are more normally distributed") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()

## Exploring a data set using mass linear regression

# Creating a copy of the chain_investment data set with log10 transformed
# gross investment values
chain_investment_df <- tt$chain_investment %>%
  # Creating a log10 transformed copy of gross_inv_chain
  mutate(gross_inv_transformed = log10(gross_inv_chain)) %>%
  # Removing -Inf values
  filter(gross_inv_transformed != -Inf) %>%
  # Selecting variables to include in the data frame
  select(category, meta_cat, group_num, year, gross_inv_transformed)

# Applying mass linear regression
transformed_investment_masslm <- masslm(chain_investment_df,
                                        dv.var = "gross_inv_transformed")

# Printing the masslm results in order of R squared values (decreasing)
transformed_investment_masslm %>%
  arrange(-R.squared)

# Printing the masslm results in order of p-values
transformed_investment_masslm %>%
  arrange(P.value)

