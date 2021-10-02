## Setup

# Loading libraries
library(tidytuesdayR) # For loading data set
library(textrecipes) # For adding tf-idf to recipies (step_tfidf)
library(tidyverse) # For the ggplot2, dplyr, forcats libraries
library(tidytext) # For text mining
library(glmnet) # For lasso model
library(tidymodels) # For various modelling libraries

# Loading data set
tt <- tt_load("2021-08-17")

## Most spoken words by prominent characters

# Creating a vector of characters with most interactions with computers
chars_with_most_interactions <- tt$computer %>%
  filter(char_type != "Computer") %>% # Filtering out rows for computer lines
  count(char, sort = TRUE) %>% # Counting the number of rows for each character
  filter(n >= 150) %>% # Filtering out characters with under 150 lines
  pull(char) # Pulling the names of the characters as a vector

# Counting words spoken per character
character_words <- tt$computer %>%
  filter(char %in% chars_with_most_interactions) %>% # Selecting rows
  unnest_tokens(word, interaction) %>% # Creating a row per word in interaction
  anti_join(get_stopwords(), by = "word") %>% # Removing stop words
  count(char, word, sort = TRUE) # Counting words per character

# Plotting most spoken words per character
character_words %>%
  group_by(char) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(n, fct_reorder(word, n), fill = char)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~char, ncol = 2, scales = "free") +
  scale_fill_viridis_d() +
  theme_classic() +
  labs(title = "Words most frequently spoken by prominent characters",
       x = "Utterances", y = "Words")

## Words specific to prominent characters

# Counting total number of words spoken per character
total_words <- character_words %>% 
  group_by(char) %>%  # Grouping words by character
  summarize(total = sum(n)) # Counting words spoken per character ("total")

# Joining tbl_dfs: adding "total" variable to character_words
character_words <- left_join(character_words, total_words)

# Adding tf_idf to character_words
character_words <- character_words %>%
  bind_tf_idf(term = word, document = char, n = n) %>%
  arrange(desc(tf_idf)) # Arranging rows by descending tf_idf values

# Printing a summary of the character_words object
glimpse(character_words)

# Plotting words with the top tf-idf values per character
character_words %>%
  group_by(char) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = char)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~char, ncol = 2, scales = "free") +
  scale_fill_viridis_d() +
  theme_classic() +
  labs(title = "Spoken words specific to each character",
       subtitle = "Words listed are used often by each character relative to other characters",
       y = "Words",
       x = "term frequency-inverse document frequency (tf-idf)")

## Specifying a model to classify People and Computers in the data set

# Summarising the values in the char_type variable
tt$computer %>% select(char_type) %>% table()

# Setting seed for reproducibility
set.seed(20210818)
# Splitting data into training and test subsets
char_type_split <- tt$computer %>%
  initial_split(strata = char_type) # Ensures equal proportion of char_type

# Creating training and test sets using departures_split
char_type_train <- training(char_type_split)
char_type_test <- testing(char_type_split)

# Setting seed for reproducibility
set.seed(20210818)
# Setting up 10-fold cross-validation (CV) using training data
char_type_folds <- vfold_cv(char_type_train,
                            strata = char_type) # Ensures equal proportion of char_type

# Creating preprocessing recipe for predicting if lines are spoken by a computer
char_type_rec <-  recipe(char_type ~ interaction, data = char_type_train) %>%
  step_tokenize(interaction) %>% # Splitting interaction variable into words
  step_tokenfilter(interaction, max_tokens = 1500) %>% # Limiting tokens used
  step_tfidf(interaction) %>% # Weighing tokens by tf-idf
  themis::step_downsample(char_type) # Downsampling to address class imbalance

# Specifying a lasso regularised model
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# Creating a supervised machine learning workflow
char_type_wf <- workflow() %>%
  add_recipe(char_type_rec) %>%
  add_model(lasso_spec)

# Printing workflow
char_type_wf

## Fitting the model to resampled training data

# Fitting model to resampled folds to estimate performance
char_type_rs <- fit_resamples(
  object = char_type_wf, # Machine learning workflow
  resamples = char_type_folds, # 10-fold cross-validation
  control = control_resamples(save_pred = TRUE)
)

# Collecting predictions from resampled folds
char_type_rs_predictions <- collect_predictions(char_type_rs)

# Collecting model performance metrics metrics
char_type_rs_metrics <- collect_metrics(char_type_rs)
# Printing performance metrics
char_type_rs_metrics

# Plotting ROC curves for resampled folds
char_type_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = char_type, .pred_Computer) %>%
  autoplot() + 
  scale_color_viridis_d() +
  labs(color = "Resample (fold)",
       title = "ROC curve for character type based on interaction",
       subtitle = "True Positive Rate (TPR) v.s. False Positive Rate (FPR)",
       x = "FPR (1 - Specificity)", y = "TPR (Sensitivity)") +
  theme_bw()

## Evaluating the model using the test data

# Fitting model to training data, evaluating performance on test data
char_type_last_fit <- last_fit(char_type_wf, char_type_split)
# Printing metrics for final fit
collect_metrics(char_type_last_fit)

# Plotting confusion matrix of testing data
collect_predictions(char_type_last_fit) %>%
  conf_mat(truth = char_type, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_fill_continuous() +
  labs(title = "Confusion matrix of lasso classification model performance on test subset",
       subtitle = "Sum of True Positives and False Positives for both classes: Computer and Person")

## Important words in the final model

# Extracting model fit
char_type_workflow_fit <- pull_workflow_fit(char_type_last_fit$.workflow[[1]])

# Visualising the most important words for predicting whether a line was spoken
# by a Computer or Person character
tidy(char_type_workflow_fit) %>%
  filter(term != "Bias", term != "(Intercept)") %>%
  group_by(sign = estimate > 0) %>%
  slice_max(abs(estimate), n = 15) %>% 
  ungroup() %>%
  mutate(term = str_remove(term, "tfidf_interaction_"), # Tidying terms
         sign = ifelse(sign, "More likely from a Person",
                       "More likely a Computer")) %>%
  ggplot(aes(abs(estimate), fct_reorder(term, abs(estimate)), fill = sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sign, scales = "free") +
  theme_classic() +
  scale_fill_viridis_d() +
  labs(titles = "Which words are more likely to be used by a Computer vs. Person character?",
       subtitle = "Importance assigned by lasso classification model, based lines spoken",
       x = "Coefficient from lasso regularised model", y = "Words")
