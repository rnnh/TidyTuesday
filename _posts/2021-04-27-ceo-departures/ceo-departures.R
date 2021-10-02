## Setup

# Loading libraries
library(tidyverse)
library(tidytuesdayR)
library(tidymodels)
library(textrecipes)
library(themis)
library(parsnip)
library(doParallel)

# Installing the GitHub version of {parsnip} to use svm_linear()
#devtools::install_github("tidymodels/parsnip")

# Loading data set
tt <- tt_load("2021-04-27")

### Data wrangling

# Creating a new variable "depart_vol" to record CEO departures as
## Voluntary or Involuntary
departures <- tt$departures %>% mutate(depart_vol = factor(departure_code))
# Setting descriptive factor levels for "depart_vol"
levels(departures$depart_vol) <- list(
  "Involuntary" = levels(departures$depart_vol)[1:4],
  "Voluntary" = levels(departures$depart_vol)[5:6],
  "Other" = levels(departures$depart_vol)[7:9]
  )

# Filtering "depart_vol" and "notes" to a new object: "depart_notes"
depart_notes <- departures %>%
  filter(!is.na(notes) & !is.na(depart_vol)) %>%
  filter(depart_vol != "Other") %>%
  select(depart_vol, notes)

# Dropping empty factor levels. In this case, the level "Other" is empty
depart_notes$depart_vol <- droplevels(depart_notes$depart_vol)
# Printing the number of Involuntary and Voluntary CEO departures with descriptions
table(depart_notes$depart_vol)

## Setting parameters for machine learning model

# Setting seed for reproducibility
set.seed(100)
# Splitting depart_notes into training and test data
departures_split <- depart_notes %>%
  initial_split(strata = depart_vol)

# Creating training and test sets using departures_split
departures_train <- training(departures_split)
departures_test <- testing(departures_split)

# Setting seed for reproducibility
set.seed(100)
# Setting 10-fold cross-validation (CV) using training data
departures_folds <- vfold_cv(departures_train, strata = depart_vol)
# Printing 10-fold CV splits
departures_folds

# Feature engineering
## Creating preprocessing recipe for predicting voluntary/involuntary departures
departures_rec <-  recipe(depart_vol ~ notes, data = departures_train) %>%
  # Splitting the "notes" variable into individual tokens (words)
  step_tokenize(notes) %>%
  # Limiting number of tokens used in model
  step_tokenfilter(notes, max_tokens = 1000) %>%
  # Weighing tokens by tf-idf
  step_tfidf(notes) %>%
  # Normalising numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Upsampling to address class imbalance
  step_smote(depart_vol)

# Specifying the machine learning model
## This model will use a linear Support Vector Machine (SVM)
svm_spec <- svm_linear() %>%
  set_mode("classification") %>%
  set_engine("LiblineaR")

# Combining the feature engineering recipe and model spec to create a workflow
departures_wf <- workflow() %>%
  add_recipe(departures_rec) %>%
  add_model(svm_spec)
# Printing the workflow
departures_wf

# Selecting the metrics used for modelling
departures_metrics <- metric_set(accuracy, recall, precision)

## Creating a model using the training subset

# Setting up parallel processing
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Setting seed for reproducibility
set.seed(100)
# Fitting models using resampling
svm_results <- fit_resamples(
  departures_wf, # Specifying workflow
  departures_folds, # Specifying cross validation folds
  metrics = departures_metrics, # Metrics used for modelling
  control = control_resamples(save_pred = TRUE) # Saving predictions for confusion matrix
)

# Printing performance metrics of SVM model
collect_metrics(svm_results)

## Creating a model using the full data set

# Setting seed for reproducibility
set.seed(100)

# Fitting the final model
final_fitted <- last_fit(departures_wf, departures_split,
                      metrics = departures_metrics)

# Printing performance metrics of final fitted model
collect_metrics(final_fitted)

## Confusion matrices for training and full models

# Printing the confusion matrix of the model trained using the 10-fold cv
## training data
svm_results %>%
  conf_mat_resampled(tidy = FALSE)

# Printing the confusion matrix of the final fitted model
collect_predictions(final_fitted) %>%
  conf_mat(depart_vol, .pred_class)

# Visualising confusion matrix of the final fitted model
collect_predictions(final_fitted) %>%
  conf_mat(depart_vol, .pred_class) %>%
  autoplot() +
  labs(title = "Prediction vs Truth for final linear SVM model",
       subtitle = "Class imbalance is an issue; most of the observations were of Voluntary CEO departures")

## Significant words according to the final model

# Extracting model fit
departures_fit <- pull_workflow_fit(final_fitted$.workflow[[1]])

# Visualising the most important words for predicting whether a CEO departure
## was voluntary
tidy(departures_fit) %>%
  filter(term != "Bias") %>%
  group_by(sign = estimate > 0) %>%
  slice_max(abs(estimate), n = 15) %>% 
  ungroup() %>%
  mutate(term = str_remove(term, "tfidf_notes_"), # Tidying terms
         sign = ifelse(sign, "More likely from Voluntary CEO departures",
                       "More likely from Involuntary CEO departures")) %>%
  ggplot(aes(abs(estimate), fct_reorder(term, abs(estimate)), fill = sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sign, scales = "free") +
  labs(x = "Coefficient from linear SVM", y = NULL) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  labs(titles = "Which words are more likely to be used when a CEO leaves Involuntarily vs. Voluntarily?",
       subtitle = "Importance assigned by linear SVM model, based on descriptions of CEO departures")
