library(tidymodels)
library(tidyverse)
library(ranger)
library(palmerpenguins)
library(peopleanalyticsdata)


options(scipen = 0)
set.seed(123)
theme_set(theme_minimal())


# We want to build a model for the sex of the three species. We can build a classification
# model to distinguish between male and female penguins.

## 1. Explore data set:
penguins %>% view()

penguins %>% count(island, sort = TRUE)

penguins %>% count(island, species, sort = TRUE)

penguins %>% count(year) %>% arrange(desc(year))

# Exploratory Data Analysis:
penguins %>% 
  filter(!is.na(sex)) %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = sex,
             size = body_mass_g)) +
  geom_point(alpha = 0.7) + 
  facet_wrap(species~island) +
  geom_smooth(method = "lm")

## 2. Build a model by controlling for the difference in species, to predict the sex
penguins_df <- penguins %>% 
  filter(!is.na(sex)) %>%
  select(-year, -island)

# Split the data into test/train by sex:
penguins_split <- initial_split(penguins_df, strata = sex)

# Extract the training data
penguin_train <- training(penguins_split)

# Extract the testing data
penguin_test <- testing(penguins_split)


# Recognize that there's not enough data to train. Therefore use a resampling method
# on the Training set to artificially increase the sample size. Use Bootstrap resamples
penguin_boot <- bootstraps(penguin_train, 
                           times = 25, 
                           apparent = TRUE)

## 3. Set up the model specifications: Logistic and Random Forest Classification Models
# Set up a Logistic Regression with GLM Engine:
glm_spec <-logistic_reg() %>% 
  set_engine("glm")

    # "stan" - bayesian 
    # "glm" - usual logistic
    # "glmnet" - regularized

# Set up a Random Forest Regression:
rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")

## 4. Set up a workflow (needs model specification(s), and a pre-processor)
# Add a pre-processor
penguin_wf <- workflow() %>%
  add_formula(sex ~ .)

## 5. Train the models on penguin_boot:
# 1. GLM:
glm_rs <- penguin_wf %>% add_model(glm_spec) %>% 
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

glm_rs

# 2. Random Forest:
rf_rs <- penguin_wf %>% 
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs

## 6. Evaluate the models:
glm_rs %>% conf_mat_resampled
collect_metrics(glm_rs) # glm

rf_rs %>% conf_mat_resampled
collect_metrics(rf_rs) # random forest

# glm did better by measuring AUC

# Draw AUC 
glm_rs %>% 
  collect_predictions() %>%
  group_by(id) %>% 
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

rf_rs %>% 
  collect_predictions() %>%
  group_by(id) %>% 
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

## 7. Evaluate the model on the new data using the last_fit() function:
penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguins_split)


## 8. Evaluate 
collect_metrics(penguin_final)

collect_predictions(penguin_final) %>% conf_mat(sex, .pred_class)

## 9. Interpret Coefficients
penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE) %>% arrange(desc(estimate))

# Every 1 mm increase in bill_depth increase corresponds to an almost 6 times
# higher odds of being male.




