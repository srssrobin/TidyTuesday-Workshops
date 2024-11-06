library(tidymodels)
library(tidyverse)
library(skimr)
library(kableExtra)
library(themis)

members <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv")

# Check the dataset
members
skim(members)

# 1. Exploratory Data Analysis

## 1.1.1 EDA By Year 
members %>% group_by(year) %>%
  summarise(died = mean(died), 
            success = mean(success)) %>%
  arrange(desc(year))

## 1.1.2 EDA by Decade
df_1 <- members %>% group_by(year = 10*(year %/% 10)) %>%
  summarise(died = mean(died), success = mean(success)) %>%
  arrange(desc(year))

## 1.1.3 Pivot_longer (collapse died and success in the same column)
df_1 %>% pivot_longer(died:success, 
                      names_to = "outcome",
                      values_to = "percent") %>%
  ggplot(aes(x = year, y = percent, color = outcome)) +
  geom_line(alpha = 0.7, size = 1.5) +
  scale_y_continuous(labels = scales::percent_format())

## 1.2.1 EDA by Age instead of Year
df_2 <- members %>% group_by(age = 10*(age %/% 10)) %>%
  summarise(died = mean(died), success = mean(success)) %>%
  arrange(desc(age)) 

## 1.2.2 Visualize
df_2 %>% pivot_longer(died:success, 
                      names_to = "outcome",
                      values_to = "percent") %>%
  ggplot(aes(x = age, y = percent, color = outcome)) +
  geom_line(alpha = 0.7, size = 1.5) +
  scale_y_continuous(labels = scales::percent_format())

## 1.3 EDA Success vs Death
members %>% count(success, died) %>%
  group_by(success) %>% 
  mutate(percent = n/sum(n))

## 1.4 EDA by Peak Names
members %>% filter(!is.na(peak_name)) %>%
  mutate(peak_name = fct_lump(peak_name, prop = 0.05)) %>%
  count(peak_name, success) %>%
  group_by(success) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = peak_name, y = percent, fill = success)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Peak Name", y = "Percent", 
       title = "Expedition Success/Failure Rates") +
  scale_y_continuous(labels = scales::percent_format())


## 1.5 EDA by Seasons
members %>% 
  filter(season != "Unknown") %>%
  count(season, died) %>%
  group_by(season) %>%
  mutate(percent = n/sum(n),
         died = case_when(died ~ "Died",
                          TRUE ~ "Did not die")) %>%
  ggplot(aes(x = season, y = percent, fill = season)) +
  geom_col(show.legend = FALSE, position = "dodge", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~died, scales = "free")


# 2. Create a new dataset to build model
members_df <- members %>%
  filter(season != "Unknown") %>%
  select(peak_id, year, season, sex, age,
         citizenship, hired, success, died) %>%
  filter(!is.na(sex), !is.na(citizenship)) %>%
  mutate(died = case_when(died ~ "died",
                          TRUE ~ "survived")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.integer)

# 3. Build Models
set.seed(123)

## 3.1 Splitting Data:
members_split <- initial_split(members_df, strata = died)

members_train <- training(members_split)
member_test <- testing(members_split)

## 3.2 Resampling data using Cross Folds (10 cross folds)
set.seed(456)
members_fold <- vfold_cv(members_train)

## 3.3 Imputation and other Feature Engineering

## 3.3.1 Create a recipe by regressing the variable of interest 
# on all the other variables
members_rec <- recipe(died ~., data = members_train) %>%
  step_impute_median(age) %>%
  step_other(peak_id, citizenship) %>%
  step_dummy(all_nominal(), -died) %>%
  step_smote(died)

## 3.3.2 Prep the recipe to check if the class imbalance has been sorted
members_rec %>% 
  prep() %>% bake(new_data = NULL) %>%
  count(died)

## 3.4 Build basic models (Logistic and Random Forest)

# Logistic Regression
glm_spec <- logistic_reg() %>% set_engine("glm")

# Random Forest
rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

## 3.5 Create a workflow
members_wf <- workflow() %>% add_recipe(members_rec)

## 3.6 Add models to the workflow

## 3.6.1 Add workflow to logistic regression
glm_rs <- members_wf %>% add_model(glm_spec) %>%
  fit_resamples(resample = members_fold,
                metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
                control = control_resamples(save_pred = TRUE))

## Random Forest taking too much time

## 3.7 Evaluate models
collect_metrics(glm_rs)

glm_rs %>% conf_mat_resampled()

## 3.8 Visualize
glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(died, .pred_died) %>%
  autoplot()


## 3.9 Train on the training data
members_final <- members_wf %>% add_model(glm_spec) %>%
  last_fit(members_split) 

collect_metrics(members_final)
collect_predictions(members_final) %>%
  conf_mat(died, .pred_class)

## 4.1 Collect Variables
members_final %>% pull(.workflow) %>%
  pluck(1) %>%
  tidy(exponentiate = TRUE) %>%
  arrange(estimate) %>%
  kable(digits = 3)

# Positive estimates increase the probability of surviving
# such as going on an expedition in summer, being a citizen
# of the US/UK, et al.
# Estimates closer to 0 or negative, decrease the probability of survival.

## 4.2 Viusalize
members_final %>%
  pull(.workflow) %>%
  pluck(1) %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate, fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, color = "grey50", lty = 2, size = 1.2) +
  geom_errorbar(aes(xmin = estimate - std.error, 
                    xmax = estimate + std.error), 
                width = 0.2, alpha = 0.7) +
  geom_point(size = 2, color = "#85144B") +
  labs(y = NULL, x = "Coefficent from logistic regression")












































































































