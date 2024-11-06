library(tidymodels)
library(tidyverse)
library(ranger)
library(janitor)
library(ggrepel)
library(maps)
library(themis)

options(scipen=0)
theme_set(theme_light())

## 1. Explore the data:
volcano_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv")

volcano_raw %>% count(primary_volcano_type, sort = TRUE)

# We want to create a classification of to predict the volcano type 
# based on the given information. This is a multinomial classification problem.

# Transmute() creates a new dataframe containing only the specified computations
# We are only using 3: Stratovolcano, Shield, Others

volcano_df <- volcano_raw %>% 
  transmute(volcano_type = case_when(str_detect(primary_volcano_type,
                                                "Stratovolcano") ~ 
                                       "Stratovolcano", 
                                     str_detect(primary_volcano_type,
                                                "Shield") ~ 
                                       "Shield",
                                     TRUE ~ "Other"),
            volcano_number, latitude, longitude, elevation,
            tectonic_settings, major_rock_1) %>%
  mutate_if(is.character, factor) # if any column is character, change it to factor


## 2. Create a World map:
world <- map_data("world")

ggplot() +
  geom_map(data = world,
           map = world,
           aes(x = long, y = lat, map_id = region),
           color = "white", fill = "gray50", alpha = 0.2) +
  geom_point(data = volcano_df,
             aes(x = longitude, y = latitude, color = volcano_type),
             alpha = 0.8)


## 3. Perform data pre-processing: Recipe -> Prep -> Juice

# Create 25 bootstrap resamples:
  # instead of creating a test/train set, we are going to create 25 resamples
  # and in each of these bootstraps samples, we are going to train on the analysis
  # set of bootstrap resample, evaluate on the assessment set of the bootstrap
  # resample, and then use that as a measure of how well our multinomial model
  # is performing.
  # The estimates are likely to be biased since the dataset is really small.

volcano_boot <- bootstraps(volcano_df) 
volcano_boot

volcano_df %>% count(volcano_type, sort = TRUE)

# Recognize that there is a class im-balance: Shield type is significantly lower 
# than the other types.

volcano_df %>% count(tectonic_settings, sort = TRUE)

# Use the themis package and the recipe formula:
  # step_other collapses the small count types
  # update_role() alters/sets new role: we are using Volcano Number as an ID column
  # step_dummy() converts breaks down columns into dummy variables
  # step_zv() removes anything with 0 variance
  # step_smote() balances the minority class using the smote algorithm

# 3.1 Create a recipe: Recipes are used to perform feature engineering
volcano_rec <- recipe(volcano_type ~. , data = volcano_df) %>%
  update_role(volcano_number, new_role = "Id") %>%
  step_other(tectonic_settings) %>%
  step_other(major_rock_1) %>%
  step_dummy(tectonic_settings, major_rock_1) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_smote(volcano_type)
  
# 3.2 Create a prep: Actually calculates; preps the recipe
volcano_prep <- prep(volcano_rec)

# Inspect the new data using juice()
juice(volcano_prep) %>% count(volcano_type)

## 4. Build a model:

# 4.1 Create a Random Forest Specification:
rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger") 
  
# 4.2 Create a modeling workflow:
  # A workflow is a way to hold/stick things together.
  # add the un-prepped recipe to the workflow via add_recipe()
  # add the model to the workflow via add_model(rf_spec)

volcano_wf <- workflow() %>% 
  add_recipe(volcano_rec) %>%
  add_model(rf_spec)


# 4.3 Fit the model to the bootstrap resamples
volcano_res <- fit_resamples(
  volcano_wf,
  resamples = volcano_boot,
  control = control_resamples(save_pred = TRUE)
)

## 5. Explore the results: (check the predictions list)

# 5.1. Collect metrics:
volcano_res %>%
  collect_metrics()

# 5.2. Collect predictions:
volcano_res %>%
  collect_predictions() %>% 
  group_by(id) %>%
  ppv(volcano_type, .pred_class)

volcano_pred <- volcano_res %>%
  collect_predictions() %>% 
  mutate(correct = volcano_type == .pred_class) %>%
  left_join(volcano_df %>% mutate(.row = row_number()))

volcano_pred  

## 6. Visualize the predictions:
ggplot() +
  geom_map(data = world,
           map = world,
           aes(x = long, y = lat, map_id = region),
           color = "white", fill = "gray50", alpha = 0.2) +
  stat_summary_hex(data = volcano_pred,
             aes(x = longitude, y = latitude, z = as.integer(correct)),
             fun = "mean",
             bins = 60,
             alpha = 0.7) +
  scale_fill_gradient(high = "cyan3", labels = scales::percent) +
  labs(fill = "Percent Classified")



























































