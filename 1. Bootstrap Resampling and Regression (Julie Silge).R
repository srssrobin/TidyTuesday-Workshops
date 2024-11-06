library(tidymodels)
library(tidyverse)
options(scipen = 0)
set.seed(123)
theme_set(theme_bw())

brewing_materials_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv")

brewing_materials_raw
brewing_materials_raw %>% count(type, sort = TRUE)
brewing_materials_raw %>% count(type, wt = month_current, sort = TRUE)

# Q1: How much sugar do beer-producers need per barrel of malt used?

## Exploratory Data Analysis (EDA)
brewing_filtered <- brewing_materials_raw %>%
  filter(type %in% c("Malt and malt products",
                     "Sugar and syrups",
                     "Hops (dry)"),
         year < 2016,
         month != 12 & year %in% 2008:2014) %>%
  mutate(date = paste0(year, "-", month, "-01"),
         date = lubridate::ymd(date)) 
  
brewing_filtered %>%
  ggplot(aes(x = date, y = month_current, color = type)) +
  geom_point()

brewing_materials <- brewing_filtered %>% 
  select(date, type, month_current) %>%
  pivot_wider(names_from = type, 
              values_from = month_current) %>%
  janitor::clean_names() 

brewing_materials %>% 
  ggplot(aes(malt_and_malt_products, sugar_and_syrups)) +
  geom_smooth(method = "lm") +
  geom_point() 

# Q2: How much sugar and syrups used for every barrel of malt
## Fit a Linear Regression
beer_fit <- lm(sugar_and_syrups ~ 0 + malt_and_malt_products, 
   data = brewing_materials)

summary(beer_fit)
tidy(beer_fit)

## Bootstrap resample regression: use the bootstraps() function
beer_boot <- bootstraps(brewing_materials,
                        times = 1000,
                        apparent = TRUE)

beer_models <- beer_boot %>%
  mutate(model = map(splits, ~ lm(sugar_and_syrups ~ 0 + malt_and_malt_products, 
                                data = .))) %>% 
  mutate(coef_info = map(model, tidy))

beer_coefs <- beer_models %>% unnest(coef_info)

## Evaluate results: draw histogram
mean_est <- mean(beer_coefs$estimate)
beer_coefs %>% 
  ggplot(aes(estimate)) +
  geom_histogram(alpha = 0.7) + 
  geom_vline(xintercept = mean_est, color = 'red', linetype = 'dashed')

## Create confidence intervals based on Bootstrap Coefficients 
# (Bootstrap Confidence Interval)
int_pctl(beer_models, coef_info)
summary(beer_coefs$estimate)

## Create an Augmented Dataset with predicted and training data for each Resample
beer_aug <- beer_models %>% 
              sample_n(1000) %>%
              mutate(augmented = map(model, augment)) %>%
              unnest(augmented)

beer_aug %>%
  ggplot(aes(x = malt_and_malt_products, y = sugar_and_syrups)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, color = "cyan3") +
  geom_point()
  




