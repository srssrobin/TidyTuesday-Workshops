library(tidymodels)
library(tidyverse)
library(ranger)
library(janitor)

options(scipen=0)
theme_set(theme_minimal())


data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv")


## Explore data
employed_tidy <- data %>% 
  filter(!is.na(employ_n)) %>%
  group_by(occupation = paste(industry, minor_occupation), race_gender) %>%
  summarise(n = mean(employ_n)) %>%
  ungroup()

employed_tidy %>% 
  filter(race_gender == "TOTAL")

employed_tidy %>%
  filter(race_gender %in% c("Women", "Black or African American", "Asian")) %>%
  pivot_wider(names_from = race_gender,
              values_from = n,
              values_fill = 0) %>% janitor::clean_names() %>% 
  left_join(employed_tidy %>% 
              filter(race_gender == "TOTAL") %>%
              select(-race_gender) %>% 
              rename(total = n)) %>%
  mutate(across(c(asian, black_or_african_american, women), ~ . / total))










