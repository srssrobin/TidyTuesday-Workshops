library(tidymodels)
library(tidyverse)
library(ranger)
library(janitor)
library(ggrepel)

options(scipen=0)
theme_set(theme_light())

# 1. Load and clean data
key_crop_yields <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv")
land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")

# Identify the top countries (30):
top_countries <- land_use %>% janitor::clean_names() %>%
  na.exclude() %>%
  group_by(entity) %>%
  filter(year == max(year), entity != "World") %>%
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30) %>%
  pull(entity)


# Key Crop Yields dataset is in a Wide-Data format. We would now like to convert
# it into a Tidy, Long-Data format. We want to pivot all the crops into just one
# column. Therefore, pivot by Wheat to Bananas, and collect all the values.

# Clean the yields:
tidy_yields <- key_crop_yields %>%
  janitor::clean_names() %>%
  pivot_longer(wheat_tonnes_per_hectare:bananas_tonnes_per_hectare,
               names_to = "Crop", values_to = "Yield") %>%
  mutate(crop = str_remove(Crop, "_tonnes_per_hectare")) %>%
  filter(crop %in% c("wheat", "rice", "maize", "barley"), 
         entity %in% top_countries, 
         !is.na(Yield)) %>% select(-Crop)
  

# 2. Explore data:
tidy_yields %>%
  ggplot(aes(x = year, y = Yield, color = crop)) +
  geom_line(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~entity) +
  labs(x = NULL, y = "yield (tons per hectare)") +
  scale_color_viridis_d()


# 3. Develop many Linear Regression models:

# Create an LM framework:
tidy_lm <- tidy_yields %>% 
  nest(yields = c(year, Yield)) %>%
  mutate(model = map(yields, ~ lm(Yield ~ year, data = .x)))

# Extract the coefficients:
slopes <- tidy_lm %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year") %>%
  mutate(p.value = p.adjust(p.value))


# 4. Explore the results:
slopes %>%
  ggplot(aes(x = estimate, y = p.value, label = entity)) +
  geom_vline(xintercept = 0, lty = 2, size = 1.5, alpha = 0.7, color = "grey50") +
  geom_point(aes(color = crop), alpha = 0.8, size = 2.5, show.legend = FALSE) +
  geom_text_repel() +
  facet_wrap(~crop) +
  scale_y_log10() +
  labs(title = "Annual increase in crop yields (P-Value of Estimates)",
       y = "Log P-value") +
  scale_color_viridis_d()

# Everything on the right side is increasing, while everything on the negative side 
# is decreasing. Some barley and maize productions are decreasing.

# In the context of this visualization, p-value of an estimate 
# indicates the probability of witnessing the said estimate by chance. I.e.
# smaller p-values estimate more certainty of the estimate, and larger p-values
# indicate less certainty of the estimate.















