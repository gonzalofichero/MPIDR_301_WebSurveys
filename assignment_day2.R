# Loading libraries
library(tidyverse)
library(readr)


# Importing data
survey <- read_delim("Facebook_COVID19_surveys.csv", delim = ";")

weight <- read_delim("table_for_weights.csv", delim = ",")

codes <- read_delim("code_questions.txt", delim = ";")


glimpse(survey)
glimpse(weight)


# Let's check the % of real population to compare later

weight %>% 
  mutate(total = sum(true_count)) %>% 
  group_by(country, macroarea, SEX, agegroup) %>% 
  summarize(true_prop = true_count / total) -> real_population

survey %>% 
  filter(!is.na(macroarea)) %>% 
  group_by(country, macroarea, SEX, agegroup) %>%
  summarize(sampled_prop = n() / 13721) -> sampled_population
  
# Comparing
real_population %>% 
  left_join(sampled_population, by = c("country", "macroarea", "SEX", "agegroup")) %>% 
  # Calculating correction weight
  mutate(correct_w = true_prop / sampled_prop,
         # Logical var for over or sub representation
         IS_over = case_when(true_prop > sampled_prop ~ "01 - Sub-representation",
                             sampled_prop > true_prop ~ "02 - Over-representation",
                             TRUE ~ "03 - Correct")) -> correct_pop


# Now it's time to add the correction weights into the survey to start calculating stuff




