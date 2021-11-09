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
  filter(SEX != "Prefer not to answer") %>%
  # Losing 1.8127% of answers from this filters
  group_by(country, macroarea, SEX, agegroup) %>%
  summarize(sampled_prop = n() / 13650) -> sampled_population
  
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
survey %>% 
  left_join(correct_pop, by = c("country", "macroarea", "SEX", "agegroup")) -> survey2


# Quick calculations to see differences with corrected weights

# By Sex
survey2 %>% 
  filter(!is.na(correct_w)) %>% 
  group_by(SEX, RISK_SQ002) %>% 
  summarize(unweight_result = n(),
            weight_result = sum(correct_w)) %>% 
  ggplot(aes(y=unweight_result, x = RISK_SQ002)) + geom_point(col = "red", alpha = 0.5, size=2) +
  geom_point(aes(y=weight_result, x = RISK_SQ002), col = "blue", alpha = 0.5, size=2) +
  facet_wrap(~SEX)
# Over representation of women in results for Family Risk



# By Age Group
survey2 %>% 
  filter(!is.na(correct_w)) %>% 
  group_by(agegroup, RISK_SQ002) %>% 
  summarize(unweight_result = n(),
            weight_result = sum(correct_w)) %>% 
  pivot_longer(cols = c(unweight_result, weight_result), names_to = "type_weight", values_to = "Count") %>% 
  mutate(type_weight = as.factor(type_weight)) %>% 
  ggplot(aes(x = RISK_SQ002, y = Count, color = type_weight)) + 
  geom_point(alpha = 0.5, size=2) +
  facet_wrap(~agegroup, ncol=2, nrow = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
# Sub-representation of old people. Over representation of 25-64 age group





