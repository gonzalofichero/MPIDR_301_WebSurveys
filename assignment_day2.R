# Loading libraries
library(tidyverse)
library(readr)


# Importing data
survey <- read_delim("Facebook_COVID19_surveys.csv", delim = ";")

weight <- read_delim("table_for_weights.csv", delim = ",")

codes <- read_delim("code_questions.txt", delim = ";")


