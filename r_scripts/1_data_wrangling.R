# load packages
library(tidyverse)
library(skimr)
library(knitr)

# load data
disney_data <- read_csv("data/disney_boxoffice_history.csv")

# basic summary
summary_table <- disney_data %>%
  summarize(
    Total_Observations = n(),
    Total_Variables = ncol(disney_data),
    Numeric_Variables = sum(sapply(disney_data, is.numeric)),
    Categorical_Variables = sum(sapply(disney_data, is.character)),
    Missing_Values = sum(is.na(disney_data))
  ) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Metric") %>%
  rename(Value = V1)

# save / write out
save(summary_table, file = "data/data_summary.RData")

