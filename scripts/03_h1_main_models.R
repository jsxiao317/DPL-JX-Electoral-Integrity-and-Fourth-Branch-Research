rm(list = ls())
cat("\014")

library(dplyr)
library(readr)

# Load the country-year dataset with FB_strength + FraudIndex
h1 <- read_csv("data/derived/vdem_h1_country_year.csv", show_col_types = FALSE)

# Keep only rows where both IV and DV exist (i.e., election-coded observations)
h1_main <- h1 %>% filter(!is.na(FB_strength), !is.na(FraudIndex))

# Quick sanity checks
cat("Rows in h1:", nrow(h1), "\n")
cat("Rows in h1_main (used for model):", nrow(h1_main), "\n")
summary(h1_main$FB_strength)
summary(h1_main$FraudIndex)
