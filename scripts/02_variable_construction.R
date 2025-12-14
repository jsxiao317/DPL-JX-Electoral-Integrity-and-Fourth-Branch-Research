rm(list = ls())
cat("\014")

source("scripts/00_setup.R")
source("scripts/01_data_import.R")

# TODO: construct H1 variables (EMB autonomy/capacity, fraud proxies)
# TODO: construct H2 variables (acceptance/legitimacy proxies)

# save derived datasets
# writer::write_csv(df, "data/derived/analysis_dataset.csv")

