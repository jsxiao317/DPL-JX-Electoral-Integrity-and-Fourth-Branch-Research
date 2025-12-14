rm(list = ls())
cat("\014")

library(readr)
library(dplyr)

# ---- paths ----
nelda_path <- file.path("data", "raw", "NELDA.csv")
vdem_path  <- file.path("data", "raw", "V-Dem-CY-Full+Others-v15.csv")

# ---- NELDA import ----
nelda <- read_tsv(nelda_path, show_col_types = FALSE)

# ---- V-Dem import ----
vdem <- read_csv(
  vdem_path,
  col_types = cols(.default = col_double()),
  show_col_types = FALSE
)

