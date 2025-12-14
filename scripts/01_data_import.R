rm(list = ls())
cat("\014")

library(readr)
library(dplyr)

# ---- paths ----
nelda_path <- file.path("data", "raw", "NELDA.csv")
vdem_path  <- file.path("data", "raw", "V-Dem-CY-Full+Others-v15.csv")

# ---- safety checks ----
stopifnot(file.exists(nelda_path), file.exists(vdem_path))

# ---- NELDA import 
nelda <- read_tsv(
  nelda_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  progress = FALSE
)

# ---- V-Dem import ----
vdem <- read_csv(
  vdem_path,
  col_types = cols(.default = col_guess()),
  show_col_types = FALSE,
  progress = FALSE
)

# ---- optional: targeted repair of the two previously problematic columns (by position) ----
# Only run this if your earlier 'problems(vdem)' pointed at 2944 and 2945.
bad1 <- names(vdem)[2944]
bad2 <- names(vdem)[2945]

vdem[[bad1]] <- parse_double(vdem[[bad1]])
vdem[[bad2]] <- parse_double(vdem[[bad2]])

#---- sanity checks ----
stopifnot(is.data.frame(nelda), is.data.frame(vdem))

