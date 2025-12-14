rm(list = ls())
cat("\014")

pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr",
  "lubridate", "janitor"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)

invisible(lapply(pkgs, library, character.only = TRUE))

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# Standardize to z-scores safely
zscore <- function(x) {
  x <- as.numeric(x)
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Convert common yes/no/true/false strings to 1/0 (NELDA)
yn_to_01 <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("yes", "y", "true", "t", "1") ~ 1,
    x %in% c("no",  "n", "false","f", "0") ~ 0,
    TRUE ~ NA_real_
  )
}

# Quick NA rate
na_rate <- function(x) mean(is.na(x))
