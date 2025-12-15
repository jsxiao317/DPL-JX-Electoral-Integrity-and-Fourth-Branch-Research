rm(list = ls())
cat("\014")

library(dplyr)
library(readr)

source("scripts/00_setup.R")
source("scripts/01_data_import.R")

# ---- helper: z-score standardization ----
zscore <- function(x) {
  x <- as.numeric(x)
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# ---- build analysis frame + indices ----
vdem_h1 <- vdem %>%
  transmute(
    ccode = COWcode,
    year  = year,
    
    emb_autonomy = as.numeric(v2elembaut),
    emb_capacity = as.numeric(v2elembcap),
    
    # DV components
    irreg  = as.numeric(v2elirreg),
    votbuy = as.numeric(v2elvotbuy),
    intim  = as.numeric(v2elintim),
    frfair = as.numeric(v2elfrfair)
  ) %>%
  mutate(
    # IV: Fourth-branch strength (autonomy + capacity)
    FB_strength = 0.5 * (zscore(emb_autonomy) + zscore(emb_capacity)),
    
    # invert free/fair so "higher = worse"
    frfair_fraud = -zscore(frfair),
    
    # DV: FraudIndex (your requested 4-component definition)
    FraudIndex = 0.25 * (zscore(irreg) + zscore(votbuy) + zscore(intim) + frfair_fraud)
  )

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
write_csv(vdem_h1, "data/derived/vdem_h1_country_year.csv")
cat("Saved: data/derived/vdem_h1_country_year.csv\n")