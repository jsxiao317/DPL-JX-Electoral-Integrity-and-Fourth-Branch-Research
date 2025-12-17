rm(list = ls())
cat("\014")

# ============================================================
# H1 CROSS-SOURCE VALIDATION (NELDA as DV)
# IV: FB_strength (from V-Dem-derived country-year dataset)
# DV: Fraud / Irregularities proxy from NELDA (election-year)
# ============================================================

library(dplyr)
library(readr)
library(stringr)

# --- Load objects + data paths from your pipeline ---
source("scripts/00_setup.R")
source("scripts/01_data_import.R")

# --- Load V-Dem derived H1 country-year dataset (must already exist) ---
stopifnot(file.exists("data/derived/vdem_h1_country_year.csv"))
vdem_h1 <- read_csv("data/derived/vdem_h1_country_year.csv", show_col_types = FALSE)

# ============================================================
# (A) Diagnose NELDA structure and locate likely fraud columns
# ============================================================

cat("\nNELDA rows:", nrow(nelda), "\n")
cat("NELDA cols:", ncol(nelda), "\n\n")

# Candidate fraud-ish column search (no assumptions)
name_hits <- names(nelda)[str_detect(tolower(names(nelda)),
                                     "fraud|rig|irreg|manip|cheat|ballot|intimid|viol|register|count")]
cat("Possible NELDA fraud-related columns found:\n")
print(name_hits)

# Candidate country/year key search
key_hits <- names(nelda)[str_detect(tolower(names(nelda)),
                                    "^year$|year|^ccode$|cow|^country$|country|^cname$|name")]
cat("\nPossible NELDA key columns found:\n")
print(key_hits)

# ============================================================
# (B) Define JOIN KEYS robustly (ccode preferred)
# ============================================================

# ---- Identify year column ----
year_col <- if ("year" %in% names(nelda)) "year" else {
  # fallback: first column containing "year"
  y <- names(nelda)[str_detect(tolower(names(nelda)), "year")][1]
  if (is.na(y)) stop("No year column found in NELDA. Inspect names(nelda).")
  y
}

# ---- Identify country code column (preferred) ----
ccode_col <- if ("ccode" %in% names(nelda)) "ccode" else {
  # fallback: common variants
  cand <- names(nelda)[str_detect(tolower(names(nelda)), "ccode|cow")]
  if (length(cand) == 0) NA_character_ else cand[1]
}

# ---- Identify country name column (fallback) ----
cname_col <- if ("country" %in% names(nelda)) "country" else {
  cand <- names(nelda)[str_detect(tolower(names(nelda)), "country|cname|name")]
  if (length(cand) == 0) NA_character_ else cand[1]
}

# ============================================================
# (C) Construct NELDA DV: fraud indicator/index (user-confirmable)
# ============================================================
# We DO NOT hardcode a specific NELDA fraud variable name (since versions vary).
# Instead, we do:
# 1) you choose from `name_hits`
# 2) we build a binary DV robustly

# ---- YOU MUST SET THIS after inspecting printed name_hits ----
# Example placeholders (replace with real NELDA column name(s) you want)
fraud_cols <- c()   # <-- put e.g. c("nelda33", "nelda34") OR c("fraud")

if (length(fraud_cols) == 0) {
  stop(
    "STOP: You must set `fraud_cols` to the correct NELDA fraud variable(s).\n",
    "Look at the printed 'Possible NELDA fraud-related columns found' and choose.\n",
    "Then re-run source('scripts/05_h1_cross_source_validation_nelda.R')."
  )
}

missing_cols <- fraud_cols[!fraud_cols %in% names(nelda)]
if (length(missing_cols) > 0) {
  stop("These fraud_cols are not in NELDA: ", paste(missing_cols, collapse = ", "))
}

# Convert fraud columns to numeric safely (handles '0/1', TRUE/FALSE, text)
to_num01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  # character: extract first number if present, else NA
  x2 <- str_trim(as.character(x))
  # common true/false
  x2 <- case_when(
    tolower(x2) %in% c("true","t","yes","y") ~ "1",
    tolower(x2) %in% c("false","f","no","n") ~ "0",
    TRUE ~ x2
  )
  suppressWarnings(as.numeric(str_extract(x2, "[-]?[0-9]+\\.?[0-9]*")))
}

nelda_work <- nelda %>%
  mutate(
    year = as.integer(.data[[year_col]]),
    ccode = if (!is.na(ccode_col)) as.integer(.data[[ccode_col]]) else NA_integer_,
    country_name = if (!is.na(cname_col)) as.character(.data[[cname_col]]) else NA_character_
  )

# Build DV as "any fraud signal" across chosen columns (binary 0/1)
nelda_work <- nelda_work %>%
  mutate(across(all_of(fraud_cols), to_num01)) %>%
  rowwise() %>%
  mutate(
    Fraud_NELDA = {
      vals <- c_across(all_of(fraud_cols))
      if (all(is.na(vals))) NA_real_
      else as.numeric(any(vals == 1, na.rm = TRUE))
    }
  ) %>%
  ungroup()

cat("\nNELDA DV summary (Fraud_NELDA):\n")
print(table(nelda_work$Fraud_NELDA, useNA = "ifany"))

# ============================================================
# (D) Merge NELDA (election-year) with V-Dem H1 index (country-year)
# ============================================================

# Ensure vdem_h1 has consistent key names
vdem_h1 <- vdem_h1 %>%
  mutate(
    year = as.integer(year),
    ccode = as.integer(ccode)
  )

# Prefer joining by ccode+year if possible
can_join_ccode <- all(!is.na(nelda_work$ccode)) && "ccode" %in% names(vdem_h1)

if (can_join_ccode) {
  h1_nelda <- nelda_work %>%
    left_join(vdem_h1 %>% select(ccode, year, FB_strength), by = c("ccode","year"))
} else {
  # fallback: join by country name + year (riskier)
  if (all(is.na(nelda_work$country_name))) {
    stop("Cannot join: NELDA has no usable ccode AND no country_name.")
  }
  if (!("country_name" %in% names(vdem_h1))) {
    stop("Cannot join by name: vdem_h1 has no country_name column.")
  }
  
  h1_nelda <- nelda_work %>%
    left_join(vdem_h1 %>% select(country_name, year, FB_strength),
              by = c("country_name","year"))
}

cat("\nRows after merge:", nrow(h1_nelda), "\n")
cat("Missing FB_strength after merge:", sum(is.na(h1_nelda$FB_strength)), "\n")

# Keep usable rows
h1_nelda_main <- h1_nelda %>%
  filter(!is.na(Fraud_NELDA), !is.na(FB_strength), !is.na(year)) %>%
  mutate(
    year_f = factor(year),
    ccode_f = factor(ifelse(!is.na(ccode), ccode, country_name))
  )

cat("\nRows used for NELDA validation model:", nrow(h1_nelda_main), "\n")

# ============================================================
# (E) Models: logistic (Fraud_NELDA is binary) + multilevel
# ============================================================

pkgs <- c("lme4","lmerTest","clubSandwich","lmtest","broom","broom.mixed","ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# (1) Logistic regression with year FE (pooled)
m1_n <- glm(Fraud_NELDA ~ FB_strength + year_f,
            data = h1_nelda_main, family = binomial())

# CR2 clustered by country (ccode_f)
m1_n_cr2 <- clubSandwich::coef_test(
  m1_n,
  vcov = "CR2",
  cluster = h1_nelda_main$ccode_f
)

cat("\n===== NELDA MODEL 1: Logit + Year FE (CR2 clustered) =====\n")
print(m1_n_cr2)

# (2) Multilevel logit: random intercept by country + year FE
m2_n <- glmer(Fraud_NELDA ~ FB_strength + year_f + (1 | ccode_f),
              data = h1_nelda_main, family = binomial(),
              control = glmerControl(optimizer = "bobyqa"))

cat("\n===== NELDA MODEL 2: Multilevel logit (random intercept country) =====\n")
print(summary(m2_n))

# ============================================================
# (F) Save outputs
# ============================================================

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

writeLines(capture.output(m1_n_cr2),
           "output/tables/H1_NELDA_m1_logit_yearFE_CR2.txt")
writeLines(capture.output(summary(m2_n)),
           "output/tables/H1_NELDA_m2_multilevel_logit.txt")

# Coef plot (FB_strength only, clean)
tidy_m2 <- broom.mixed::tidy(m2_n, effects = "fixed") %>%
  filter(term == "FB_strength") %>%
  mutate(
    lo = estimate - 1.96 * std.error,
    hi = estimate + 1.96 * std.error
  )

p <- ggplot(tidy_m2, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
  coord_flip() +
  labs(
    title = "NELDA validation: Effect of FB_strength on fraud (multilevel logit)",
    x = NULL,
    y = "Log-odds coefficient (±1.96 SE)"
  )

ggsave("output/figures/H1_NELDA_coefplot_m2.png", p, width = 8, height = 4, dpi = 300)

cat("\nSaved tables to output/tables/ and figure to output/figures/\n")