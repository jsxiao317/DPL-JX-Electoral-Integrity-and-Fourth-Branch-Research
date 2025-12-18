# =============================================================================
# 05_h1_cross_source_validation_nelda.R
# Cross-source validation: V-Dem FraudIndex vs NELDA election fraud indicator
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(clubSandwich)
})

# ----------------------------
# 0) User settings (EDIT HERE)
# ----------------------------

# Which NELDA variable(s) should represent fraud?
# Default based on your runs:
fraud_cols <- c("nelda11")  # <-- change if you want a different NELDA fraud measure

# Where is the raw NELDA file in your repo?
nelda_path <- file.path("data", "raw", "NELDA.csv")

# Output paths
derived_out <- file.path("data", "derived", "h1_with_nelda_validation.csv")
table_diff_out <- file.path("output", "tables", "H1_NELDA_validation_diffmeans.txt")
table_model_out <- file.path("output", "tables", "H1_NELDA_validation_model.txt")
fig_out <- file.path("output", "figures", "H1_validation_VDem_vs_NELDA_boxjitter.png")

# ----------------------------
# 1) Ensure required objects exist
# ----------------------------

if (!exists("h1") || !exists("h1_main")) {
  message("`h1` and/or `h1_main` not found. Sourcing scripts/01_data_import.R ...")
  if (!file.exists(file.path("scripts", "01_data_import.R"))) {
    stop("Could not find scripts/01_data_import.R. Run data import first.")
  }
  source(file.path("scripts", "01_data_import.R"))
}

if (!("ccode" %in% names(h1_main))) stop("`h1_main` must contain a `ccode` column.")
if (!("year"  %in% names(h1_main))) stop("`h1_main` must contain a `year` column.")
if (!("FraudIndex" %in% names(h1_main))) stop("`h1_main` must contain `FraudIndex`.")

# Print basic diagnostics (matches your console style)
message(sprintf("Rows in h1: %d", nrow(h1)))
message(sprintf("Rows in h1_main (used for validation): %d", nrow(h1_main)))
message(sprintf("NA in ccode (should be 0): %d", sum(is.na(h1_main$ccode))))
message(sprintf("NA in year  (should be 0): %d", sum(is.na(h1_main$year))))

# ----------------------------
# 2) Load NELDA
# ----------------------------

if (!file.exists(nelda_path)) {
  stop(sprintf("NELDA file not found at: %s\nPut it in data/raw/ (or update nelda_path).", nelda_path))
}

# NELDA is often tab-delimited even when named .csv
nelda <- read_delim(
  nelda_path,
  delim = "\t",
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A"),
  guess_max = 20000,
  show_col_types = FALSE
)

# Lowercase names to be safe
names(nelda) <- tolower(names(nelda))

# Convert key merge columns
# (NELDA ccode/year sometimes read as character)
if ("ccode" %in% names(nelda)) nelda <- nelda %>% mutate(ccode = suppressWarnings(as.numeric(ccode)))
if ("year"  %in% names(nelda)) nelda <- nelda %>% mutate(year  = suppressWarnings(as.integer(year)))

message(sprintf("\nNELDA rows: %d  cols: %d", nrow(nelda), ncol(nelda)))
message(sprintf("Has ccode?   %s", "ccode" %in% names(nelda)))
message(sprintf("Has year?    %s", "year"  %in% names(nelda)))

# Parsing problems (informative, but not fatal)
nelda_probs <- problems(nelda)
if (nrow(nelda_probs) > 0) {
  message("\nWARNING: NELDA parsing issues found. Showing first 10:")
  print(head(nelda_probs, 10))
}

# ----------------------------
# 3) Choose / validate fraud columns
# ----------------------------

missing_fraud_cols <- setdiff(tolower(fraud_cols), names(nelda))
if (length(missing_fraud_cols) > 0) {
  # Help user: print likely candidates
  possible <- names(nelda)[str_detect(names(nelda), "^nelda\\d+$")]
  message("\nPossible NELDA nelda# columns found (first 60 shown):")
  print(head(possible, 60))
  stop(sprintf(
    "STOP: `fraud_cols` contains columns not in NELDA: %s\nEdit fraud_cols at the top of this script and re-run.",
    paste(missing_fraud_cols, collapse = ", ")
  ))
}

message(sprintf("\nUsing NELDA fraud column(s): %s", paste(fraud_cols, collapse = ", ")))

# Helper to convert yes/no/1/0/TRUE/FALSE to 1/0 (else NA)
as_01 <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("1", "true", "t", "yes", "y") ~ 1L,
    x %in% c("0", "false", "f", "no",  "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

nelda_fraud_df <- nelda %>%
  mutate(across(all_of(fraud_cols), as_01)) %>%
  mutate(
    nelda_fraud = case_when(
      # if ALL are NA -> NA
      if_all(all_of(fraud_cols), ~ is.na(.x)) ~ NA_integer_,
      # if ANY equals 1 -> 1
      if_any(all_of(fraud_cols), ~ .x == 1L) ~ 1L,
      # otherwise -> 0
      TRUE ~ 0L
    )
  ) %>%
  select(ccode, year, country = any_of("country"), nelda_fraud) %>%
  distinct()

# ----------------------------
# 4) Merge onto h1_main
# ----------------------------

# Ensure h1_main types
h1_main <- h1_main %>%
  mutate(
    ccode = suppressWarnings(as.numeric(ccode)),
    year  = suppressWarnings(as.integer(year))
  )

merged <- h1_main %>%
  left_join(nelda_fraud_df, by = c("ccode", "year"))

message(sprintf("\nMerged rows: %d", nrow(merged)))
message(sprintf("NELDA matched (non-NA nelda_fraud): %d", sum(!is.na(merged$nelda_fraud))))

# Create country_name if missing (prevents your “country_name doesn’t exist” error)
if (!("country_name" %in% names(merged))) {
  if ("country" %in% names(merged)) {
    merged <- merged %>% mutate(country_name = country)
  } else {
    merged <- merged %>% mutate(country_name = NA_character_)
  }
}

# Save merged dataset (derived)
dir.create(dirname(derived_out), recursive = TRUE, showWarnings = FALSE)
write_csv(merged, derived_out)

# ----------------------------
# 5) Diff-in-means table
# ----------------------------

diff_tbl <- merged %>%
  filter(!is.na(nelda_fraud), !is.na(FraudIndex)) %>%
  group_by(nelda_fraud) %>%
  summarise(
    n = n(),
    mean_FraudIndex = mean(FraudIndex),
    sd_FraudIndex = sd(FraudIndex),
    .groups = "drop"
  )

message("\nDifference in FraudIndex by NELDA fraud indicator:")
print(diff_tbl)

dir.create(dirname(table_diff_out), recursive = TRUE, showWarnings = FALSE)
capture.output(diff_tbl, file = table_diff_out)

# ----------------------------
# 6) Validation regression with CR2 clustered SEs
#    FraudIndex ~ nelda_fraud + year FE
# ----------------------------

val_dat <- merged %>%
  filter(!is.na(nelda_fraud), !is.na(FraudIndex), !is.na(year), !is.na(ccode))

val_mod <- lm(FraudIndex ~ nelda_fraud + factor(year), data = val_dat)

# CR2 variance, clustered by country code (ccode)
V_cr2 <- vcovCR(val_mod, cluster = val_dat$ccode, type = "CR2")
ct <- coef_test(val_mod, vcov = V_cr2, test = "Satterthwaite")

message("\nValidation model (FraudIndex ~ NELDA fraud + year FE) [CR2 clustered by ccode]:")
print(ct)

dir.create(dirname(table_model_out), recursive = TRUE, showWarnings = FALSE)
capture.output(ct, file = table_model_out)

# ----------------------------
# 7) Figure: V-Dem FraudIndex by NELDA fraud
# ----------------------------

plot_dat <- val_dat %>%
  mutate(nelda_fraud = factor(nelda_fraud, levels = c(0, 1), labels = c("NELDA: No fraud", "NELDA: Fraud")))

p <- ggplot(plot_dat, aes(x = nelda_fraud, y = FraudIndex)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.18, alpha = 0.25, size = 0.9) +
  labs(
    x = NULL,
    y = "V-Dem FraudIndex",
    title = "Cross-source validation: V-Dem FraudIndex vs NELDA fraud indicator"
  ) +
  theme_minimal(base_size = 12)

dir.create(dirname(fig_out), recursive = TRUE, showWarnings = FALSE)
ggsave(fig_out, p, width = 9, height = 5, dpi = 300)

message("\nSaved:")
message(paste0("- ", derived_out))
message(paste0("- ", table_diff_out))
message(paste0("- ", table_model_out))
message(paste0("- ", fig_out))

# =============================================================================
# Notes:
# - If you ever see: "object 'nelda_path' not found", it means you were NOT
#   running this full script version (or the variable got removed). This file
#   defines nelda_path at the top.
# - Don't run `git status` inside the R console. Run it in Terminal.
# =============================================================================

