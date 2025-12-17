rm(list = ls())
cat("\014")

# ================================
# H1 Cross-source validation:
# Compare V-Dem FraudIndex against a NELDA fraud indicator
# ================================

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# -------------------------------
# 1) Load V-Dem derived dataset (country-year)
# -------------------------------
h1 <- read_csv("data/derived/vdem_h1_country_year.csv", show_col_types = FALSE)

# Ensure key columns exist
need <- c("ccode", "year", "FB_strength", "FraudIndex")
missing_need <- setdiff(need, names(h1))
if (length(missing_need) > 0) stop("Missing required columns in h1: ", paste(missing_need, collapse = ", "))

# Force types safely
h1 <- h1 %>%
  mutate(
    ccode = suppressWarnings(as.integer(ccode)),
    year  = suppressWarnings(as.integer(year))
  )

# IMPORTANT: drop any rows with missing ccode/year and keep only election-coded obs
h1_main <- h1 %>%
  filter(!is.na(ccode), !is.na(year), !is.na(FB_strength), !is.na(FraudIndex))

cat("Rows in h1:", nrow(h1), "\n")
cat("Rows in h1_main (used for validation):", nrow(h1_main), "\n")
cat("NA in ccode (should be 0):", sum(is.na(h1_main$ccode)), "\n")
cat("NA in year  (should be 0):", sum(is.na(h1_main$year)), "\n\n")

# -------------------------------
# 2) Load NELDA (it is tab-delimited even if named .csv)
#    Force everything to character to avoid parsing warnings.
# -------------------------------
nelda_path <- file.path("data", "raw", "NELDA.csv")

nelda <- read_tsv(
  nelda_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

# Sanity checks
cat("NELDA rows:", nrow(nelda), " cols:", ncol(nelda), "\n")
cat("Has ccode?  ", "ccode" %in% names(nelda), "\n")
cat("Has year?   ", "year"  %in% names(nelda), "\n\n")

if (!("ccode" %in% names(nelda))) stop("Could not find ccode column in NELDA.")
if (!("year"  %in% names(nelda))) stop("Could not find year column in NELDA.")

nelda <- nelda %>%
  mutate(
    ccode = suppressWarnings(as.integer(ccode)),
    year  = suppressWarnings(as.integer(year))
  )

# -------------------------------
# 3) Choose NELDA fraud indicator
#    You already selected nelda11 and it worked, so we lock it in.
# -------------------------------
fraud_cols <- c("nelda11")
missing_fraud <- setdiff(fraud_cols, names(nelda))
if (length(missing_fraud) > 0) stop("NELDA fraud column not found: ", paste(missing_fraud, collapse = ", "))

cat("Using NELDA fraud column(s):", paste(fraud_cols, collapse = ", "), "\n\n")

# Convert nelda11 to 0/1:
# NELDA often stores yes/no; we treat "yes" (case-insensitive) as 1 and "no" as 0.
nelda <- nelda %>%
  mutate(
    nelda_fraud = case_when(
      str_to_lower(.data[[fraud_cols[1]]]) == "yes" ~ 1L,
      str_to_lower(.data[[fraud_cols[1]]]) == "no"  ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# -------------------------------
# 4) Merge: V-Dem country-year with NELDA election-year fraud indicator
# -------------------------------
merged <- h1_main %>%
  left_join(nelda %>% select(ccode, year, nelda_fraud), by = c("ccode", "year"))

cat("Merged rows:", nrow(merged), "\n")
cat("NELDA matched (non-NA nelda_fraud):", sum(!is.na(merged$nelda_fraud)), "\n\n")

# -------------------------------
# 5) Validation outputs
#    (A) Difference in FraudIndex by NELDA fraud
# -------------------------------
diffmeans <- merged %>%
  filter(!is.na(nelda_fraud)) %>%
  group_by(nelda_fraud) %>%
  summarise(
    n = n(),
    mean_FraudIndex = mean(FraudIndex, na.rm = TRUE),
    sd_FraudIndex   = sd(FraudIndex, na.rm = TRUE),
    .groups = "drop"
  )

cat("Difference in FraudIndex by NELDA fraud indicator:\n")
print(diffmeans)
cat("\n")

# (B) Simple validation regression with year FE
m_val <- lm(FraudIndex ~ nelda_fraud + factor(year),
            data = merged %>% filter(!is.na(nelda_fraud)))

cat("Validation model (FraudIndex ~ NELDA fraud + year FE):\n")
print(summary(m_val)$coefficients)
cat("\n")

# -------------------------------
# 6) Save merged data + tables + figure
# -------------------------------
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

write_csv(merged, "data/derived/h1_with_nelda_validation.csv")

writeLines(capture.output(diffmeans), "output/tables/H1_NELDA_validation_diffmeans.txt")
writeLines(capture.output(summary(m_val)), "output/tables/H1_NELDA_validation_model.txt")

p <- merged %>%
  filter(!is.na(nelda_fraud)) %>%
  mutate(nelda_fraud = factor(nelda_fraud, levels = c(0,1), labels = c("NELDA: No fraud", "NELDA: Fraud"))) %>%
  ggplot(aes(x = nelda_fraud, y = FraudIndex)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25) +
  labs(x = NULL, y = "V-Dem FraudIndex", title = "Cross-source validation: V-Dem FraudIndex vs NELDA fraud (nelda11)") +
  theme_minimal()

ggsave("output/figures/H1_validation_VDem_vs_NELDA_boxjitter.png", p, width = 8, height = 5, dpi = 300)

cat("Saved:\n")
cat("- data/derived/h1_with_nelda_validation.csv\n")
cat("- output/tables/H1_NELDA_validation_diffmeans.txt\n")
cat("- output/tables/H1_NELDA_validation_model.txt\n")
cat("- output/figures/H1_validation_VDem_vs_NELDA_boxjitter.png\n")