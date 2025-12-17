rm(list = ls())
cat("\014")

# ============================================================
# H1 MAIN MODELS
# DV: FraudIndex
# IV: FB_strength
# Units: country-year (ccode-year)
# ============================================================

# ---- packages (install if missing) ----
pkgs <- c("dplyr", "readr", "lme4", "lmerTest", "clubSandwich")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- load derived dataset (must exist locally) ----
stopifnot(file.exists("data/derived/vdem_h1_country_year.csv"))
h1 <- read_csv("data/derived/vdem_h1_country_year.csv", show_col_types = FALSE)

# ---- main analysis sample: require DV, IV, and cluster id ----
h1_main <- h1 %>%
  filter(
    !is.na(FB_strength),
    !is.na(FraudIndex),
    !is.na(ccode),
    !is.na(year)
  )

# ---- sanity checks ----
cat("Rows in h1:", nrow(h1), "\n")
cat("Rows in h1_main (used for models):", nrow(h1_main), "\n")
cat("NA in ccode (should be 0):", sum(is.na(h1_main$ccode)), "\n")
cat("NA in year  (should be 0):", sum(is.na(h1_main$year)), "\n\n")

# ============================================================
# MODEL 1: OLS + year fixed effects + country-clustered SEs
# FraudIndex_ct = a + b*FB_strength_ct + yearFE + e_ct
# ============================================================

# Fit
m1 <- lm(
  FraudIndex ~ FB_strength + factor(year),
  data = h1_main,
  na.action = na.omit
)

# ---- CRUCIAL: align cluster vector to the exact rows used by m1 ----
# When you fit lm(), it can drop rows internally. cluster must match exactly.
used_rows <- as.integer(names(residuals(m1)))     # row indices from h1_main
cluster_id <- h1_main$ccode[used_rows]

# Hard checks
stopifnot(length(cluster_id) == length(residuals(m1)))
stopifnot(!anyNA(cluster_id))

# Cluster-robust inference (CR2 recommended)
m1_cl <- clubSandwich::coef_test(m1, vcov = "CR2", cluster = cluster_id)

cat("\n===== MODEL 1: OLS + Year FE (CR2 clustered by country) =====\n")
print(m1_cl)

# ============================================================
# MODEL 2: Multilevel random intercept by country + year FE
# FraudIndex_ct = a + b*FB_strength_ct + yearFE + u_c + e_ct
# ============================================================

m2 <- lmer(
  FraudIndex ~ FB_strength + factor(year) + (1 | ccode),
  data = h1_main,
  REML = FALSE
)

cat("\n===== MODEL 2: Multilevel random intercept (country) =====\n")
print(summary(m2))

# ============================================================
# MODEL 3: Mundlak decomposition (within vs between)
# FB_strength_ct = FB_between_c + FB_within_ct
# FraudIndex_ct = a + bW*FB_within_ct + bB*FB_between_c + yearFE + u_c + e_ct
# ============================================================

h1_mund <- h1_main %>%
  group_by(ccode) %>%
  mutate(
    FB_between = mean(FB_strength, na.rm = TRUE),
    FB_within  = FB_strength - FB_between
  ) %>%
  ungroup()

m3 <- lmer(
  FraudIndex ~ FB_within + FB_between + factor(year) + (1 | ccode),
  data = h1_mund,
  REML = FALSE
)

cat("\n===== MODEL 3: Mundlak multilevel (within + between) =====\n")
print(summary(m3))

# ============================================================
# Save outputs
# ============================================================

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

writeLines(capture.output(m1_cl),       "output/tables/H1_m1_OLS_yearFE_clustered_CR2.txt")
writeLines(capture.output(summary(m2)), "output/tables/H1_m2_multilevel.txt")
writeLines(capture.output(summary(m3)), "output/tables/H1_m3_mundlak.txt")

cat("\nSaved tables to output/tables/\n")