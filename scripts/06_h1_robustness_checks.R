# =============================================================================
# 06_h1_robustness_checks.R
# Robustness checks for H1:
#  - Two-way FE (country + year)
#  - Lagged FB_strength
#  - Placebo (lead FB_strength)
#  - Nonlinearity (bins)
# Saves tables + figures to output/
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# fixest is best for TWFE; install if missing
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
if (!requireNamespace("modelsummary", quietly = TRUE)) install.packages("modelsummary")

library(fixest)
library(modelsummary)

h1_path <- file.path("data", "derived", "vdem_h1_country_year.csv")

out_table <- file.path("output", "tables", "H1_robustness_models.txt")
out_fig   <- file.path("output", "figures", "H1_robustness_coefplot.png")
out_fig2  <- file.path("output", "figures", "H1_robustness_binned.png")

dir.create(dirname(out_table), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_fig),   recursive = TRUE, showWarnings = FALSE)

if (!file.exists(h1_path)) stop(paste("Missing:", h1_path))

d <- read_csv(h1_path, show_col_types = FALSE)

need <- c("ccode", "year", "FraudIndex", "FB_strength")
miss <- setdiff(need, names(d))
if (length(miss) > 0) stop(paste("Missing required columns:", paste(miss, collapse = ", ")))

d <- d %>%
  mutate(
    ccode = as.integer(ccode),
    year  = as.integer(year)
  ) %>%
  filter(!is.na(ccode), !is.na(year), !is.na(FraudIndex), !is.na(FB_strength)) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    FB_l1 = lag(FB_strength, 1),
    FB_f1 = lead(FB_strength, 1)
  ) %>%
  ungroup()

# ----------------------------
# Models
# ----------------------------

# (A) Baseline TWFE: country + year FE
m_twfe <- feols(FraudIndex ~ FB_strength | ccode + year, data = d, cluster = "ccode")

# (B) Lag model: FB_strength(t-1)
m_lag1 <- feols(FraudIndex ~ FB_l1 | ccode + year, data = d, cluster = "ccode")

# (C) Placebo lead: FB_strength(t+1) should be ~0 if no reverse timing
m_lead1 <- feols(FraudIndex ~ FB_f1 | ccode + year, data = d, cluster = "ccode")

# (D) Nonlinearity: bins of FB_strength (country+year FE)
d_bins <- d %>%
  mutate(FB_bin = ntile(FB_strength, 5) %>% factor())

m_bins <- feols(FraudIndex ~ i(FB_bin, ref = "1") | ccode + year, data = d_bins, cluster = "ccode")

# ----------------------------
# Output table
# ----------------------------
models <- list(
  "TWFE"        = m_twfe,
  "TWFE (lag1)" = m_lag1,
  "TWFE (lead1 placebo)" = m_lead1
)

# modelsummary text output
ms <- modelsummary(
  models,
  output = "data.frame",
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|Adj|Within|FE"
)

capture.output(ms, file = out_table)

# Add binned model separately (it has multiple coefficients)
capture.output("\n\n--- Binned FB_strength (quintiles), TWFE ---\n", file = out_table, append = TRUE)
capture.output(etable(m_bins), file = out_table, append = TRUE)

# ----------------------------
# Figures
# ----------------------------

# Coef plot for the three single-coef models
get_coef <- function(m, term) {
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  row <- ct[ct$term == term, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  est <- row$Estimate
  se  <- row$`Std. Error`
  data.frame(
    model = deparse(substitute(m)),
    term = term,
    estimate = est,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se
  )
}

cp <- bind_rows(
  data.frame(model = "TWFE", term = "FB_strength", estimate = coef(m_twfe)["FB_strength"],
             conf.low = coef(m_twfe)["FB_strength"] - 1.96*se(m_twfe)["FB_strength"],
             conf.high = coef(m_twfe)["FB_strength"] + 1.96*se(m_twfe)["FB_strength"]),
  data.frame(model = "TWFE (lag1)", term = "FB_l1", estimate = coef(m_lag1)["FB_l1"],
             conf.low = coef(m_lag1)["FB_l1"] - 1.96*se(m_lag1)["FB_l1"],
             conf.high = coef(m_lag1)["FB_l1"] + 1.96*se(m_lag1)["FB_l1"]),
  data.frame(model = "TWFE (lead1 placebo)", term = "FB_f1", estimate = coef(m_lead1)["FB_f1"],
             conf.low = coef(m_lead1)["FB_f1"] - 1.96*se(m_lead1)["FB_f1"],
             conf.high = coef(m_lead1)["FB_f1"] + 1.96*se(m_lead1)["FB_f1"])
)

p1 <- ggplot(cp, aes(x = model, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Coefficient (95% CI)",
    title = "H1 Robustness: TWFE, lag, placebo lead"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_fig, p1, width = 9, height = 4.8, dpi = 300)

# Binned plot: average FraudIndex by FB_strength quintile (descriptive)
bin_means <- d_bins %>%
  group_by(FB_bin) %>%
  summarise(
    mean_FraudIndex = mean(FraudIndex, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p2 <- ggplot(bin_means, aes(x = FB_bin, y = mean_FraudIndex)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  labs(
    x = "FB_strength quintile (1=lowest)",
    y = "Mean FraudIndex",
    title = "Nonlinearity check: FraudIndex across FB_strength quintiles"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_fig2, p2, width = 8.2, height = 4.8, dpi = 300)

message("Saved:")
message(paste0("- ", out_table))
message(paste0("- ", out_fig))
message(paste0("- ", out_fig2))

