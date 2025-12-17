rm(list = ls())
cat("\014")

# ================================
# 04_h1_results_plots.R
# Make plots for H1 models
# ================================

pkgs <- c("dplyr","readr","ggplot2","broom","broom.mixed","lme4","lmerTest")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Load dataset produced by variable construction ----
h1 <- read_csv("data/derived/vdem_h1_country_year.csv", show_col_types = FALSE)

# Keep only complete DV + IV rows
h1_main <- h1 %>%
  filter(!is.na(FB_strength), !is.na(FraudIndex)) %>%
  mutate(
    year  = factor(year),
    ccode = as.factor(ccode)
  )

cat("Rows in h1:", nrow(h1), "\n")
cat("Rows in h1_main:", nrow(h1_main), "\n")

# ---- Fit the three models again (for plotting) ----
m1 <- lm(FraudIndex ~ FB_strength + year, data = h1_main)
m2 <- lmer(FraudIndex ~ FB_strength + year + (1 | ccode), data = h1_main, REML = FALSE)

h1_mund <- h1_main %>%
  group_by(ccode) %>%
  mutate(
    FB_between = mean(FB_strength, na.rm = TRUE),
    FB_within  = FB_strength - FB_between
  ) %>%
  ungroup()

m3 <- lmer(FraudIndex ~ FB_within + FB_between + year + (1 | ccode),
           data = h1_mund, REML = FALSE)

# ---- Output folder ----
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ================================
# FIGURE 1: Scatter + smooth
# ================================
p1 <- ggplot(h1_main, aes(x = FB_strength, y = FraudIndex)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "H1: EMB Fourth-Branch Strength vs FraudIndex",
    x = "FB_strength (standardized index)",
    y = "FraudIndex (standardized index)"
  )

ggsave("output/figures/H1_scatter_FBstrength_FraudIndex.png",
       plot = p1, width = 8, height = 5, dpi = 300)

# ================================
# FIGURE 2: Coefficient plot (key terms only)
# ================================

tidy_m2 <- broom.mixed::tidy(m2, effects = "fixed", conf.int = TRUE) %>%
  filter(term == "FB_strength") %>%
  mutate(model = "Model 2: Random intercept")

tidy_m3 <- broom.mixed::tidy(m3, effects = "fixed", conf.int = TRUE) %>%
  filter(term %in% c("FB_within","FB_between")) %>%
  mutate(model = "Model 3: Mundlak")

coef_df <- bind_rows(tidy_m2, tidy_m3) %>%
  mutate(term_label = case_when(
    term == "FB_strength" ~ "FB_strength",
    term == "FB_within"   ~ "FB_within (within-country)",
    term == "FB_between"  ~ "FB_between (between-country)",
    TRUE ~ term
  ))

p2 <- ggplot(coef_df, aes(x = estimate, y = term_label)) +
  geom_point() +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.15,
    orientation = "y"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Key H1 coefficients with 95% CIs",
    x = "Estimated effect on FraudIndex",
    y = NULL
  )

ggsave("output/figures/H1_coefficients_keyterms.png",
       plot = p2, width = 8, height = 4.5, dpi = 300)

# ================================
# FIGURE 3: Predicted DV over FB_strength (from m2)
# Hold year at most common year level (first level) for a clean curve
# ================================
grid <- tibble(
  FB_strength = seq(min(h1_main$FB_strength, na.rm = TRUE),
                    max(h1_main$FB_strength, na.rm = TRUE),
                    length.out = 100),
  year = levels(h1_main$year)[1]
)

grid$pred <- predict(m2, newdata = grid, re.form = NA)

p3 <- ggplot(grid, aes(x = FB_strength, y = pred)) +
  geom_line() +
  labs(
    title = "Predicted FraudIndex over FB_strength (Model 2)",
    x = "FB_strength",
    y = "Predicted FraudIndex"
  )

ggsave("output/figures/H1_predicted_curve_m2.png",
       plot = p3, width = 8, height = 5, dpi = 300)

cat("\nSaved figures to output/figures/\n")