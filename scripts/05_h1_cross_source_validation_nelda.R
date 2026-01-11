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
# 0) User settings
# ----------------------------

fraud_cols <- c("nelda11")

h1_path    <- file.path("data", "derived", "vdem_h1_country_year.csv")
nelda_path <- file.path("data", "raw", "NELDA.csv")

derived_out     <- file.path("data", "derived", "h1_with_nelda_validation.csv")
table_diff_out  <- file.path("output", "tables",  "H1_NELDA_validation_diffmeans.txt")
table_model_out <- file.path("output", "tables",  "H1_NELDA_validation_model.txt")
fig_out         <- file.path("output", "figures", "H1_validation_VDem_vs_NELDA_boxjitter.png")

# ----------------------------
# Helpers
# ----------------------------

dir_safe <- function(p) dir.create(p, recursive = TRUE, showWarnings = FALSE)

to_int_safe <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- trimws(as.character(x))
  suppressWarnings(as.integer(x))
}

read_nelda_any <- function(path) {
  x <- suppressMessages(read_csv(
    path,
    col_types = cols(.default = col_character()),
    na = c("", "NA", "N/A"),
    show_col_types = FALSE,
    progress = FALSE
  ))
  if (ncol(x) <= 1) {
    x2 <- suppressMessages(read_tsv(
      path,
      col_types = cols(.default = col_character()),
      na = c("", "NA", "N/A"),
      show_col_types = FALSE,
      progress = FALSE
    ))
    if (ncol(x2) > 1) return(x2)
  }
  x
}

parse_year_any <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  s <- trimws(as.character(x))
  
  # Extract YYYY if embedded in a date-like string
  y_str <- str_extract(s, "(17|18|19|20|21)\\d{2}")
  y1 <- suppressWarnings(as.integer(y_str))
  
  # Or parse numeric (YYYY or YYYYMMDD)
  xn <- suppressWarnings(as.numeric(s))
  y2 <- dplyr::case_when(
    !is.na(xn) & xn > 3000 ~ as.integer(xn %/% 10000), # YYYYMMDD -> YYYY
    !is.na(xn)             ~ as.integer(xn),           # YYYY
    TRUE                   ~ NA_integer_
  )
  
  ifelse(!is.na(y1), y1, y2)
}

as_01 <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("1", "true", "t", "yes", "y") ~ 1L,
    x %in% c("0", "false", "f", "no",  "n") ~ 0L,
    x %in% c("unclear", "u", "", "na", "n/a") ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

resolve_cols <- function(df_names, wanted) {
  dn <- tolower(df_names)
  out <- character(0)
  
  for (w in tolower(wanted)) {
    if (w %in% dn) {
      out <- c(out, df_names[match(w, dn)])
    } else {
      hits <- df_names[str_detect(tolower(df_names), paste0("^", fixed(w)))]
      if (length(hits) == 1) out <- c(out, hits)
      if (length(hits) > 1) stop(sprintf("Multiple columns match '%s': %s", w, paste(hits, collapse = ", ")))
      if (length(hits) == 0) {
        possible <- df_names[str_detect(tolower(df_names), "^nelda\\d+")]
        message("\nPossible NELDA nelda# columns (first 120):")
        print(head(possible, 120))
        stop(sprintf("Could not find fraud column '%s' in NELDA.", w))
      }
    }
  }
  out
}

# Collapse multiple rows per (ccode,year) into ONE nelda_fraud value:
# 1 if any election in that country-year has fraud=1; else 0; else NA if all missing.
collapse_binary_by_key <- function(df, key = c("ccode", "year"), var = "nelda_fraud") {
  df %>%
    group_by(across(all_of(key))) %>%
    summarise(
      nelda_fraud = dplyr::case_when(
        all(is.na(.data[[var]])) ~ NA_integer_,
        any(.data[[var]] == 1L, na.rm = TRUE) ~ 1L,
        TRUE ~ 0L
      ),
      .groups = "drop"
    )
}

merge_with_shifts <- function(left, right, by_keys, shifts = c(0L, -1L, 1L)) {
  best <- NULL
  best_n <- -1
  best_shift <- NA_integer_
  
  for (s in shifts) {
    r2 <- right
    if ("year" %in% names(r2) && "year" %in% by_keys) {
      r2 <- r2 %>% mutate(year = year + s)
    }
    m <- left %>% left_join(r2, by = by_keys)
    n <- sum(!is.na(m$nelda_fraud))
    if (n > best_n) {
      best <- m
      best_n <- n
      best_shift <- s
    }
  }
  
  list(merged = best, matched = best_n, shift = best_shift)
}

# ----------------------------
# 1) Load H1 from derived file
# ----------------------------

if (!file.exists(h1_path)) stop(sprintf("Missing %s", h1_path))
h1 <- read_csv(h1_path, show_col_types = FALSE)

req <- c("ccode", "year", "FraudIndex")
miss <- setdiff(req, names(h1))
if (length(miss) > 0) stop(sprintf("H1 missing required columns: %s", paste(miss, collapse = ", ")))

h1 <- h1 %>%
  mutate(
    ccode = to_int_safe(ccode),
    year  = to_int_safe(year)
  ) %>%
  filter(!is.na(ccode), !is.na(year), !is.na(FraudIndex))

# Force one row per ccode-year (defensive)
h1 <- h1 %>%
  group_by(ccode, year) %>%
  summarise(FraudIndex = mean(FraudIndex), .groups = "drop")

message(sprintf("H1 rows: %d", nrow(h1)))
message(sprintf("H1 year range: %s to %s", min(h1$year, na.rm = TRUE), max(h1$year, na.rm = TRUE)))

# ----------------------------
# 2) Load NELDA
# ----------------------------

if (!file.exists(nelda_path)) stop(sprintf("NELDA file not found at: %s", nelda_path))

nelda_raw <- read_nelda_any(nelda_path)
names(nelda_raw) <- tolower(names(nelda_raw))

if (!("ccode" %in% names(nelda_raw))) stop("NELDA must contain a `ccode` column.")
if (!("year"  %in% names(nelda_raw))) stop("NELDA must contain a `year` column.")

nelda <- nelda_raw %>%
  mutate(
    ccode = to_int_safe(ccode),
    year  = parse_year_any(year)
  ) %>%
  filter(!is.na(ccode), !is.na(year))

message(sprintf("\nNELDA rows: %d  cols: %d", nrow(nelda), ncol(nelda)))
message(sprintf("NELDA year range: %s to %s", min(nelda$year, na.rm = TRUE), max(nelda$year, na.rm = TRUE)))

# ----------------------------
# 3) Build NELDA fraud indicator (and FORCE uniqueness per ccode-year)
# ----------------------------

fraud_cols_real <- resolve_cols(names(nelda), fraud_cols)

nelda_tmp <- nelda %>%
  mutate(across(all_of(fraud_cols_real), as_01)) %>%
  mutate(
    nelda_fraud = case_when(
      if_all(all_of(fraud_cols_real), ~ is.na(.x)) ~ NA_integer_,
      if_any(all_of(fraud_cols_real), ~ .x == 1L) ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  select(ccode, year, nelda_fraud)

# KEY FIX: collapse multiple elections per country-year to a single country-year indicator
nelda_fraud_df <- collapse_binary_by_key(nelda_tmp, key = c("ccode", "year"), var = "nelda_fraud")

message(sprintf("NELDA fraud non-NA: %d", sum(!is.na(nelda_fraud_df$nelda_fraud))))

# ----------------------------
# 4) Merge (with year shifts)
# ----------------------------

m <- merge_with_shifts(
  left = h1,
  right = nelda_fraud_df,
  by_keys = c("ccode", "year"),
  shifts = c(0L, -1L, 1L)
)

merged <- m$merged
matched_n <- m$matched

message(sprintf("\nMerge attempts: by ccode+year with shifts 0/-1/+1"))
message(sprintf("Best year shift: %d", m$shift))
message(sprintf("Merged rows: %d", nrow(merged)))
message(sprintf("NELDA matched (non-NA nelda_fraud): %d", matched_n))

# Confirm duplicates after merge (should be 0)
dup_after <- merged %>%
  count(ccode, year, name = "n") %>%
  filter(n > 1)

if (nrow(dup_after) > 0) {
  message("\nWARNING: Duplicate ccode-year rows remain after merge (showing first 20):")
  print(head(dup_after, 20))
} else {
  message("Duplicate ccode-year rows after merge: 0")
}

if (matched_n == 0) {
  message("\nDIAGNOSTICS:")
  message(sprintf("Unique ccode overlap: %d",
                  length(intersect(sort(unique(h1$ccode)), sort(unique(nelda$ccode))))))
  message(sprintf("Unique year overlap: %d",
                  length(intersect(sort(unique(h1$year)), sort(unique(nelda$year))))))
  message("H1 sample years:"); print(head(sort(unique(h1$year)), 25))
  message("NELDA sample years:"); print(head(sort(unique(nelda$year)), 25))
  stop("STOP: No NELDA matches found even after trying year shifts. This indicates a key mismatch (ccode/year).")
}

dir_safe(dirname(derived_out))
write_csv(merged, derived_out)

# ----------------------------
# 5) Diff-in-means
# ----------------------------

diff_tbl <- merged %>%
  filter(!is.na(nelda_fraud), !is.na(FraudIndex)) %>%
  group_by(nelda_fraud) %>%
  summarise(
    n = n(),
    mean_FraudIndex = mean(FraudIndex),
    sd_FraudIndex   = sd(FraudIndex),
    .groups = "drop"
  )

dir_safe(dirname(table_diff_out))
capture.output(diff_tbl, file = table_diff_out)

message("\nDifference in FraudIndex by NELDA fraud indicator:")
print(diff_tbl)

# ----------------------------
# 6) Regression + Figure (only if both 0 and 1 exist)
# ----------------------------

levels_present <- sort(unique(na.omit(merged$nelda_fraud)))
if (length(levels_present) < 2) {
  msg <- sprintf("NOTE: Only one nelda_fraud level present (%s). Skipping regression + figure.",
                 paste(levels_present, collapse = ","))
  message(msg)
  dir_safe(dirname(table_model_out))
  writeLines(msg, con = table_model_out)
  
} else {
  
  val_dat <- merged %>%
    filter(!is.na(nelda_fraud), !is.na(FraudIndex), !is.na(year), !is.na(ccode))
  
  val_mod <- lm(FraudIndex ~ nelda_fraud + factor(year), data = val_dat)
  V_cr2 <- vcovCR(val_mod, cluster = val_dat$ccode, type = "CR2")
  ct <- coef_test(val_mod, vcov = V_cr2, test = "Satterthwaite")
  
  dir_safe(dirname(table_model_out))
  capture.output(ct, file = table_model_out)
  
  message("\nValidation model (FraudIndex ~ NELDA fraud + year FE) [CR2 clustered by ccode]:")
  print(ct)
  
  plot_dat <- val_dat %>%
    mutate(nelda_fraud = factor(nelda_fraud, levels = c(0, 1),
                                labels = c("NELDA: No fraud", "NELDA: Fraud")))
  
  p <- ggplot(plot_dat, aes(x = nelda_fraud, y = FraudIndex)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.18, alpha = 0.25, size = 0.9) +
    labs(
      x = NULL,
      y = "V-Dem FraudIndex",
      title = "Cross-source validation: V-Dem FraudIndex vs NELDA fraud indicator"
    ) +
    theme_minimal(base_size = 12)
  
  dir_safe(dirname(fig_out))
  ggsave(fig_out, p, width = 9, height = 5, dpi = 300)
}

message("\nSaved:")
message(paste0("- ", derived_out))
message(paste0("- ", table_diff_out))
message(paste0("- ", table_model_out))
message(paste0("- ", fig_out))