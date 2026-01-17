# 02_analysis_modeling.R
# Purpose:
#   1) Load monthly_full panel
#   2) Impute missing monthly PM2.5 per site (Kalman + auto.arima)
#   3) Create policy variables
#   4) Run DID (unit FE + month FE) using fixest
# Outputs:
#   - output/monthly_ready.rds
#   - output/did_baseline.csv
#   - output/did_multistage.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(imputeTS)
  library(fixest)
  library(broom)
  library(readr)
})

# ---- 1) Load data ----
monthly_full <- readRDS("output/monthly_full.rds")

# ---- 2) Impute missing values (per site) ----
# Rationale: keep a single, stable imputation method for a clean, reproducible pipeline.
monthly_filled <- monthly_full %>%
  group_by(code) %>%
  arrange(date) %>%
  mutate(
    pm25_filled = {
      y <- ts(pm25, frequency = 12)
      if (sum(!is.na(y)) > 12) {
        as.numeric(imputeTS::na_kalman(y, model = "auto.arima", smooth = TRUE))
      } else {
        rep(NA_real_, length(y))
      }
    }
  ) %>%
  ungroup()

# ---- 3) Policy variables ----
# ULEZ key dates
bp1 <- as.Date("2019-04-01")  # ULEZ initial
bp2 <- as.Date("2021-10-01")  # Expansion
bp3 <- as.Date("2023-08-29")  # All London

monthly_ready <- monthly_filled %>%
  mutate(
    policy_stage = case_when(
      date < bp1 ~ "Pre-ULEZ",
      date < bp2 ~ "ULEZ-1",
      TRUE       ~ "ULEZ-2"
    ),
    treated = ifelse(tolower(code) == "my1", 1L, 0L),
    post    = ifelse(date >= bp1, 1L, 0L),
    interaction = treated * post
  ) %>%
  group_by(code) %>%
  arrange(date) %>%
  mutate(month_id = row_number()) %>%
  ungroup()

saveRDS(monthly_ready, "output/monthly_ready.rds")

# ---- 4) DID (fixest) ----
panel_did <- monthly_ready %>%
  mutate(
    code_u = toupper(code),
    date   = floor_date(as.Date(date), "month")
  ) %>%
  distinct(code_u, date, .keep_all = TRUE) %>%
  filter(!is.na(pm25_filled)) %>%
  mutate(
    Treated = as.integer(code_u == "MY1"),
    Post19  = as.integer(date >= bp1),
    phase   = case_when(
      date < bp1 ~ "pre",
      date < bp2 ~ "p19_21",
      date < bp3 ~ "p21_23",
      TRUE       ~ "p23p"
    ),
    phase = factor(phase, levels = c("pre", "p19_21", "p21_23", "p23p"))
  )

# Baseline DID: post-2019 vs pre-2019
# Unit FE: code_u; Time FE: factor(date); Cluster: site
m_did_baseline <- feols(
  pm25_filled ~ i(Post19, Treated, ref = 0) | code_u + factor(date),
  cluster = ~ code_u,
  data = panel_did
)

# Multi-stage DID
m_did_multistage <- feols(
  pm25_filled ~ i(phase, Treated, ref = "pre") | code_u + factor(date),
  cluster = ~ code_u,
  data = panel_did
)

# Export tidy outputs (compact; easy to cite in reports)
dir.create("output", showWarnings = FALSE)
write_csv(broom::tidy(m_did_baseline),  "output/did_baseline.csv")
write_csv(broom::tidy(m_did_multistage),"output/did_multistage.csv")

message("Saved: output/monthly_ready.rds")
message("Saved: output/did_baseline.csv")
message("Saved: output/did_multistage.csv")
