# 01_data_cleaning.R
# Purpose:
#   1) Download AURN PM2.5 monitoring data
#   2) Aggregate to monthly means
#   3) Build a complete site x month panel (2016-01 to 2024-12)
# Output:
#   - output/monthly_full.rds

suppressPackageStartupMessages({
  library(openair)
  library(dplyr)
  library(lubridate)
  library(tidyr)
})

# ---- Settings ----
site_codes <- c("my1", "kc1", "bex", "lon6")
years <- 2016:2024

# ---- 1) Download raw (hourly) PM2.5 ----
# Note: importAURN downloads public data from the UK AURN network.
# If download fails, re-run or try smaller time windows.
data_raw <- importAURN(site = site_codes, year = years, pollutant = "pm2.5")

# ---- 2) Aggregate to monthly mean PM2.5 ----
monthly_data <- data_raw %>%
  mutate(
    year  = year(date),
    month = month(date)
  ) %>%
  group_by(code, year, month) %>%
  summarise(
    pm25 = mean(pm2.5, na.rm = TRUE),
    n    = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  select(code, date, year, month, pm25, n)

# ---- 3) Build a complete site-month panel ----
all_months <- expand.grid(
  code = unique(monthly_data$code),
  date = seq.Date(from = as.Date("2016-01-01"), to = as.Date("2024-12-01"), by = "month")
)

monthly_full <- all_months %>%
  left_join(monthly_data, by = c("code", "date")) %>%
  mutate(
    year  = year(date),
    month = month(date)
  ) %>%
  arrange(code, date)

# ---- 4) Save intermediate dataset ----
dir.create("output", showWarnings = FALSE)
saveRDS(monthly_full, "output/monthly_full.rds")

message("Saved: output/monthly_full.rds")
