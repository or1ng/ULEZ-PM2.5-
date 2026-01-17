# 03_visualization.R
# Purpose:
#   Generate a small set of publication-ready figures for README / portfolio.
# Outputs:
#   - output/figures/pm25_trend_facet.png
#   - output/figures/did_multistage_effects.png

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(fixest)
  library(broom)
  library(readr)
})

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

monthly_ready <- readRDS("output/monthly_ready.rds")

bp1 <- as.Date("2019-04-01")
bp2 <- as.Date("2021-10-01")
bp3 <- as.Date("2023-08-29")

# ---- Figure 1: Monthly PM2.5 trend (facet by site) ----
p_trend <- ggplot(monthly_ready, aes(x = date, y = pm25_filled)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = c(bp1, bp2, bp3), linetype = "dashed") +
  facet_wrap(~ code, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Monthly PM2.5 trends by monitoring site",
    x = "Date",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_light(base_size = 12)

ggsave("output/figures/pm25_trend_facet.png", p_trend,
       width = 9.5, height = 6.2, dpi = 300, bg = "white")

# ---- Figure 2: DID multi-stage effects (coefficients) ----
did_multi <- read.csv("output/did_multistage.csv")

# Keep treated interaction terms only
coef_df <- did_multi %>%
  dplyr::filter(grepl(":Treated$", term)) %>%   
  dplyr::mutate(
    phase_raw = sub("^phase::", "", sub(":Treated$", "", term)),
    phase = dplyr::case_when(
      phase_raw == "p19_21" ~ "2019–2021",
      phase_raw == "p21_23" ~ "2021–2023",
      phase_raw == "p23p"   ~ "Post-2023",
      TRUE ~ phase_raw
    ),
    conf_low  = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error
  )

p_did <- ggplot2::ggplot(coef_df, ggplot2::aes(x = phase, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high), width = 0.15) +
  ggplot2::labs(
    title = "ULEZ effect estimates by phase (DID)",
    x = NULL,
    y = "Estimated effect on PM2.5 (µg/m³)"
  ) +
  ggplot2::theme_minimal(base_size = 12)


ggsave("output/figures/did_multistage_effects.png", p_did,
       width = 8.2, height = 4.8, dpi = 300, bg = "white")

message("Saved figures in output/figures/")
