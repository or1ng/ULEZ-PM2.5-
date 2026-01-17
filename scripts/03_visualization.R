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
p_trend <- ggplot2::ggplot(monthly_ready, ggplot2::aes(x = date, y = pm25_filled)) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::facet_wrap(~ code, ncol = 2) +
  ggplot2::geom_vline(xintercept = as.Date("2019-04-01"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = as.Date("2023-08-29"), linetype = "dashed") +
  ggplot2::labs(
    title = "Monthly PM2.5 trends by monitoring site",
    subtitle = "Dashed lines indicate ULEZ implementation/expansion dates (2019.04, 2021.10, 2023.08)",
    x = "Date",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

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

coef_df$phase <- factor(coef_df$phase, levels = c("2019–2021", "2021–2023", "Post-2023"))

p_did <- ggplot2::ggplot(coef_df, ggplot2::aes(x = phase, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high), width = 0.12) +
  ggplot2::labs(
    title = "Estimated ULEZ policy effect on PM2.5 by phase (DID)",
    subtitle = "Points are coefficient estimates; bars are 95% confidence intervals",
    x = NULL,
    y = "Estimated change in PM2.5 (µg/m³)\n(negative values indicate reductions)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )


ggsave("output/figures/did_multistage_effects.png", p_did,
       width = 8.2, height = 4.8, dpi = 300, bg = "white")

message("Saved figures in output/figures/")

