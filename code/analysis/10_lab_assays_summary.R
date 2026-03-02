# 10_lab_assays_summary.R
# Create plot × timepoint summary table with replicate-averaged lab assays
# Output: data/processed/lab_assays_summary.csv

library(dplyr)
library(tidyr)

# --- Load processed data -----------------------------------------------------
treatment_key <- read.csv("data/processed/treatment_key.csv")
gwc <- read.csv("data/processed/gwc.csv")
ph <- read.csv("data/processed/ph.csv")
sir <- read.csv("data/processed/sir.csv")
cmin_cum <- read.csv("data/processed/cmin_cumulative.csv")

# =============================================================================
# Lab assays summary: plot × timepoint (GWC, pH, SIR, cumulative C-min)
#   Replicates averaged within plot × timepoint
# =============================================================================

gwc_avg <- gwc %>%
  group_by(plot, treatment, timepoint) %>%
  summarize(gwc = mean(gwc, na.rm = TRUE), .groups = "drop")

ph_avg <- ph %>%
  group_by(plot, treatment, timepoint) %>%
  summarize(ph = mean(ph, na.rm = TRUE), .groups = "drop")

# SIR: average replicates; if both LGR and IRGA exist for a timepoint, keep IRGA
sir_avg <- sir %>%
  group_by(plot, treatment, timepoint, method) %>%
  summarize(sir_ug_co2c_hr_g = mean(sir_ug_co2c_hr_g, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(plot, timepoint) %>%
  arrange(desc(method == "IRGA")) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(plot, timepoint, sir_ug_co2c_hr_g, sir_method = method)

# Cumulative C-min: average replicates; prefer IRGA if both exist
cmin_avg <- cmin_cum %>%
  group_by(plot, treatment, timepoint, method) %>%
  summarize(cumulative_ug_co2c_g = mean(cumulative_ug_co2c_g, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(plot, timepoint) %>%
  arrange(desc(method == "IRGA")) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(plot, timepoint, cumulative_ug_co2c_g, cmin_method = method)

lab_assays <- expand_grid(plot = 1:15, timepoint = 1:3) %>%
  left_join(treatment_key, by = "plot") %>%
  left_join(gwc_avg,  by = c("plot", "treatment", "timepoint")) %>%
  left_join(ph_avg,   by = c("plot", "treatment", "timepoint")) %>%
  left_join(sir_avg,  by = c("plot", "timepoint")) %>%
  left_join(cmin_avg, by = c("plot", "timepoint")) %>%
  arrange(timepoint, plot)

write.csv(lab_assays, "data/processed/lab_assays_summary.csv", row.names = FALSE)
cat("Wrote data/processed/lab_assays_summary.csv\n")
cat(sprintf("  %d rows (15 plots x 3 timepoints)\n", nrow(lab_assays)))
