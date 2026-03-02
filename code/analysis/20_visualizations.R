# 20_visualizations.R
# Summary visualizations for Churchtown Dairy data
# Output: output/figures/summary_*.png

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Treatment color palette
treat_colors <- c("control" = "#377eb8", "compost" = "#4daf4a", "slurry" = "#e41a1c")

# =============================================================================
# 1. Lab assays: GWC, pH, SIR, C-min by treatment × timepoint
# =============================================================================

gwc <- read.csv("data/processed/gwc.csv")
ph  <- read.csv("data/processed/ph.csv")
sir <- read.csv("data/processed/sir.csv")
cmin_cum <- read.csv("data/processed/cmin_cumulative.csv")
cmin_tr  <- read.csv("data/processed/cmin_timeresolved.csv")

# Panel A: GWC
gwc_summary <- gwc %>%
  group_by(treatment, timepoint) %>%
  summarize(mean = mean(gwc, na.rm = TRUE),
            se = sd(gwc, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_gwc <- ggplot(gwc_summary, aes(x = factor(timepoint), y = mean, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Timepoint", y = "Gravimetric Water Content (g/g)", fill = "Treatment",
       title = "Gravimetric Water Content") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Panel B: pH
ph_summary <- ph %>%
  group_by(treatment, timepoint) %>%
  summarize(mean = mean(ph, na.rm = TRUE),
            se = sd(ph, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_ph <- ggplot(ph_summary, aes(x = factor(timepoint), y = mean, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Timepoint", y = "pH", fill = "Treatment",
       title = "Soil pH") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Panel C: SIR
sir_summary <- sir %>%
  filter(is.na(flag)) %>%
  group_by(treatment, timepoint, method) %>%
  summarize(mean = mean(sir_ug_co2c_hr_g, na.rm = TRUE),
            se = sd(sir_ug_co2c_hr_g, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_sir <- ggplot(sir_summary, aes(x = factor(timepoint), y = mean, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Timepoint", y = expression(paste("SIR (", mu, "g CO"[2], "-C hr"^{-1}, " g"^{-1}, ")")),
       fill = "Treatment", title = "Substrate-Induced Respiration") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Panel D: Cumulative C mineralization
cmin_summary <- cmin_cum %>%
  group_by(treatment, timepoint, method) %>%
  summarize(mean = mean(cumulative_ug_co2c_g, na.rm = TRUE),
            se = sd(cumulative_ug_co2c_g, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_cmin <- ggplot(cmin_summary, aes(x = factor(timepoint), y = mean, fill = treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Timepoint", y = expression(paste("Cumulative C-min (", mu, "g CO"[2], "-C g"^{-1}, ")")),
       fill = "Treatment", title = "Cumulative Carbon Mineralization") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Save 4-panel lab assays figure
png("output/figures/summary_lab_assays.png", width = 10, height = 8, units = "in", res = 150)
gridExtra::grid.arrange(p_gwc, p_ph, p_sir, p_cmin, ncol = 2)
dev.off()
cat("Saved output/figures/summary_lab_assays.png\n")


# =============================================================================
# 2. C-min time courses by treatment for each incubation
# =============================================================================

cmin_tc <- cmin_tr %>%
  filter(!is.na(cmin_rate_ug_co2c_hr_g), is.na(flag)) %>%
  group_by(treatment, timepoint, day) %>%
  summarize(mean = mean(cmin_rate_ug_co2c_hr_g, na.rm = TRUE),
            se = sd(cmin_rate_ug_co2c_hr_g, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_tc <- ggplot(cmin_tc, aes(x = day, y = mean, color = treatment, group = treatment)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.5) +
  facet_wrap(~ paste("Timepoint", timepoint), scales = "free") +
  scale_color_manual(values = treat_colors) +
  labs(x = "Day of Incubation", y = expression(paste("C-min rate (", mu, "g CO"[2], "-C hr"^{-1}, " g"^{-1}, ")")),
       color = "Treatment", title = "C Mineralization Time Courses") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/figures/summary_cmin_timecourse.png", p_tc, width = 12, height = 5, dpi = 150)
cat("Saved output/figures/summary_cmin_timecourse.png\n")


# =============================================================================
# 3. Field CO2 flux time series by treatment
# =============================================================================

flux <- read.csv("data/processed/flux_estimates.csv") %>%
  mutate(date = as.Date(date))

flux_summary <- flux %>%
  group_by(treatment, date) %>%
  summarize(mean_co2 = mean(FCO2_DRY, na.rm = TRUE),
            se_co2 = sd(FCO2_DRY, na.rm = TRUE) / sqrt(n()),
            mean_ch4 = mean(FCH4_DRY, na.rm = TRUE),
            se_ch4 = sd(FCH4_DRY, na.rm = TRUE) / sqrt(n()),
            mean_n2o = mean(FN2O, na.rm = TRUE),
            se_n2o = sd(FN2O, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_co2 <- ggplot(flux_summary, aes(x = date, y = mean_co2, color = treatment)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = mean_co2 - se_co2, ymax = mean_co2 + se_co2, fill = treatment),
              alpha = 0.15, color = NA) +
  scale_color_manual(values = treat_colors) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Date", y = expression(paste("F"[CO2], " (", mu, "mol m"^{-2}, " s"^{-1}, ")")),
       color = "Treatment", fill = "Treatment",
       title = expression(paste("Soil CO"[2], " Flux"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_ch4 <- ggplot(flux_summary, aes(x = date, y = mean_ch4, color = treatment)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = mean_ch4 - se_ch4, ymax = mean_ch4 + se_ch4, fill = treatment),
              alpha = 0.15, color = NA) +
  scale_color_manual(values = treat_colors) +
  scale_fill_manual(values = treat_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Date", y = expression(paste("F"[CH4], " (nmol m"^{-2}, " s"^{-1}, ")")),
       color = "Treatment", fill = "Treatment",
       title = expression(paste("Soil CH"[4], " Flux"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_n2o <- ggplot(flux_summary, aes(x = date, y = mean_n2o, color = treatment)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = mean_n2o - se_n2o, ymax = mean_n2o + se_n2o, fill = treatment),
              alpha = 0.15, color = NA) +
  scale_color_manual(values = treat_colors) +
  scale_fill_manual(values = treat_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Date", y = expression(paste("F"[N2O], " (nmol m"^{-2}, " s"^{-1}, ")")),
       color = "Treatment", fill = "Treatment",
       title = expression(paste("Soil N"[2], "O Flux"))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

png("output/figures/summary_ghg_fluxes.png", width = 12, height = 10, units = "in", res = 150)
gridExtra::grid.arrange(p_co2, p_ch4, p_n2o, ncol = 1)
dev.off()
cat("Saved output/figures/summary_ghg_fluxes.png\n")


# =============================================================================
# 4. Field metadata: VWC and soil temp over time
# =============================================================================

meta <- read.csv("data/processed/field_metadata.csv") %>%
  mutate(date = as.Date(date))

meta_summary <- meta %>%
  group_by(treatment, date) %>%
  summarize(mean_vwc = mean(mean_vwc, na.rm = TRUE),
            se_vwc = sd(mean_vwc, na.rm = TRUE) / sqrt(n()),
            mean_temp = mean(soil_temp_c, na.rm = TRUE),
            se_temp = sd(soil_temp_c, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_vwc <- ggplot(meta_summary, aes(x = date, y = mean_vwc, color = treatment)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = treat_colors) +
  labs(x = "Date", y = "VWC (%)", color = "Treatment",
       title = "Volumetric Water Content") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_temp <- ggplot(meta_summary, aes(x = date, y = mean_temp, color = treatment)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = treat_colors) +
  labs(x = "Date", y = "Soil Temperature (C)", color = "Treatment",
       title = "Soil Temperature") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

png("output/figures/summary_field_conditions.png", width = 10, height = 6, units = "in", res = 150)
gridExtra::grid.arrange(p_vwc, p_temp, ncol = 2)
dev.off()
cat("Saved output/figures/summary_field_conditions.png\n")


# =============================================================================
# 5. Print summary statistics
# =============================================================================
cat("\n=== SUMMARY STATISTICS ===\n\n")

cat("--- GWC ---\n")
gwc %>%
  group_by(treatment, timepoint) %>%
  summarize(n = n(), mean = round(mean(gwc, na.rm=TRUE), 3),
            sd = round(sd(gwc, na.rm=TRUE), 3), .groups = "drop") %>%
  print()

cat("\n--- pH ---\n")
ph %>%
  group_by(treatment, timepoint) %>%
  summarize(n = n(), mean = round(mean(ph, na.rm=TRUE), 2),
            sd = round(sd(ph, na.rm=TRUE), 2), .groups = "drop") %>%
  print()

cat("\n--- SIR (unflagged only) ---\n")
sir %>%
  filter(is.na(flag)) %>%
  group_by(treatment, timepoint, method) %>%
  summarize(n = n(), mean = round(mean(sir_ug_co2c_hr_g, na.rm=TRUE), 2),
            sd = round(sd(sir_ug_co2c_hr_g, na.rm=TRUE), 2), .groups = "drop") %>%
  print()

cat("\n--- Cumulative C-min ---\n")
cmin_cum %>%
  group_by(treatment, timepoint, method) %>%
  summarize(n = n(), mean = round(mean(cumulative_ug_co2c_g, na.rm=TRUE), 0),
            sd = round(sd(cumulative_ug_co2c_g, na.rm=TRUE), 0), .groups = "drop") %>%
  print()

cat("\n--- CO2 Flux (by treatment, overall) ---\n")
flux %>%
  group_by(treatment) %>%
  summarize(n_obs = n(),
            mean_co2 = round(mean(FCO2_DRY, na.rm=TRUE), 2),
            sd_co2 = round(sd(FCO2_DRY, na.rm=TRUE), 2),
            .groups = "drop") %>%
  print()
