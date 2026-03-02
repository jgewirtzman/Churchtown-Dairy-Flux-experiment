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

application_date <- as.Date("2025-05-28")

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
  geom_vline(xintercept = application_date, linetype = "dashed", color = "gray40") +
  annotate("text", x = application_date, y = Inf, label = "Manure applied",
           vjust = 1.5, hjust = -0.05, size = 3, color = "gray30") +
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
  geom_vline(xintercept = application_date, linetype = "dashed", color = "gray40") +
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
  geom_vline(xintercept = application_date, linetype = "dashed", color = "gray40") +
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
# 5. Vegetation biomass by treatment
# =============================================================================

biomass <- read.csv("data/processed/biomass.csv")

biomass_summary <- biomass %>%
  group_by(treatment) %>%
  summarize(mean = mean(dry_matter_g_m2, na.rm = TRUE),
            se = sd(dry_matter_g_m2, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_biomass <- ggplot(biomass_summary, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_jitter(data = biomass, aes(x = treatment, y = dry_matter_g_m2),
              width = 0.1, size = 2, alpha = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = treat_colors) +
  labs(x = "Treatment", y = expression(paste("Dry matter (g m"^{-2}, ")")),
       title = "Aboveground Vegetation Biomass (Oct 2025)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("output/figures/summary_biomass.png", p_biomass, width = 6, height = 5, dpi = 150)
cat("Saved output/figures/summary_biomass.png\n")


# =============================================================================
# 6. Forage quality PCA by treatment
# =============================================================================

forage <- read.csv("data/processed/dairy_one_forage.csv")

# Select numeric nutrient columns for PCA (drop moisture/dry_matter — they're complements)
pca_vars <- forage %>%
  select(crude_protein_pct, avail_protein_pct, adicp_pct, adf_pct, andf_pct,
         crude_fat_pct, tdn_pct, ca_pct, p_pct, mg_pct, k_pct, s_pct,
         rfv, ash_pct, lignin_pct, ndicp_pct, starch_pct, nfc_pct,
         water_sol_carbs_pct, simple_sugars_pct)

pca_fit <- prcomp(pca_vars, center = TRUE, scale. = TRUE)

pca_scores <- as.data.frame(pca_fit$x[, 1:2])
pca_scores$treatment <- forage$treatment
pca_scores$plot <- forage$plot

pca_loadings <- as.data.frame(pca_fit$rotation[, 1:2])
pca_loadings$variable <- rownames(pca_loadings)

# Variance explained
var_pct <- round(100 * pca_fit$sdev^2 / sum(pca_fit$sdev^2), 1)

# Scale loadings for biplot arrows
arrow_scale <- max(abs(pca_scores$PC1), abs(pca_scores$PC2)) * 0.8 /
  max(abs(pca_loadings$PC1), abs(pca_loadings$PC2))
pca_loadings$PC1s <- pca_loadings$PC1 * arrow_scale
pca_loadings$PC2s <- pca_loadings$PC2 * arrow_scale

# Clean variable labels
pca_loadings$label <- gsub("_pct$", "", pca_loadings$variable)

p_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = treatment)) +
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1s, yend = PC2s),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "gray50", linewidth = 0.4, inherit.aes = FALSE) +
  geom_text(data = pca_loadings,
            aes(x = PC1s * 1.08, y = PC2s * 1.08, label = label),
            color = "gray30", size = 2.5, inherit.aes = FALSE) +
  geom_point(size = 3) +
  geom_text(aes(label = plot), size = 2.5, vjust = -0.8) +
  scale_color_manual(values = treat_colors) +
  labs(x = paste0("PC1 (", var_pct[1], "%)"),
       y = paste0("PC2 (", var_pct[2], "%)"),
       color = "Treatment",
       title = "Forage Quality PCA") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  coord_fixed()

ggsave("output/figures/summary_forage_pca.png", p_pca, width = 8, height = 7, dpi = 150)
cat("Saved output/figures/summary_forage_pca.png\n")


# =============================================================================
# 7. Forage quality z-score heatmap
# =============================================================================

library(pheatmap)

# Z-score the nutrient matrix (rows = variables, columns = plots)
zmat <- scale(pca_vars)
rownames(zmat) <- paste0(forage$treatment, " (", forage$plot, ")")
colnames(zmat) <- gsub("_pct$", "", colnames(zmat))

# Transpose so variables are rows, plots are columns
zmat_t <- t(zmat)

# Annotation bar for treatment
col_anno <- data.frame(Treatment = forage$treatment, row.names = rownames(zmat))
anno_colors <- list(Treatment = treat_colors)

png("output/figures/summary_forage_heatmap.png", width = 9, height = 8, units = "in", res = 150)
pheatmap(zmat_t,
         annotation_col = col_anno,
         annotation_colors = anno_colors,
         clustering_method = "ward.D2",
         color = colorRampPalette(c("#2166ac", "white", "#b2182b"))(100),
         breaks = seq(-3, 3, length.out = 101),
         main = "Forage Quality (z-scores)",
         fontsize = 10,
         fontsize_row = 9,
         fontsize_col = 9,
         angle_col = 45)
dev.off()
cat("Saved output/figures/summary_forage_heatmap.png\n")


# =============================================================================
# 8. Print summary statistics
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

cat("\n--- Vegetation Biomass ---\n")
biomass %>%
  group_by(treatment) %>%
  summarize(n = n(),
            mean_g_m2 = round(mean(dry_matter_g_m2, na.rm=TRUE), 0),
            sd_g_m2 = round(sd(dry_matter_g_m2, na.rm=TRUE), 0),
            .groups = "drop") %>%
  print()
