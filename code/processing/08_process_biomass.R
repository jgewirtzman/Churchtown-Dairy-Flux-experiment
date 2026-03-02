# 08_process_biomass.R
# Process vegetation biomass data (aboveground clipping)
# Output: data/processed/biomass.csv

library(readxl)
library(dplyr)

treatment_key <- read.csv("data/processed/treatment_key.csv")

biomass_raw <- read_excel("data/raw/vegetation/vegetation biomass.xlsx", sheet = "final")

biomass <- biomass_raw %>%
  transmute(
    plot = as.integer(plot_no),
    replicate = as.character(Replicate),
    sampling_date = as.Date("2025-10-24"),
    wet_mass_g = as.numeric(wet_mass_g),
    dry_mass_g = as.numeric(dry_mass_g),
    dry_matter_pct = as.numeric(drymatt_percent),
    dry_matter_g_m2 = as.numeric(gdrymatt_per_m2)
  ) %>%
  filter(!is.na(plot)) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, everything()) %>%
  arrange(plot)

write.csv(biomass, "data/processed/biomass.csv", row.names = FALSE)
cat("Wrote data/processed/biomass.csv\n")
cat(sprintf("  %d rows (%d plots)\n", nrow(biomass), n_distinct(biomass$plot)))
