# 09_process_dairy_one_forage.R
# Process DairyOne forage (pasture) nutrient analysis
# Output: data/processed/dairy_one_forage.csv

library(dplyr)

treatment_key <- read.csv("data/processed/treatment_key.csv")

forage_raw <- read.csv("data/raw/dairy_one/forage/DairyOne_forage.CSV")

forage <- forage_raw %>%
  transmute(
    plot = as.integer(plot_number),
    treatment = tolower(trimws(treatment)),
    moisture_pct = as.numeric(moisture),
    dry_matter_pct = as.numeric(drymatter),
    crude_protein_pct = as.numeric(crudeprotein),
    avail_protein_pct = as.numeric(availprotein),
    adicp_pct = as.numeric(ADICP),
    adf_pct = as.numeric(ADF),
    andf_pct = as.numeric(aNDF),
    crude_fat_pct = as.numeric(crudefat),
    tdn_pct = as.numeric(TDN),
    ca_pct = as.numeric(calcium),
    p_pct = as.numeric(phosphorus),
    mg_pct = as.numeric(magnesium),
    k_pct = as.numeric(potassium),
    s_pct = as.numeric(sulfur),
    rfv = as.numeric(RFV),
    ash_pct = as.numeric(ash),
    lignin_pct = as.numeric(lignin),
    ndicp_pct = as.numeric(NDICP),
    starch_pct = as.numeric(starch),
    nfc_pct = as.numeric(NFC),
    water_sol_carbs_pct = as.numeric(watersolcarbs),
    simple_sugars_pct = as.numeric(simplesugars)
  ) %>%
  filter(!is.na(plot)) %>%
  arrange(plot)

write.csv(forage, "data/processed/dairy_one_forage.csv", row.names = FALSE)
cat("Wrote data/processed/dairy_one_forage.csv\n")
cat(sprintf("  %d rows (one per plot)\n", nrow(forage)))
