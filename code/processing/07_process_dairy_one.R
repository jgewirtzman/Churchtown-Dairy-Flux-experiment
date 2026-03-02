# 07_process_dairy_one.R
# Process Dairy One soil chemistry data (one-time baseline measurement)
# Output: data/processed/dairy_one_clean.csv

library(dplyr)

treatment_key <- read.csv("data/processed/treatment_key.csv")

dairy_one <- read.csv("data/raw/dairy_one/soil/DairyOne_soildata.csv",
                      check.names = FALSE)

dairy_one_clean <- dairy_one %>%
  transmute(
    plot = as.integer(trimws(`Field Name`)),
    om_pct = as.numeric(`Organic Matter %`),
    ph = as.numeric(pH),
    cec = as.numeric(CEC),
    buffer_ph = as.numeric(`Buffer pH`),
    nitrate_n_ppm = as.numeric(`Nitrate-N ppm`),
    ca_ppm = as.numeric(`Ca ppm`),
    p_ppm = as.numeric(`P ppm`),
    mg_ppm = as.numeric(`Mg ppm`),
    k_ppm = as.numeric(`K ppm`),
    na_ppm = as.numeric(`Na ppm`),
    fe_ppm = as.numeric(`Fe ppm`),
    zn_ppm = as.numeric(`Zn ppm`),
    al_ppm = as.numeric(`Al ppm`),
    s_ppm = as.numeric(`S ppm`),
    total_n_pct = as.numeric(`% Total Nitrogen`),
    soluble_salts_pct = as.numeric(`% Soluble Salts`)
  ) %>%
  filter(!is.na(plot)) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, everything()) %>%
  arrange(plot)

write.csv(dairy_one_clean, "data/processed/dairy_one_clean.csv", row.names = FALSE)
cat("Wrote data/processed/dairy_one_clean.csv\n")
cat(sprintf("  %d rows (one per plot)\n", nrow(dairy_one_clean)))
