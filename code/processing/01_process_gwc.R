# 01_process_gwc.R
# Process gravimetric water content data from 3 timepoints
# Output: data/processed/gwc.csv

library(readxl)
library(dplyr)
library(tidyr)

treatment_key <- read.csv("data/processed/treatment_key.csv")

# --- GWC_1: 15 rows (1 replicate per plot, replicate A only) ----------------
gwc1_raw <- read_excel("data/raw/soil/gwc/GWC_1.xlsx", sheet = "Sheet1")

# Column names have special chars: "Fresh_Mass_Soil+Tin _g", "Dry_Mass_Soil+Tin _g"
gwc1 <- gwc1_raw %>%
  transmute(
    plot = as.integer(Plot),
    replicate = as.character(Replicate),
    # readxl reads cached formula values; compute from raw masses as fallback
    gwc = as.numeric(GWC)
  )

# If GWC formula values weren't cached, compute from masses
if (any(is.na(gwc1$gwc))) {
  message("GWC_1: Some GWC values are NA — computing from mass data")
  fresh_col <- names(gwc1_raw)[grepl("Fresh_Mass", names(gwc1_raw))]
  dry_col   <- names(gwc1_raw)[grepl("Dry_Mass", names(gwc1_raw))]
  gwc1 <- gwc1_raw %>%
    transmute(
      plot = as.integer(Plot),
      replicate = as.character(Replicate),
      mass_tin = as.numeric(Mass_Tin_g),
      fresh_soil_tin = as.numeric(.data[[fresh_col]]),
      dry_soil_tin = as.numeric(.data[[dry_col]]),
      gwc = (fresh_soil_tin - dry_soil_tin) / (dry_soil_tin - mass_tin)
    ) %>%
    select(plot, replicate, gwc)
}

gwc1 <- gwc1 %>% mutate(timepoint = 1L)

# --- GWC_2: 30 rows (2 replicates per plot) ---------------------------------
gwc2_raw <- read_excel("data/raw/soil/gwc/GWC_2.xlsx", sheet = "Sheet1")

gwc2 <- gwc2_raw %>%
  transmute(
    plot = as.integer(Plot),
    # Replicates are numeric (1,2) — convert to (A,B)
    replicate = ifelse(as.integer(Replicate) == 1, "A", "B"),
    gwc = as.numeric(GWC)
  )

# Fallback: compute from masses if GWC is NA
if (any(is.na(gwc2$gwc))) {
  message("GWC_2: Some GWC values are NA — computing from mass data")
  dry_col <- names(gwc2_raw)[grepl("Dry_Mass", names(gwc2_raw))]
  gwc2 <- gwc2_raw %>%
    transmute(
      plot = as.integer(Plot),
      replicate = ifelse(as.integer(Replicate) == 1, "A", "B"),
      mass_tin = as.numeric(Mass_Tin_g),
      fresh_soil = as.numeric(Fresh_Mass_Soil_g),
      dry_soil_tin = as.numeric(.data[[dry_col]]),
      dry_soil = dry_soil_tin - mass_tin,
      gwc = (fresh_soil - dry_soil) / dry_soil
    ) %>%
    select(plot, replicate, gwc)
}

gwc2 <- gwc2 %>% mutate(timepoint = 2L)

# --- GWC_3: 30 rows (2 replicates per plot) ---------------------------------
# Note: Some GWC_3 values may be stored as text — coerce to numeric
gwc3_raw <- read_excel("data/raw/soil/gwc/GWC_3.xlsx", sheet = "Sheet1")

gwc3 <- gwc3_raw %>%
  transmute(
    plot = as.integer(Plot),
    replicate = ifelse(as.integer(Replicate) == 1, "A", "B"),
    gwc = suppressWarnings(as.numeric(GWC))
  )

# Fallback: compute from masses if GWC is NA
if (any(is.na(gwc3$gwc))) {
  message("GWC_3: Some GWC values are NA — computing from mass data")
  dry_col <- names(gwc3_raw)[grepl("Dry_Mass", names(gwc3_raw))]
  gwc3 <- gwc3_raw %>%
    transmute(
      plot = as.integer(Plot),
      replicate = ifelse(as.integer(Replicate) == 1, "A", "B"),
      mass_tin = as.numeric(Mass_Tin_g),
      fresh_soil = suppressWarnings(as.numeric(Fresh_Mass_Soil_g)),
      dry_soil_tin = as.numeric(.data[[dry_col]]),
      dry_soil = dry_soil_tin - mass_tin,
      gwc = (fresh_soil - dry_soil) / dry_soil
    ) %>%
    select(plot, replicate, gwc)
}

gwc3 <- gwc3 %>% mutate(timepoint = 3L)

# --- Combine all timepoints --------------------------------------------------
gwc_all <- bind_rows(gwc1, gwc2, gwc3) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, timepoint, replicate, gwc) %>%
  arrange(timepoint, plot, replicate)

# Flag any anomalous values
if (any(gwc_all$gwc < 0, na.rm = TRUE)) {
  warning("Negative GWC values detected — flagging but not removing")
}
if (any(gwc_all$gwc > 1, na.rm = TRUE)) {
  warning("GWC values > 1 detected — check if units are fraction vs percent")
}

write.csv(gwc_all, "data/processed/gwc.csv", row.names = FALSE)
cat("Wrote data/processed/gwc.csv\n")
cat(sprintf("  Timepoint 1: %d rows\n", sum(gwc_all$timepoint == 1)))
cat(sprintf("  Timepoint 2: %d rows\n", sum(gwc_all$timepoint == 2)))
cat(sprintf("  Timepoint 3: %d rows\n", sum(gwc_all$timepoint == 3)))
