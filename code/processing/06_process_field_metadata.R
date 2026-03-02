# 06_process_field_metadata.R
# Process in situ field metadata (VWC, soil temperature)
# Input: data/raw/field_metadata/In situ fluxes meta-data.xlsx
# Output: data/processed/field_metadata.csv

library(readxl)
library(dplyr)
library(lubridate)

treatment_key <- read.csv("data/processed/treatment_key.csv")

meta_raw <- read_excel("data/raw/field_metadata/In situ fluxes meta-data.xlsx",
                       sheet = "Sheet1")

meta <- meta_raw %>%
  transmute(
    plot = as.integer(Plot),
    collar = as.character(Collar),
    date = as.Date(Date),
    time = format(as.POSIXct(Time, format = "%H:%M"), "%H:%M"),
    vwc1 = as.numeric(VWC1),
    vwc2 = as.numeric(VWC2),
    vwc3 = as.numeric(VWC3),
    mean_vwc = rowMeans(cbind(vwc1, vwc2, vwc3), na.rm = TRUE),
    soil_temp_c = as.numeric(soil_temp_C),
    mean_depth = as.numeric(mean_depth)
  ) %>%
  filter(!is.na(plot)) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, date, collar, time, vwc1, vwc2, vwc3,
         mean_vwc, soil_temp_c, mean_depth) %>%
  arrange(date, plot, collar)

write.csv(meta, "data/processed/field_metadata.csv", row.names = FALSE)
cat("Wrote data/processed/field_metadata.csv\n")
cat(sprintf("  %d rows, %d unique dates\n", nrow(meta), length(unique(meta$date))))
