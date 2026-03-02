# 05_process_flux.R
# Process field flux data (CO2, CH4, N2O)
# Input: data/raw/flux/co2_n20_ch4_allflux2025.xlsx
# Output: data/processed/flux_estimates.csv

library(readxl)
library(dplyr)
library(lubridate)

treatment_key <- read.csv("data/processed/treatment_key.csv")

flux_raw <- read_excel("data/raw/flux/co2_n20_ch4_allflux2025.xlsx",
                       sheet = "co2_n20_ch4_allflux2025")

flux <- flux_raw %>%
  transmute(
    plot = as.integer(plot_rep),
    collar = collar_rep,
    # Convert DOY to date (year = 2025)
    doy = as.numeric(DOY_initial_value),
    date = as.Date(doy - 1, origin = "2025-01-01"),
    FCO2_DRY = as.numeric(FCO2_DRY),
    FCH4_DRY = as.numeric(FCH4_DRY),
    FN2O = as.numeric(FN2O),
    # Keep fit statistics for quality filtering
    FCO2_R2 = as.numeric(FCO2_DRY_R2),
    FCH4_R2 = as.numeric(FCH4_DRY_R2),
    FN2O_R2 = as.numeric(FN2O_R2)
  ) %>%
  filter(!is.na(plot))

# Merge split measurement dates:
# Sept 10/11 → Sept 10; Oct 14/15 → Oct 14
flux <- flux %>%
  mutate(date = case_when(
    date == as.Date("2025-09-11") ~ as.Date("2025-09-10"),
    date == as.Date("2025-10-15") ~ as.Date("2025-10-14"),
    TRUE ~ date
  ))

# Add treatment
flux <- flux %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, date, collar, FCO2_DRY, FCH4_DRY, FN2O,
         FCO2_R2, FCH4_R2, FN2O_R2) %>%
  arrange(date, plot, collar)

# Flag anomalous values
n_neg_co2 <- sum(flux$FCO2_DRY < 0, na.rm = TRUE)
n_high_n2o <- sum(abs(flux$FN2O) > 50, na.rm = TRUE)
if (n_neg_co2 > 0) message(sprintf("  %d negative FCO2_DRY values", n_neg_co2))
if (n_high_n2o > 0) message(sprintf("  %d |FN2O| > 50 values", n_high_n2o))

write.csv(flux, "data/processed/flux_estimates.csv", row.names = FALSE)
cat("Wrote data/processed/flux_estimates.csv\n")
cat(sprintf("  %d rows, %d unique dates, %d plots\n",
            nrow(flux), length(unique(flux$date)), length(unique(flux$plot))))
