# 02_process_ph.R
# Process soil pH data from 3 timepoints
# Each file has 30 rows (2 replicates x 15 plots)
# Output: data/processed/ph.csv

library(readxl)
library(dplyr)

treatment_key <- read.csv("data/processed/treatment_key.csv")

read_ph <- function(file, timepoint) {
  read_excel(file, sheet = "Sheet1") %>%
    transmute(
      plot = as.integer(Plot),
      replicate = as.character(Replicate),
      ph = as.numeric(pH),
      timepoint = timepoint
    )
}

ph_all <- bind_rows(
  read_ph("data/raw/soil/ph/pH_1.xlsx", 1L),
  read_ph("data/raw/soil/ph/pH_2.xlsx", 2L),
  read_ph("data/raw/soil/ph/pH_3.xlsx", 3L)
) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, timepoint, replicate, ph) %>%
  arrange(timepoint, plot, replicate)

# Flag anomalous values
if (any(ph_all$ph < 3 | ph_all$ph > 10, na.rm = TRUE)) {
  warning("Extreme pH values detected — check data")
}

write.csv(ph_all, "data/processed/ph.csv", row.names = FALSE)
cat("Wrote data/processed/ph.csv\n")
cat(sprintf("  %d total rows across 3 timepoints\n", nrow(ph_all)))
