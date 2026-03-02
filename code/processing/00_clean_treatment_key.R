# 00_clean_treatment_key.R
# Read treatment list and standardize treatment names
# Output: data/processed/treatment_key.csv

library(readxl)
library(dplyr)

treatment <- read_excel("data/raw/treatment_list.xlsx", sheet = "Sheet1") %>%
  rename(plot = Plot, treatment = Treatment) %>%
  mutate(
    plot = as.integer(plot),
    treatment = tolower(treatment)
  ) %>%
  arrange(plot)

stopifnot(nrow(treatment) == 15)
stopifnot(all(treatment$treatment %in% c("control", "slurry", "compost")))

write.csv(treatment, "data/processed/treatment_key.csv", row.names = FALSE)
cat("Wrote data/processed/treatment_key.csv\n")
