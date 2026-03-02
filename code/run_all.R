# run_all.R — Run the entire Churchtown Dairy processing + analysis pipeline
# Run from the project root directory
#
# Dependency order:
#   Stage 1 (processing): raw → data/processed/*.csv
#     00 treatment_key  (no dependencies)
#     01 gwc            (depends on: treatment_key)
#     02 ph             (depends on: treatment_key)
#     03 sir            (depends on: treatment_key, gwc)
#     04 cmin           (depends on: treatment_key, gwc)
#     05 flux           (depends on: treatment_key)
#     06 field_metadata (depends on: treatment_key)
#     07 dairy_one      (depends on: treatment_key)
#
#   Stage 2 (analysis): data/processed/*.csv → summaries + figures
#     10 lab_assays_summary (depends on: gwc, ph, sir, cmin_cumulative)
#     20 visualizations     (depends on: all processed CSVs)

cat("=== STAGE 1: PROCESSING ===\n\n")

cat("--- 00: Treatment Key ---\n")
source("code/processing/00_clean_treatment_key.R")

cat("\n--- 01: GWC ---\n")
source("code/processing/01_process_gwc.R")

cat("\n--- 02: pH ---\n")
source("code/processing/02_process_ph.R")

cat("\n--- 03: SIR ---\n")
source("code/processing/03_process_sir.R")

cat("\n--- 04: C Mineralization ---\n")
source("code/processing/04_process_cmin.R")

cat("\n--- 05: Flux ---\n")
source("code/processing/05_process_flux.R")

cat("\n--- 06: Field Metadata ---\n")
source("code/processing/06_process_field_metadata.R")

cat("\n--- 07: Dairy One ---\n")
source("code/processing/07_process_dairy_one.R")

cat("\n=== STAGE 2: ANALYSIS ===\n\n")

cat("--- 10: Lab Assays Summary ---\n")
source("code/analysis/10_lab_assays_summary.R")

cat("\n--- 20: Visualizations ---\n")
source("code/analysis/20_visualizations.R")

cat("\n=== PIPELINE COMPLETE ===\n")
