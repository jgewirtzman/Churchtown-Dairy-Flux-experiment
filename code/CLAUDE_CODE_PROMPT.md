# Claude Code Prompt — Churchtown Dairy Data Processing

Use this prompt in Claude Code, run from the project root directory (`Churchtown-Dairy/` or wherever this repo lives). Read this entire prompt before writing any code.

---

## Project Context

This is a field experiment at Hudson Carbon (Churchtown Dairy, NY) studying how manure fertilizer type affects soil greenhouse gas emissions and biogeochemistry. There are **15 plots** with 3 treatments (5 replicates each):

- **Control** (no fertilizer): plots 2, 5, 7, 10, 15
- **Slurry manure**: plots 1, 6, 11, 12, 13
- **Compost manure**: plots 3, 4, 8, 9, 14

Manure was applied **May 28, 2025**. Measurements span May–December 2025 across 3 soil sampling timepoints and 12 flux sampling dates.

---

## Task

Write a series of numbered R scripts in `code/` that read raw data from `data/raw/`, process/calculate variables, and write tidy CSVs to `data/processed/`. Use the tidyverse ecosystem (readxl, dplyr, tidyr, lubridate, etc.). Each output CSV should have consistent columns: `plot` (integer 1–15), `treatment` (control/slurry/compost), `timepoint` (1, 2, or 3 — or date where relevant), `replicate` (A or B if applicable).

### Script 00: Treatment Key (`00_clean_treatment_key.R`)

Read `data/raw/treatment_list.xlsx` (Sheet1, columns: Plot, Treatment). Standardize treatment names to lowercase (control/slurry/compost). Write `data/processed/treatment_key.csv` with columns: `plot`, `treatment`.

### Script 01: GWC (`01_process_gwc.R`)

Read `data/raw/soil/gwc/GWC_1.xlsx`, `GWC_2.xlsx`, `GWC_3.xlsx`. Each has a Sheet1 with columns including Plot, Replicate, Sample_ID, and a GWC column (already calculated). Extract the GWC value for each sample. Combine all 3 timepoints, add a `timepoint` column (1, 2, 3). Join with treatment key. Write `data/processed/gwc.csv`.

Note: GWC_1 has 15 rows (1 per plot), GWC_2 and GWC_3 have 30 rows (2 replicates per plot). Column names may vary slightly between files — inspect headers and adapt.

### Script 02: pH (`02_process_ph.R`)

Read `data/raw/soil/ph/pH_1.xlsx`, `pH_2.xlsx`, `pH_3.xlsx`. Each has Sheet1 with columns including Plot, Replicate, Sample_ID, pH. Extract pH values, combine 3 timepoints, join with treatment key. Write `data/processed/ph.csv`.

Each file has 30 rows (2 replicates × 15 plots).

### Script 03: SIR (`03_process_sir.R`)

This is the most complex script because two different instruments were used across timepoints.

**SIR_1** (LGR closed-loop method):
- Mass sheet: columns include Sample_ID, Lab_No, Mass_Soil_g (this is FRESH mass)
- Gas sheet: columns include Lab_No, Date_Flushed, Time_Flushed, LGR_time, Pre-injection, Post-injection, difference, Veff

Calculation (LGR method — based on `protocols/sir/lgr_sir_calc.R`):
1. Calculate CO2 concentration in headspace using dilution: `CO2_conc = ((Vsam * Pre-injection) + (Veff * difference)) / Vsam` (where Vsam = 5 mL, Veff from data or default 344.6 mL)
2. Convert to moles: `V_air_L = Veff / 1000; moles_air = (1 atm * V_air_L) / (0.08206 * 298.15); moles_CO2 = (CO2_conc / 1e6) * moles_air`
3. Convert to µg C: `CO2_C_ug = moles_CO2 * 12.011 * 1e6`
4. Calculate incubation time in hours from flush datetime to LGR datetime
5. SIR = `CO2_C_ug / soil_dry_mass / incubation_hours` (µg CO2-C hr⁻¹ g⁻¹ dry soil)

**Important**: Mass_Soil_g in the Mass sheet is FRESH mass. To get dry mass, you need GWC from the corresponding timepoint: `dry_mass = fresh_mass / (1 + GWC)`. Load the GWC data from `data/processed/gwc.csv` for this.

**SIR_2** — has both LGR (Gas sheet) and IRGA (Sheet3) data. Flag this with a comment: `# TODO: Confirm with Jon which method to use for SIR_2. Processing both for now.`
- Process the Gas sheet with the LGR method above
- Process Sheet3 with the IRGA method below

**SIR_3** (IRGA flow-through method):
- Mass sheet: same structure as SIR_1
- Gas sheet: 20 columns in Bradford Lab IRGA format including `sir.id`/`cmin.id`, `standard.co2`, `date.flush`, `time.flush`, `date.irga`, `time.irga`, `irga.integral`, `soil.volume`, `soil.dry.mass`, `times.sampled`, `std.value.1` through `std.value.4`, `std.start.time`, `std.end.time`

Calculation (IRGA method — based on `protocols/carbon_mineralization/irga_calculation_code/fun_cmin_calc_general.R`):
1. Identify standard rows (where std.value.1 is not NA)
2. Calculate mean standard integral for each standard set
3. Interpolate corrected standard between bracketing standard sets using time-weighted linear interpolation
4. `measuredCO2 = irga.integral * (standard.co2 / correctedStandard)` (ppm)
5. `dilutionFactor = ((5 * times.sampled) / (57.15 - soil.volume)) + 1`
6. `concentrationCO2 = measuredCO2 * dilutionFactor` (ppm)
7. `volumeCO2 = concentrationCO2 * ((57.15 - soil.volume) / 1000)` (µL)
8. `molesCO2 = (volumeCO2 / 22.414) * 273.15 / 293.15` (µmol)
9. `CO2C = molesCO2 * 12.011` (µg)
10. `CO2CperHour = CO2C / incubationTime` (µg hr⁻¹)
11. `SIR = CO2CperHour / soil.dry.mass` (µg CO2-C hr⁻¹ g⁻¹ dry soil)

Note: For SIR (unlike C-min), there is only ONE measurement per sample — no cumulative integration needed. The incubation is typically ~4 hours with glucose substrate.

Join all timepoints with treatment key. Write `data/processed/sir.csv` with columns: `plot`, `treatment`, `timepoint`, `replicate`, `sir_ug_co2c_hr_g`, `method` (LGR or IRGA).

### Script 04: Carbon Mineralization (`04_process_cmin.R`)

Read C-min files and C-NMin_Mass files. Each C-min file has MULTIPLE sheets — one per measurement date within that incubation run.

**C-min_1** (LGR method, 5 sheets: june3, june10, june17, june23, july1):
- Each sheet: Lab_No, Date_Flushed, Time_Flushed, LGR_time, Pre-injection, Post-injection, Difference, Veff
- Use the LGR calculation method from SIR above
- C-NMin_Mass_1.xlsx provides soil masses (Sheet1: Plot, Timepoint, Replicate, Sample_ID, Lab_No, Mass_Soil_g, GWC)

**C-min_2** (IRGA method, 6 sheets: aug4, aug8, aug11, aug18, Aug 25, sep 2):
- Each sheet: Bradford Lab IRGA format (20 columns with irga.integral, std.values, etc.)
- Use the IRGA calculation method from SIR above
- C-NMin_Mass_2.xlsx provides soil masses

**C-min_3** (IRGA method, 6 sheets: nov26, nov29, dec2, dec9, dec16, dec23):
- Same IRGA format as C-min_2
- C-NMin_Mass_3.xlsx provides soil masses

For each incubation run (1, 2, 3):
1. Calculate CO2-C production rate (µg CO2-C hr⁻¹ g⁻¹ dry soil) for each measurement date
2. Calculate cumulative C mineralization across dates using trapezoidal integration (AUC). Days should be calculated from the first flush date of each run. Use `DescTools::AUC()` or manual trapezoid integration.

Output TWO files:
- `data/processed/cmin_timeresolved.csv`: columns `plot`, `treatment`, `timepoint` (1/2/3), `replicate`, `date`, `day` (days since start of incubation), `cmin_rate_ug_co2c_hr_g`, `method`
- `data/processed/cmin_cumulative.csv`: columns `plot`, `treatment`, `timepoint`, `replicate`, `cumulative_ug_co2c_g`, `method`

### Script 05: Flux Estimates (`05_process_flux.R`)

Read `data/raw/flux/co2_n20_ch4_allflux2025.xlsx`. This already contains calculated flux rates. Clean it:
1. Convert DOY to date (year = 2025)
2. Merge Sept 10/11 and Oct 14/15 dates (measurements split across two days)
3. Add treatment column using the treatment key
4. Select key columns: `plot`, `treatment`, `date`, `FCO2_DRY`, `FCH4_DRY`, `FN2O` and any relevant metadata (soil temp, moisture if present)

Write `data/processed/flux_estimates.csv`.

### Script 06: Field Metadata (`06_process_field_metadata.R`)

Read `data/raw/field_metadata/In situ fluxes meta-data.xlsx` (Sheet1). Columns: Plot, Collar, Date, Time, VWC1, VWC2, VWC3, soil_temp_C. Calculate mean VWC from the 3 readings. Join with treatment key. Write `data/processed/field_metadata.csv`.

### Script 10: Combine All (`10_combine_all.R`)

Read all processed CSVs. Create a master summary table with one row per plot × timepoint, joining:
- Treatment key
- GWC (averaged across replicates if needed)
- pH (averaged across replicates)
- SIR
- Cumulative C-min
- Dairy One soil chemistry (`data/raw/dairy_one/soil/DairyOne_soildata.csv` — join on plot number via "Field Name" column)

Write `data/processed/master_soil_properties.csv`.

---

## Important Notes

- Read the reference R scripts in `protocols/` before writing the SIR and C-min calculation code — they contain the exact formulas and logic
- Handle the LGR vs IRGA method distinction carefully. Label every output row with `method`
- Some GWC values in GWC_3 may be stored as text — coerce to numeric
- Time columns in some sheets may be in different formats (HH:MM, seconds since midnight, or Excel serial time) — inspect and handle accordingly
- The `standard.co2` column in IRGA data contains the known standard concentration (ppm) used for calibration — it should be present in each row
- For C-min, the `soil.dry.mass` is in the IRGA sheets themselves; for LGR sheets, compute from fresh mass × GWC from the C-NMin_Mass files
- Flag any samples with anomalous values (negative SIR, impossibly high flux rates, etc.) rather than silently dropping them
