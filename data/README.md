# Churchtown Dairy — Manure Management & GHG Flux Experiment (2025)

## Overview

Field experiment at Hudson Carbon (Churchtown Dairy, NY) investigating the effects of manure fertilizer type on soil greenhouse gas emissions, soil biogeochemistry, and vegetation productivity during the 2025 growing season.

## Experimental Design

- **15 plots**, 5 replicates per treatment
- **3 treatments**: slurry manure, compost manure, no fertilizer (control)
- **Application date**: May 28, 2025
- **Plot assignments**:
  - Control (no fertilizer): plots 2, 5, 7, 10, 15
  - Slurry manure: plots 1, 6, 11, 12, 13
  - Compost manure: plots 3, 4, 8, 9, 14

## Data Structure

### `raw/` — Untouched instrument and lab outputs

- `flux/` — In-situ GHG flux measurements (LI-COR SmartChamber + trace gas analyzer)
  - `json/` — SmartChamber .json exports (12 sampling dates, May–Oct 2025)
  - `data/` — Trace gas analyzer .data files
  - `metadata_pdfs/` — Scanned field datasheets (soil moisture, temperature, timing)
  - `co2_n20_ch4_allflux2025.xlsx` — Compiled flux estimates (CO2, CH4, N2O)
  - `smartchamber_data_key.xlsx` — Variable definitions for SmartChamber output
- `soil/` — Lab soil analyses
  - `sir/` — Substrate-induced respiration (3 timepoints). Methods: LGR closed-loop (timepoints 1–2) and IRGA flow-through (timepoint 3).
  - `cmin/` — Carbon mineralization incubation data (3 timepoints, multiple dates each). Methods: LGR (timepoint 1) and IRGA (timepoints 2–3).
  - `cmin_mass/` — Soil masses and moisture for C/N mineralization normalization
  - `manure_amendment/` — Manure-specific incubation samples
  - `gwc/` — Gravimetric water content (3 timepoints)
  - `ph/` — Soil pH (3 timepoints)
  - `microbes/` — DNA sample inventory (tracking only, no sequence data here)
  - `paper_datasheets/` — Photos of original handwritten lab datasheets
- `dairy_one/` — External lab results (Dairy One)
  - `soil/` — Soil nutrient analysis (45 samples, Jan 2026)
  - `forage/` — Forage quality analysis
  - `manure/` — Manure composition analysis
- `vegetation/` — End-of-season aboveground biomass (Oct 2025)
- `field_metadata/` — In-situ VWC, soil temp, GPS coordinates
- `treatment_list.xlsx` — Plot-to-treatment lookup

### `processed/` — Cleaned, calculated outputs (CSV)

All scripts in `code/` read from `raw/` and write here. Each CSV is tidy format with consistent column naming:
- `plot` (integer 1–15), `treatment` (control/slurry/compost), `timepoint` (1/2/3 or date), `replicate` (A/B)

## Instrument Methods

**SIR and C-min** were measured using two instruments depending on availability:

1. **IRGA (LI-COR)**: Flow-through infrared gas analyzer. CO2 concentrations determined from peak areas (integrals) calibrated against standard curves interpolated between bracketing standard sets. Calculation pipeline in `protocols/carbon_mineralization/irga_calculation_code/`.

2. **LGR (Los Gatos Research)**: Closed-loop laser gas analyzer. CO2 concentrations determined by dilution (C1V1 = C2V2): a known volume of headspace gas is injected into the LGR loop, and the concentration change is back-calculated. Calculation script in `protocols/sir/lgr_sir_calc.R`.

Both methods output **µg CO2-C hr⁻¹ g⁻¹ dry soil**.

## Constants

- Veff (effective headspace volume): 344.6 mL
- Vsam (sample injection volume, LGR): 5 mL
- Temperature assumed: 25°C (298.15 K)
- Pressure: 1 atm
