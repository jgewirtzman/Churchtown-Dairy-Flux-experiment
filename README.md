# Churchtown Dairy Flux Experiment

GHG flux experiment studying the effects of variable manure fertilizer application on soil greenhouse gas emissions at Hudson Carbon (Churchtown, NY), 2025 growing season.

## Experimental Design

- **15 plots**, 3 treatments (5 plots each):
  - **Control**: plots 2, 5, 7, 10, 15
  - **Slurry**: plots 1, 6, 11, 12, 13
  - **Compost**: plots 3, 4, 8, 9, 14
- Manure applied May 28, 2025
- Measurements: May–December 2025

## Data

### Raw data (`data/raw/`)

- **Flux**: LI-COR Smart Chamber CO2, CH4, N2O flux measurements (biweekly)
- **Soil**: Lab assays from 3 timepoints — GWC, pH, SIR, C mineralization (IRGA + LGR methods)
- **Vegetation**: Aboveground biomass clippings (0.5 m2 quadrats, Oct 2025)
- **DairyOne**: Forage nutrient quality (15 plots) and manure amendment characterization (compost + slurry)
- **Spatial**: Plot corner and flux collar GPS coordinates

### Processed data (`data/processed/`)

| File | Description |
|------|-------------|
| `treatment_key.csv` | Plot-to-treatment mapping |
| `gwc.csv` | Gravimetric water content (3 timepoints) |
| `ph.csv` | Soil pH (3 timepoints) |
| `sir.csv` | Substrate-induced respiration (IRGA + LGR) |
| `cmin_timeresolved.csv` | C mineralization rates per measurement day |
| `cmin_cumulative.csv` | Cumulative C mineralization per incubation |
| `flux_estimates.csv` | Field GHG flux estimates (CO2, CH4, N2O) |
| `field_metadata.csv` | VWC, soil temperature per measurement date |
| `dairy_one_clean.csv` | DairyOne soil nutrient analysis |
| `dairy_one_forage.csv` | DairyOne forage nutrient quality |
| `dairy_one_manure.csv` | DairyOne manure amendment characterization |
| `biomass.csv` | Aboveground vegetation biomass by plot |
| `lab_assays_summary.csv` | Combined lab assay summary table |

## Pipeline

Run the full pipeline from the project root:

```r
source("code/run_all.R")
```

Or from the terminal:

```bash
Rscript code/run_all.R
```

### Stage 1 — Processing (`code/processing/`)

Reads raw data files, cleans, aligns, and computes derived values. Outputs clean CSVs to `data/processed/`.

| Script | Output |
|--------|--------|
| `00_clean_treatment_key.R` | `treatment_key.csv` |
| `01_process_gwc.R` | `gwc.csv` |
| `02_process_ph.R` | `ph.csv` |
| `03_process_sir.R` | `sir.csv` |
| `04_process_cmin.R` | `cmin_timeresolved.csv`, `cmin_cumulative.csv` |
| `05_process_flux.R` | `flux_estimates.csv` |
| `06_process_field_metadata.R` | `field_metadata.csv` |
| `07_process_dairy_one.R` | `dairy_one_clean.csv` |
| `08_process_biomass.R` | `biomass.csv` |
| `09_process_dairy_one_forage.R` | `dairy_one_forage.csv` |
| `10_process_dairy_one_manure.R` | `dairy_one_manure.csv` |

Shared helper functions (date/time parsing) are in `helpers.R`.

### Stage 2 — Analysis (`code/analysis/`)

Reads processed CSVs and produces summary tables and figures.

| Script | Output |
|--------|--------|
| `10_lab_assays_summary.R` | `data/processed/lab_assays_summary.csv` |
| `20_visualizations.R` | `output/figures/summary_*.png` (5 figures) |

### Output figures (`output/figures/`)

- `summary_lab_assays.png` — GWC, pH, SIR, cumulative C-min by treatment x timepoint
- `summary_cmin_timecourse.png` — C mineralization rate time courses per incubation
- `summary_ghg_fluxes.png` — Field CO2, CH4, N2O flux time series
- `summary_field_conditions.png` — VWC and soil temperature over time
- `summary_biomass.png` — Aboveground vegetation biomass by treatment

## Requirements

R packages: `dplyr`, `tidyr`, `readxl`, `lubridate`, `ggplot2`, `gridExtra`, `DescTools`
