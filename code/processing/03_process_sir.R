# 03_process_sir.R
# Process Substrate-Induced Respiration data from 3 timepoints
# SIR_1: LGR closed-loop method
# SIR_2: Both LGR (Gas sheet - empty) and IRGA (Sheet3) — flagged
# SIR_3: IRGA flow-through method
# Output: data/processed/sir.csv

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

treatment_key <- read.csv("data/processed/treatment_key.csv")
gwc <- read.csv("data/processed/gwc.csv")

# Compute mean GWC per plot per timepoint (for replicate-level fallback)
gwc_plot_avg <- gwc %>%
  group_by(plot, timepoint) %>%
  summarize(gwc_avg = mean(gwc, na.rm = TRUE), .groups = "drop")

source("code/processing/helpers.R")

# =============================================================================
# LGR calculation function
# Based on protocols/sir/lgr_sir_calc.R
# =============================================================================
calc_sir_lgr <- function(gas_data, mass_data, gwc_data, timepoint_num,
                         Vsam = 5, default_Veff = 344.6, jar_volume = 57.15) {
  # Constants
  R_gas <- 0.08206  # L atm K-1 mol-1
  P_atm <- 1        # atm
  T_K   <- 298.15   # 25 C in Kelvin
  C_mol <- 12.011   # g/mol

  # Separate standards from samples
  standards <- gas_data %>%
    filter(grepl("Standard", as.character(Lab_No), ignore.case = TRUE))
  samples <- gas_data %>%
    filter(!grepl("Standard|Zero Air", as.character(Lab_No), ignore.case = TRUE)) %>%
    filter(!is.na(Lab_No))

  # Compute Veff from standards if possible
  if (nrow(standards) > 0 &&
      any(!is.na(standards$`Pre-injection`) & !is.na(standards$`Post-injection`))) {
    std_calc <- standards %>%
      mutate(
        pre  = as.numeric(`Pre-injection`),
        post = as.numeric(`Post-injection`),
        diff = post - pre
      ) %>%
      filter(!is.na(pre) & !is.na(post) & diff != 0) %>%
      mutate(Veff = (Vsam * (1976 - post)) / diff)
    Veff <- if (nrow(std_calc) > 0) mean(std_calc$Veff, na.rm = TRUE) else default_Veff
  } else {
    Veff <- default_Veff
  }

  # Process samples
  samples <- samples %>%
    mutate(
      Lab_No = as.numeric(Lab_No),
      pre  = as.numeric(`Pre-injection`),
      post = as.numeric(`Post-injection`),
      diff = post - pre
    ) %>%
    filter(!is.na(pre) & !is.na(post))

  # Parse date/time — columns may be character (Excel serial) or POSIXct
  samples <- samples %>%
    mutate(
      flush_date = sapply(Date_Flushed, parse_excel_date) %>% as.Date(origin = "1970-01-01"),
      flush_time_str = sapply(Time_Flushed, parse_time_value),
      lgr_time_str   = sapply(LGR_time, parse_time_value)
    ) %>%
    # Drop rows where date or time couldn't be parsed
    filter(!is.na(flush_date) & !is.na(flush_time_str) & !is.na(lgr_time_str)) %>%
    mutate(
      flush_dt = ymd_hms(paste(flush_date, flush_time_str)),
      lgr_dt   = ymd_hms(paste(flush_date, lgr_time_str)),
      # If LGR time is before flush time, measurement was next day
      lgr_dt = if_else(lgr_dt < flush_dt, lgr_dt + days(1), lgr_dt),
      incubation_hours = as.numeric(difftime(lgr_dt, flush_dt, units = "hours"))
    )

  # CO2 concentration and SIR calculation
  # Use .env$ to avoid shadowing by the Excel 'Veff' column in the gas data
  # NOTE: CO2_conc reconstruction uses Veff (LGR loop volume) — this is correct
  #       for back-calculating the sample concentration from the dilution.
  #       But total CO2 uses jar_volume (headspace where CO2 accumulated),
  #       NOT Veff. Using Veff would overestimate by ~6x (345 mL vs 57 mL).
  samples <- samples %>%
    mutate(
      CO2_conc = ((.env$Vsam * pre) + (.env$Veff * diff)) / .env$Vsam,
      V_air_L  = .env$jar_volume / 1000,
      moles_air = (P_atm * V_air_L) / (R_gas * T_K),
      moles_CO2 = (CO2_conc / 1e6) * moles_air,
      CO2_C_ug  = moles_CO2 * C_mol * 1e6
    )

  # Join with mass data to get plot, replicate, fresh mass
  samples <- samples %>%
    left_join(mass_data %>% select(Lab_No, Plot, Replicate, Mass_Soil_g),
              by = "Lab_No")

  # Join with GWC to compute dry mass
  samples <- samples %>%
    mutate(
      plot = as.integer(Plot),
      replicate = as.character(Replicate),
      timepoint = timepoint_num
    ) %>%
    left_join(gwc_data, by = c("plot", "timepoint", "replicate")) %>%
    left_join(gwc_plot_avg, by = c("plot", "timepoint")) %>%
    mutate(
      gwc_value = coalesce(gwc, gwc_avg),
      fresh_mass = as.numeric(Mass_Soil_g),
      dry_mass = fresh_mass / (1 + gwc_value),
      sir_ug_co2c_hr_g = CO2_C_ug / dry_mass / incubation_hours
    )

  # Diagnostics
  message(sprintf("SIR LGR timepoint %d diagnostics:", timepoint_num))
  message(sprintf("  Veff=%.1f mL (dilution), jar_volume=%.1f mL (ideal gas law)",
                  Veff, jar_volume))
  message(sprintf("  Pre ppm=[%.0f,%.0f], diff=[%.0f,%.0f]",
                  min(samples$pre), max(samples$pre),
                  min(samples$diff), max(samples$diff)))
  message(sprintf("  CO2_conc=[%.0f,%.0f] ppm, CO2_C_ug=[%.1f,%.1f]",
                  min(samples$CO2_conc, na.rm=TRUE), max(samples$CO2_conc, na.rm=TRUE),
                  min(samples$CO2_C_ug, na.rm=TRUE), max(samples$CO2_C_ug, na.rm=TRUE)))
  message(sprintf("  Incubation hrs=[%.1f,%.1f]",
                  min(samples$incubation_hours, na.rm=TRUE), max(samples$incubation_hours, na.rm=TRUE)))
  message(sprintf("  Fresh mass=[%.2f,%.2f]g, GWC=[%.3f,%.3f], Dry mass=[%.2f,%.2f]g",
                  min(samples$fresh_mass, na.rm=TRUE), max(samples$fresh_mass, na.rm=TRUE),
                  min(samples$gwc_value, na.rm=TRUE), max(samples$gwc_value, na.rm=TRUE),
                  min(samples$dry_mass, na.rm=TRUE), max(samples$dry_mass, na.rm=TRUE)))
  message(sprintf("  SIR rate=[%.2f,%.2f], mean=%.2f ug CO2-C/hr/g (n=%d)",
                  min(samples$sir_ug_co2c_hr_g, na.rm=TRUE), max(samples$sir_ug_co2c_hr_g, na.rm=TRUE),
                  mean(samples$sir_ug_co2c_hr_g, na.rm=TRUE), sum(!is.na(samples$sir_ug_co2c_hr_g))))

  # Flag anomalous values
  # Check for mass outliers (>10g is suspicious for SIR jars)
  samples <- samples %>%
    mutate(flag = case_when(
      fresh_mass > 10 ~ "mass_outlier",
      sir_ug_co2c_hr_g < 0 ~ "negative_SIR",
      sir_ug_co2c_hr_g > 100 ~ "very_high_SIR",
      is.na(sir_ug_co2c_hr_g) ~ "NA_value",
      TRUE ~ NA_character_
    ))

  if (any(!is.na(samples$flag))) {
    message(sprintf("  Flagged: %s",
                    paste(samples$flag[!is.na(samples$flag)], collapse = ", ")))
  }

  samples %>%
    select(plot, replicate, sir_ug_co2c_hr_g, flag) %>%
    mutate(timepoint = timepoint_num, method = "LGR")
}

# =============================================================================
# IRGA calculation function
# Based on protocols/carbon_mineralization/irga_calculation_code/fun_cmin_calc_general.R
# Adapted for SIR (single measurement, no cumulative integration)
# =============================================================================
calc_sir_irga <- function(irga_data, mass_data, gwc_data, timepoint_num,
                          jar_volume = 57.15, standard_co2 = 1976) {
  C_mol <- 12.011  # g/mol

  # Filter to non-empty rows
  df <- irga_data %>%
    filter(!is.na(irga.integral)) %>%
    mutate(row_idx = row_number())

  # Identify standard rows (std.value.1 is not NA)
  std_rows <- df %>%
    filter(!is.na(std.value.1)) %>%
    mutate(
      meanStandard = rowMeans(
        cbind(std.value.1, std.value.2, std.value.3, std.value.4),
        na.rm = TRUE
      )
    )

  # Compute corrected standard via interpolation
  if (nrow(std_rows) == 0) {
    stop("No standard rows found in IRGA data")
  } else if (nrow(std_rows) == 1) {
    # Single standard set — use mean for all samples
    df$correctedStandard <- std_rows$meanStandard[1]
  } else {
    # Multiple standard sets — interpolate based on row index
    df$correctedStandard <- NA_real_
    for (i in seq_len(nrow(df))) {
      row_i <- df$row_idx[i]
      # Find bracketing standards
      before <- std_rows %>% filter(row_idx <= row_i) %>% slice_tail(n = 1)
      after  <- std_rows %>% filter(row_idx >= row_i) %>% slice_head(n = 1)

      if (nrow(before) == 0) {
        df$correctedStandard[i] <- after$meanStandard[1]
      } else if (nrow(after) == 0) {
        df$correctedStandard[i] <- before$meanStandard[1]
      } else if (before$row_idx[1] == after$row_idx[1]) {
        df$correctedStandard[i] <- before$meanStandard[1]
      } else {
        # Linear interpolation
        frac <- (row_i - before$row_idx[1]) /
                (after$row_idx[1] - before$row_idx[1])
        df$correctedStandard[i] <- before$meanStandard[1] +
          frac * (after$meanStandard[1] - before$meanStandard[1])
      }
    }
  }

  # Parse flush and IRGA datetimes
  # date columns are POSIXct; time columns are POSIXct with 1899-12-31 date
  df <- df %>%
    mutate(
      flush_date_parsed = as.Date(date.flush),
      irga_date_parsed  = as.Date(date.irga),
      flush_time_str = sapply(time.flush, parse_time_value),
      irga_time_str  = sapply(time.irga, parse_time_value),
      flush_dt = ymd_hms(paste(flush_date_parsed, flush_time_str)),
      irga_dt  = ymd_hms(paste(irga_date_parsed, irga_time_str)),
      irga_dt = if_else(irga_dt < flush_dt, irga_dt + days(1), irga_dt),
      incubationTime = as.numeric(difftime(irga_dt, flush_dt, units = "hours"))
    )

  # For SIR: times.sampled = 1, soil.volume defaults to 0 if NA
  df <- df %>%
    mutate(
      times.sampled = replace_na(times.sampled, 1),
      soil.volume   = replace_na(soil.volume, 0)
    )

  # IRGA calculation steps
  df <- df %>%
    mutate(
      measuredCO2     = irga.integral * (standard_co2 / correctedStandard),  # ppm
      dilutionFactor  = ((5 * times.sampled) / (jar_volume - soil.volume)) + 1,
      concentrationCO2 = measuredCO2 * dilutionFactor,                        # ppm
      volumeCO2       = concentrationCO2 * ((jar_volume - soil.volume) / 1000), # uL
      molesCO2        = (volumeCO2 / 22.414) * 273.15 / 293.15,              # umol
      CO2C            = molesCO2 * C_mol                                      # ug
    )

  # Get the ID column (sir.id or cmin.id)
  id_col <- if ("sir.id" %in% names(df)) "sir.id" else "cmin.id"

  # Join with mass data on Lab_No (ensure numeric type match)
  df <- df %>%
    rename(Lab_No = !!sym(id_col)) %>%
    mutate(Lab_No = as.numeric(Lab_No)) %>%
    left_join(mass_data %>%
                mutate(Lab_No = as.numeric(Lab_No)) %>%
                select(Lab_No, Plot, Replicate, Mass_Soil_g),
              by = "Lab_No")

  # Join with GWC to compute dry mass
  df <- df %>%
    mutate(
      plot = as.integer(Plot),
      replicate = as.character(Replicate),
      timepoint = timepoint_num
    ) %>%
    left_join(gwc_data, by = c("plot", "timepoint", "replicate")) %>%
    left_join(gwc_plot_avg, by = c("plot", "timepoint")) %>%
    mutate(
      gwc_value = coalesce(gwc, gwc_avg),
      fresh_mass = as.numeric(Mass_Soil_g),
      dry_mass = fresh_mass / (1 + gwc_value),
      CO2CperHour = CO2C / incubationTime,
      sir_ug_co2c_hr_g = CO2CperHour / dry_mass
    )

  # Diagnostics
  message(sprintf("SIR IRGA timepoint %d diagnostics:", timepoint_num))
  message(sprintf("  Standards: n=%d, correctedStandard=[%.1f,%.1f]",
                  nrow(std_rows),
                  min(df$correctedStandard, na.rm=TRUE), max(df$correctedStandard, na.rm=TRUE)))
  message(sprintf("  irga.integral=[%.0f,%.0f], measuredCO2=[%.0f,%.0f] ppm",
                  min(df$irga.integral, na.rm=TRUE), max(df$irga.integral, na.rm=TRUE),
                  min(df$measuredCO2, na.rm=TRUE), max(df$measuredCO2, na.rm=TRUE)))
  message(sprintf("  Incubation hrs=[%.1f,%.1f], dilutionFactor=%.3f",
                  min(df$incubationTime, na.rm=TRUE), max(df$incubationTime, na.rm=TRUE),
                  mean(df$dilutionFactor, na.rm=TRUE)))
  message(sprintf("  Fresh mass=[%.2f,%.2f]g, GWC=[%.3f,%.3f], Dry mass=[%.2f,%.2f]g",
                  min(df$fresh_mass, na.rm=TRUE), max(df$fresh_mass, na.rm=TRUE),
                  min(df$gwc_value, na.rm=TRUE), max(df$gwc_value, na.rm=TRUE),
                  min(df$dry_mass, na.rm=TRUE), max(df$dry_mass, na.rm=TRUE)))
  message(sprintf("  SIR rate=[%.2f,%.2f], mean=%.2f ug CO2-C/hr/g (n=%d)",
                  min(df$sir_ug_co2c_hr_g, na.rm=TRUE), max(df$sir_ug_co2c_hr_g, na.rm=TRUE),
                  mean(df$sir_ug_co2c_hr_g, na.rm=TRUE), sum(!is.na(df$sir_ug_co2c_hr_g))))

  # Flag anomalous values
  df <- df %>%
    mutate(flag = case_when(
      fresh_mass > 10 ~ "mass_outlier",
      sir_ug_co2c_hr_g < 0 ~ "negative_SIR",
      sir_ug_co2c_hr_g > 100 ~ "very_high_SIR",
      is.na(sir_ug_co2c_hr_g) ~ "NA_value",
      TRUE ~ NA_character_
    ))

  if (any(!is.na(df$flag))) {
    message(sprintf("  Flagged: %s",
                    paste(df$flag[!is.na(df$flag)], collapse = ", ")))
  }

  df %>%
    select(plot, replicate, sir_ug_co2c_hr_g, flag) %>%
    mutate(timepoint = timepoint_num, method = "IRGA")
}


# =============================================================================
# SIR_1: LGR method (timepoint 1)
# =============================================================================
cat("Processing SIR_1 (LGR)...\n")

sir1_mass <- read_excel("data/raw/soil/sir/SIR_1.xlsx", sheet = "Mass")
sir1_gas  <- read_excel("data/raw/soil/sir/SIR_1.xlsx", sheet = "Gas")

sir1 <- calc_sir_lgr(
  gas_data = sir1_gas,
  mass_data = sir1_mass,
  gwc_data = gwc %>% select(plot, timepoint, replicate, gwc),
  timepoint_num = 1L
)

# =============================================================================
# SIR_2: Has both LGR (Gas) and IRGA (Sheet3) data
# TODO: Confirm with Jon which method to use for SIR_2. Processing both for now.
# =============================================================================
cat("Processing SIR_2 (LGR + IRGA)...\n")

sir2_mass <- read_excel("data/raw/soil/sir/SIR_2.xlsx", sheet = "Mass")
sir2_irga <- read_excel("data/raw/soil/sir/SIR_2.xlsx", sheet = "Sheet3")
sir2_gas  <- read_excel("data/raw/soil/sir/SIR_2.xlsx", sheet = "Gas")

# Check if LGR Gas data has actual measurements
has_lgr_data <- sir2_gas %>%
  filter(!is.na(Lab_No), !grepl("Standard|Zero Air", as.character(Lab_No))) %>%
  filter(!is.na(`Pre-injection`) | !is.na(`Post-injection`)) %>%
  nrow() > 0

if (has_lgr_data) {
  sir2_lgr <- calc_sir_lgr(
    gas_data = sir2_gas,
    mass_data = sir2_mass,
    gwc_data = gwc %>% select(plot, timepoint, replicate, gwc),
    timepoint_num = 2L
  )
  cat("  SIR_2 LGR: processed\n")
} else {
  sir2_lgr <- NULL
  cat("  SIR_2 LGR: Gas sheet has no usable data (Pre/Post injection empty)\n")
}

# Process IRGA data from Sheet3
sir2_irga_result <- calc_sir_irga(
  irga_data = sir2_irga,
  mass_data = sir2_mass,
  gwc_data = gwc %>% select(plot, timepoint, replicate, gwc),
  timepoint_num = 2L
)
cat("  SIR_2 IRGA: processed\n")

# =============================================================================
# SIR_3: IRGA method (timepoint 3)
# =============================================================================
cat("Processing SIR_3 (IRGA)...\n")

sir3_mass <- read_excel("data/raw/soil/sir/SIR_3.xlsx", sheet = "Mass")
sir3_gas  <- read_excel("data/raw/soil/sir/SIR_3.xlsx", sheet = "Gas")

# Fix mass outlier: Lab_No 77 has 40.1g (neighbors are 4.0-4.4g; decimal typo)
sir3_mass <- sir3_mass %>%
  mutate(Mass_Soil_g = if_else(as.numeric(Lab_No) == 77 & Mass_Soil_g > 30,
                                4.01, Mass_Soil_g))
cat("  Fixed SIR_3 mass outlier: Lab_No 77 (40.1g -> 4.01g)\n")

sir3 <- calc_sir_irga(
  irga_data = sir3_gas,
  mass_data = sir3_mass,
  gwc_data = gwc %>% select(plot, timepoint, replicate, gwc),
  timepoint_num = 3L
)

# =============================================================================
# Combine all SIR data
# =============================================================================
sir_all <- bind_rows(
  sir1,
  sir2_lgr,          # NULL if LGR data was empty
  sir2_irga_result,
  sir3
) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, timepoint, replicate, sir_ug_co2c_hr_g, method, flag) %>%
  arrange(method, timepoint, plot, replicate)

write.csv(sir_all, "data/processed/sir.csv", row.names = FALSE)
cat("Wrote data/processed/sir.csv\n")
cat(sprintf("  Total rows: %d\n", nrow(sir_all)))
cat(sprintf("  By method: LGR=%d, IRGA=%d\n",
            sum(sir_all$method == "LGR"),
            sum(sir_all$method == "IRGA")))
