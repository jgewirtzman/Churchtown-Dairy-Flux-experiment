# 04_process_cmin.R
# Process Carbon Mineralization data from 3 incubation runs
# C-min_1: LGR method (5 sheets: june3 — july1)
# C-min_2: IRGA method (6 sheets: aug4 — sep 2)
# C-min_3: IRGA method (6 sheets: nov26 — dec23)
#
# Outputs:
#   data/processed/cmin_timeresolved.csv
#   data/processed/cmin_cumulative.csv

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(DescTools)

treatment_key <- read.csv("data/processed/treatment_key.csv")
gwc <- read.csv("data/processed/gwc.csv")

gwc_plot_avg <- gwc %>%
  group_by(plot, timepoint) %>%
  summarize(gwc_avg = mean(gwc, na.rm = TRUE), .groups = "drop")

source("code/processing/helpers.R")

# =============================================================================
# LGR calculation for C-min
# Uses same physics as SIR LGR but applied across multiple measurement dates
# =============================================================================
calc_cmin_lgr <- function(gas_data, mass_data, sheet_name, Vsam = 5, default_Veff = 344.6,
                          jar_volume = 57.15) {
  R_gas <- 0.08206
  P_atm <- 1
  T_K   <- 298.15
  C_mol <- 12.011

  # Separate standards from samples
  standards <- gas_data %>%
    filter(grepl("Standard", as.character(Lab_No), ignore.case = TRUE))
  samples <- gas_data %>%
    filter(!grepl("Standard|Zero Air", as.character(Lab_No), ignore.case = TRUE)) %>%
    filter(!is.na(Lab_No))

  # Compute Veff from standards if possible
  if (nrow(standards) > 0) {
    std_calc <- standards %>%
      mutate(
        pre  = suppressWarnings(as.numeric(`Pre-injection`)),
        post = suppressWarnings(as.numeric(`Post-injection`)),
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
      pre  = suppressWarnings(as.numeric(`Pre-injection`)),
      post = suppressWarnings(as.numeric(`Post-injection`)),
      diff = post - pre
    ) %>%
    filter(!is.na(pre) & !is.na(post) & diff != 0)

  if (nrow(samples) == 0) {
    message(sprintf("  Sheet '%s': No valid samples after filtering", sheet_name))
    return(NULL)
  }

  # Parse datetimes — columns may be character (Excel serial) or POSIXct
  samples <- samples %>%
    mutate(
      flush_date = sapply(Date_Flushed, parse_excel_date) %>% as.Date(origin = "1970-01-01"),
      flush_time_str = sapply(Time_Flushed, parse_time_value),
      lgr_time_str   = sapply(LGR_time, parse_time_value)
    ) %>%
    filter(!is.na(flush_date) & !is.na(flush_time_str) & !is.na(lgr_time_str)) %>%
    mutate(
      flush_dt = ymd_hms(paste(flush_date, flush_time_str)),
      # C-min measurement always happens the NEXT DAY after flushing (~24 hr incubation)
      # Both times share the same Date_Flushed column, so add 1 day to LGR time
      lgr_dt   = ymd_hms(paste(flush_date, lgr_time_str)) + days(1)
    )

  # Compute incubation time; should be ~24 hrs for C-min
  samples <- samples %>%
    mutate(
      incubation_hours_raw = as.numeric(difftime(lgr_dt, flush_dt, units = "hours")),
      # Flag incubation far from expected ~24 hrs (< 12 or > 48 hrs)
      bad_time = incubation_hours_raw < 12 | incubation_hours_raw > 48,
      incubation_hours = if_else(bad_time, NA_real_, incubation_hours_raw)
    )

  n_bad_time <- sum(samples$bad_time, na.rm = TRUE)
  if (n_bad_time > 0) {
    warning(sprintf("  Sheet '%s': %d/%d samples have bad incubation time (%.1f to %.1f hrs). Flagging as NA.",
                    sheet_name, n_bad_time, nrow(samples),
                    min(samples$incubation_hours_raw), max(samples$incubation_hours_raw)))
  }

  # CO2 concentration calculation
  # Use .env$ to avoid shadowing by the Excel 'Veff' column in the gas data
  # NOTE: CO2_conc uses Veff (LGR loop volume) for dilution back-calculation — correct.
  #       Total CO2 uses jar_volume (headspace), NOT Veff. Veff overestimates ~6x.
  samples <- samples %>%
    mutate(
      CO2_conc = ((.env$Vsam * pre) + (.env$Veff * diff)) / .env$Vsam,
      V_air_L  = .env$jar_volume / 1000,
      moles_air = (P_atm * V_air_L) / (R_gas * T_K),
      moles_CO2 = (CO2_conc / 1e6) * moles_air,
      CO2_C_ug  = moles_CO2 * C_mol * 1e6
    )

  # Join with mass data
  samples <- samples %>%
    left_join(
      mass_data %>%
        transmute(
          Lab_No = as.numeric(Lab_No),
          plot = as.integer(Plot),
          replicate = as.character(Replicate),
          fresh_mass = as.numeric(Mass_Soil_g),
          gwc_mass = as.numeric(GWC)
        ),
      by = "Lab_No"
    ) %>%
    mutate(
      # Mass file GWC is in percent for Mass_1 — convert to fraction
      gwc_frac = if_else(gwc_mass > 1, gwc_mass / 100, gwc_mass),
      dry_mass = fresh_mass / (1 + gwc_frac),
      cmin_rate_ug_co2c_hr_g = CO2_C_ug / dry_mass / incubation_hours,
      date = flush_date
    )

  # Diagnostics
  message(sprintf("  Sheet '%s' diagnostics (LGR):", sheet_name))
  message(sprintf("    Veff=%.1f mL (dilution), jar_volume=%.1f mL (ideal gas law)",
                  Veff, jar_volume))
  message(sprintf("    Pre ppm=[%.0f,%.0f], Post-Pre diff=[%.0f,%.0f]",
                  min(samples$pre), max(samples$pre),
                  min(samples$diff), max(samples$diff)))
  message(sprintf("    Incubation hrs=[%.1f,%.1f], CO2_C_ug=[%.1f,%.1f]",
                  min(samples$incubation_hours, na.rm=TRUE), max(samples$incubation_hours, na.rm=TRUE),
                  min(samples$CO2_C_ug, na.rm=TRUE), max(samples$CO2_C_ug, na.rm=TRUE)))
  message(sprintf("    Fresh mass=[%.2f,%.2f]g, GWC=[%.3f,%.3f], Dry mass=[%.2f,%.2f]g",
                  min(samples$fresh_mass, na.rm=TRUE), max(samples$fresh_mass, na.rm=TRUE),
                  min(samples$gwc_frac, na.rm=TRUE), max(samples$gwc_frac, na.rm=TRUE),
                  min(samples$dry_mass, na.rm=TRUE), max(samples$dry_mass, na.rm=TRUE)))
  valid_rates <- samples$cmin_rate_ug_co2c_hr_g[!is.na(samples$cmin_rate_ug_co2c_hr_g)]
  if (length(valid_rates) > 0) {
    message(sprintf("    Rate (ug CO2-C/hr/g)=[%.2f,%.2f], mean=%.2f, n=%d",
                    min(valid_rates), max(valid_rates), mean(valid_rates), length(valid_rates)))
  } else {
    message("    Rate: ALL NA")
  }

  # Flag anomalous values
  samples <- samples %>%
    mutate(flag = case_when(
      bad_time ~ "bad_incubation_time",
      cmin_rate_ug_co2c_hr_g < 0 ~ "negative_rate",
      is.na(cmin_rate_ug_co2c_hr_g) ~ "NA_value",
      TRUE ~ NA_character_
    ))

  samples %>%
    select(Lab_No, plot, replicate, date, cmin_rate_ug_co2c_hr_g, dry_mass, flag)
}


# =============================================================================
# IRGA calculation for C-min
# Based on protocols/carbon_mineralization/irga_calculation_code/fun_cmin_calc_general.R
# =============================================================================
calc_cmin_irga <- function(irga_data, mass_data, sheet_name,
                           times_sampled_default = 1,
                           jar_volume = 57.15, standard_co2 = 1976) {
  C_mol <- 12.011

  # Clean column names: handle cmin.id or sir.id
  id_col <- if ("cmin.id" %in% names(irga_data)) "cmin.id" else "sir.id"

  df <- irga_data %>%
    filter(!is.na(.data[[id_col]])) %>%
    # Remove rows with non-numeric irga.integral (e.g., "X")
    mutate(irga.integral = suppressWarnings(as.numeric(irga.integral))) %>%
    filter(!is.na(irga.integral)) %>%
    mutate(row_idx = row_number())

  if (nrow(df) == 0) {
    message(sprintf("  Sheet '%s': No valid IRGA rows", sheet_name))
    return(NULL)
  }

  # Identify standard rows
  std_rows <- df %>%
    filter(!is.na(std.value.1)) %>%
    mutate(
      meanStandard = rowMeans(
        cbind(
          as.numeric(std.value.1),
          as.numeric(std.value.2),
          as.numeric(std.value.3),
          as.numeric(std.value.4)
        ),
        na.rm = TRUE
      )
    )

  if (nrow(std_rows) == 0) {
    message(sprintf("  Sheet '%s': No standard rows found — using standard.co2 directly", sheet_name))
    df$correctedStandard <- standard_co2
  } else if (nrow(std_rows) == 1) {
    df$correctedStandard <- std_rows$meanStandard[1]
  } else {
    # Interpolate between standard sets
    df$correctedStandard <- NA_real_
    for (i in seq_len(nrow(df))) {
      ri <- df$row_idx[i]
      before <- std_rows %>% filter(row_idx <= ri) %>% slice_tail(n = 1)
      after  <- std_rows %>% filter(row_idx >= ri) %>% slice_head(n = 1)
      if (nrow(before) == 0) {
        df$correctedStandard[i] <- after$meanStandard[1]
      } else if (nrow(after) == 0) {
        df$correctedStandard[i] <- before$meanStandard[1]
      } else if (before$row_idx[1] == after$row_idx[1]) {
        df$correctedStandard[i] <- before$meanStandard[1]
      } else {
        frac <- (ri - before$row_idx[1]) / (after$row_idx[1] - before$row_idx[1])
        df$correctedStandard[i] <- before$meanStandard[1] +
          frac * (after$meanStandard[1] - before$meanStandard[1])
      }
    }
  }

  # Parse datetimes — date cols may be POSIXct or character (Excel serial);
  # time cols may be POSIXct with 1899-12-31 date or character fractions
  df <- df %>%
    mutate(
      flush_date_parsed = sapply(date.flush, parse_excel_date) %>% as.Date(origin = "1970-01-01"),
      irga_date_parsed  = sapply(date.irga, parse_excel_date) %>% as.Date(origin = "1970-01-01"),
      flush_time_str = sapply(time.flush, parse_time_value),
      irga_time_str  = sapply(time.irga, parse_time_value)
    ) %>%
    filter(!is.na(flush_date_parsed) & !is.na(irga_date_parsed) &
           !is.na(flush_time_str) & !is.na(irga_time_str)) %>%
    mutate(
      flush_dt = ymd_hms(paste(flush_date_parsed, flush_time_str)),
      irga_dt  = ymd_hms(paste(irga_date_parsed, irga_time_str)),
      # Only add 1 day if IRGA time is slightly before flush (same-day wrap)
      # Do NOT add 1 day if dates are different (would mask date errors)
      irga_dt = if_else(
        irga_date_parsed == flush_date_parsed & irga_dt < flush_dt,
        irga_dt + days(1), irga_dt
      ),
      incubationTime_raw = as.numeric(difftime(irga_dt, flush_dt, units = "hours")),
      # Flag only truly invalid incubation times (negative or zero)
      bad_time = incubationTime_raw <= 0,
      incubationTime = if_else(bad_time, NA_real_, incubationTime_raw)
    )

  n_bad_time <- sum(df$bad_time, na.rm = TRUE)
  if (n_bad_time > 0) {
    warning(sprintf("  Sheet '%s': %d/%d samples have bad incubation time (%.1f to %.1f hrs). Flagging as NA.",
                    sheet_name, n_bad_time, nrow(df),
                    min(df$incubationTime_raw, na.rm = TRUE),
                    max(df$incubationTime_raw, na.rm = TRUE)))
  }

  # Set defaults for missing values
  df <- df %>%
    mutate(
      times.sampled = replace_na(as.numeric(times.sampled), times_sampled_default),
      soil.volume   = replace_na(as.numeric(soil.volume), 0)
    )

  # IRGA calculation
  df <- df %>%
    mutate(
      measuredCO2      = irga.integral * (standard_co2 / correctedStandard),
      dilutionFactor   = ((5 * times.sampled) / (jar_volume - soil.volume)) + 1,
      concentrationCO2 = measuredCO2 * dilutionFactor,
      volumeCO2        = concentrationCO2 * ((jar_volume - soil.volume) / 1000),
      molesCO2         = (volumeCO2 / 22.414) * 273.15 / 293.15,
      CO2C             = molesCO2 * C_mol,
      CO2CperHour      = CO2C / incubationTime
    )

  # Rename ID column for joining and ensure numeric type
  # Filter out rows where Lab_No is NA (e.g., standard rows without an ID)
  df <- df %>%
    rename(Lab_No = !!sym(id_col)) %>%
    mutate(Lab_No = as.numeric(Lab_No)) %>%
    filter(!is.na(Lab_No))

  # Join with mass data to get plot, replicate, dry mass
  # Try joining on Lab_No directly
  if ("lab.id" %in% names(df) && any(!is.na(df$lab.id))) {
    # Some sheets have a separate lab.id column for joining
    # (e.g., C-min_3 dec16 which has different cmin.id numbering)
    df <- df %>%
      mutate(lab.id = as.numeric(lab.id)) %>%
      filter(!is.na(lab.id)) %>%
      left_join(
        mass_data %>%
          transmute(
            lab_id_join = as.numeric(Lab_No),
            plot = as.integer(Plot),
            replicate = as.character(Replicate),
            fresh_mass = as.numeric(Mass_Soil_g),
            gwc_mass = as.numeric(GWC)
          ) %>%
          filter(!is.na(lab_id_join)),
        by = c("lab.id" = "lab_id_join")
      )
  } else {
    df <- df %>%
      left_join(
        mass_data %>%
          transmute(
            Lab_No = as.numeric(Lab_No),
            plot = as.integer(Plot),
            replicate = as.character(Replicate),
            fresh_mass = as.numeric(Mass_Soil_g),
            gwc_mass = as.numeric(GWC)
          ) %>%
          filter(!is.na(Lab_No)),
        by = "Lab_No"
      )
  }

  # Compute dry mass if soil.dry.mass not available
  df <- df %>%
    mutate(
      soil_dry_mass_irga = suppressWarnings(as.numeric(soil.dry.mass)),
      gwc_frac = if_else(!is.na(gwc_mass) & gwc_mass > 1, gwc_mass / 100, gwc_mass),
      dry_mass_computed = fresh_mass / (1 + gwc_frac),
      dry_mass = coalesce(soil_dry_mass_irga, dry_mass_computed),
      cmin_rate_ug_co2c_hr_g = CO2CperHour / dry_mass,
      date = as.Date(date.flush)
    )

  # Diagnostics
  message(sprintf("  Sheet '%s' diagnostics (IRGA):", sheet_name))
  message(sprintf("    Standards: n=%d, range=[%.1f,%.1f]",
                  nrow(std_rows),
                  if (nrow(std_rows) > 0) min(std_rows$meanStandard) else NA,
                  if (nrow(std_rows) > 0) max(std_rows$meanStandard) else NA))
  message(sprintf("    irga.integral=[%.0f,%.0f], measuredCO2=[%.0f,%.0f] ppm",
                  min(df$irga.integral, na.rm=TRUE), max(df$irga.integral, na.rm=TRUE),
                  min(df$measuredCO2, na.rm=TRUE), max(df$measuredCO2, na.rm=TRUE)))
  message(sprintf("    Incubation hrs=[%.1f,%.1f], dilutionFactor=%.3f",
                  min(df$incubationTime, na.rm=TRUE), max(df$incubationTime, na.rm=TRUE),
                  mean(df$dilutionFactor, na.rm=TRUE)))
  message(sprintf("    Fresh mass=[%.2f,%.2f]g, Dry mass=[%.2f,%.2f]g",
                  min(df$fresh_mass, na.rm=TRUE), max(df$fresh_mass, na.rm=TRUE),
                  min(df$dry_mass, na.rm=TRUE), max(df$dry_mass, na.rm=TRUE)))
  valid_rates <- df$cmin_rate_ug_co2c_hr_g[!is.na(df$cmin_rate_ug_co2c_hr_g)]
  if (length(valid_rates) > 0) {
    message(sprintf("    Rate (ug CO2-C/hr/g)=[%.2f,%.2f], mean=%.2f, n=%d",
                    min(valid_rates), max(valid_rates), mean(valid_rates), length(valid_rates)))
  } else {
    message("    Rate: ALL NA")
  }

  # Flag anomalous values
  df <- df %>%
    mutate(flag = case_when(
      bad_time ~ "bad_incubation_time",
      cmin_rate_ug_co2c_hr_g < 0 ~ "negative_rate",
      is.na(cmin_rate_ug_co2c_hr_g) ~ "NA_value",
      TRUE ~ NA_character_
    ))

  df %>%
    select(Lab_No, plot, replicate, date, cmin_rate_ug_co2c_hr_g, dry_mass, flag)
}


# =============================================================================
# Read mass data files
# =============================================================================
cat("Reading mass data files...\n")

mass1 <- read_excel("data/raw/soil/cmin_mass/C-NMin_Mass_1.xlsx", sheet = "Sheet1") %>%
  mutate(Lab_No = as.numeric(Lab_No), Plot = as.integer(Plot))

mass2 <- read_excel("data/raw/soil/cmin_mass/C-NMin_Mass_2.xlsx", sheet = "Sheet1") %>%
  mutate(Lab_No = as.numeric(Lab_No), Plot = as.integer(Plot)) %>%
  # Rename "Initial GWC" to "GWC" for consistency; it's already a fraction
  rename_with(~ ifelse(.x == "Initial GWC", "GWC", .x))

mass3 <- read_excel("data/raw/soil/cmin_mass/C-NMin_Mass_3.xlsx", sheet = "Sheet1") %>%
  mutate(Lab_No = as.numeric(Lab_No), Plot = as.integer(Plot))

# Mass_3 GWC is broken (formula =sum(H:I)). Replace with GWC from processed data.
mass3_gwc <- gwc %>%
  filter(timepoint == 3) %>%
  group_by(plot) %>%
  summarize(gwc_avg = mean(gwc, na.rm = TRUE), .groups = "drop")

mass3 <- mass3 %>%
  left_join(mass3_gwc, by = c("Plot" = "plot")) %>%
  mutate(GWC = gwc_avg) %>%
  select(-gwc_avg)


# =============================================================================
# C-min_1: LGR method (timepoint 1, 5 sheets)
# =============================================================================
cat("Processing C-min_1 (LGR, 5 sheets)...\n")

cmin1_sheets <- c("june3", "june10", "june17", "june23", "july1")
cmin1_results <- list()

for (sheet in cmin1_sheets) {
  cat(sprintf("  Processing sheet: %s\n", sheet))
  gas <- read_excel("data/raw/soil/cmin/C-min_1.xlsx", sheet = sheet)

  # RESCUE: july1 Lab_No 42 has LGR_time = 05:54 (typo for 17:54)
  # Context: neighboring samples have LGR_time 17:50-17:55; 05:54 AM is clearly wrong
  if (sheet == "july1") {
    lab42_idx <- which(suppressWarnings(as.numeric(as.character(gas$Lab_No))) == 42)
    if (length(lab42_idx) > 0) {
      gas$LGR_time[lab42_idx] <- gas$LGR_time[lab42_idx] + hours(12)
      cat("    RESCUE: Fixed Lab_No 42 LGR_time typo (05:54 -> 17:54)\n")
    }
  }

  result <- calc_cmin_lgr(gas, mass1, sheet)
  if (!is.null(result)) {
    cmin1_results[[sheet]] <- result
  }
}

cmin1 <- bind_rows(cmin1_results) %>%
  mutate(timepoint = 1L, method = "LGR")


# =============================================================================
# C-min_2: IRGA method (timepoint 2, 6 sheets)
# =============================================================================
cat("Processing C-min_2 (IRGA, 6 sheets)...\n")

cmin2_sheets <- c("aug4", "aug8", "aug11", "aug18", "Aug 25", "sep 2")
cmin2_results <- list()

for (i in seq_along(cmin2_sheets)) {
  sheet <- cmin2_sheets[i]
  cat(sprintf("  Processing sheet: %s (times_sampled=%d)\n", sheet, i))
  irga <- read_excel("data/raw/soil/cmin/C-min_2.xlsx", sheet = sheet)

  # RESCUE: aug8 has time.irga and irga.integral, but date.flush/time.flush/date.irga all NA.
  # A separate flushing log (flushing_20250807.xlsx) has per-sample flush times.
  # Flush date = Aug 7, IRGA measurement date = Aug 8 (sheet name).
  if (sheet == "aug8") {
    cat("    RESCUE: Populating missing date/time from flushing_20250807.xlsx\n")
    flush_log <- read_excel("data/raw/soil/cmin/flushing_20250807.xlsx")
    # Extract numeric ID from 'number' column (e.g., "min62" → 62, "mon80" → 80)
    flush_log <- flush_log %>%
      mutate(
        cmin_id = suppressWarnings(as.numeric(gsub("[^0-9]", "", number))),
        flush_time_str = sapply(time.flushed, parse_time_value)
      )
    flush_lookup <- flush_log %>%
      filter(!is.na(cmin_id)) %>%
      select(cmin_id, flush_time_str) %>%
      distinct(cmin_id, .keep_all = TRUE)
    # Map flush times to irga rows by cmin.id
    irga <- irga %>%
      mutate(cmin_id_num = suppressWarnings(as.numeric(as.character(cmin.id)))) %>%
      left_join(flush_lookup, by = c("cmin_id_num" = "cmin_id")) %>%
      mutate(
        date.flush = as.POSIXct("2025-08-07", tz = "UTC"),
        time.flush = flush_time_str,
        date.irga  = as.POSIXct("2025-08-08", tz = "UTC")
      ) %>%
      select(-cmin_id_num, -flush_time_str)
    cat(sprintf("    Matched %d/%d samples with flush times\n",
                sum(!is.na(irga$time.flush)), nrow(irga)))
  }

  # RESCUE: aug18 has date.irga = 2025-08-10 (should be 2025-08-18)
  # All other sheets in C-min_2 show measurement date = flush date + 1 day.
  # aug18: flush = Aug 17, so measurement should be Aug 18 (not Aug 10).
  if (sheet == "aug18") {
    cat("    RESCUE: Correcting date.irga from 2025-08-10 to 2025-08-18\n")
    irga$date.irga <- as.POSIXct("2025-08-18", tz = "UTC")
  }

  result <- calc_cmin_irga(irga, mass2, sheet, times_sampled_default = i)
  if (!is.null(result)) {
    cmin2_results[[sheet]] <- result
  }
}

cmin2 <- bind_rows(cmin2_results) %>%
  mutate(timepoint = 2L, method = "IRGA")


# =============================================================================
# C-min_3: IRGA method (timepoint 3, 6 sheets)
# Note: dec16 has cmin.id values from a different numbering scheme (62-120)
#       with a lab.id column (1-30). This may represent a re-measurement of
#       run 2 samples or a data entry issue.
# TODO: Confirm with Jon about dec16 sheet identity
# =============================================================================
cat("Processing C-min_3 (IRGA, 6 sheets)...\n")

cmin3_sheets <- c("nov26", "nov29", "dec2", "dec9", "dec16", "dec23")
cmin3_results <- list()

for (i in seq_along(cmin3_sheets)) {
  sheet <- cmin3_sheets[i]
  cat(sprintf("  Processing sheet: %s (times_sampled=%d)\n", sheet, i))
  irga <- read_excel("data/raw/soil/cmin/C-min_3.xlsx", sheet = sheet)

  # RESCUE: nov26 has time.irga ALL NA. Three std.start.time anchors bracket
  # the measurement window (~17:00 to ~17:26). Linearly interpolate time.irga.
  # Incubation is ~24 hrs so even rough estimates introduce <2% error.
  if (sheet == "nov26") {
    cat("    RESCUE: Interpolating time.irga from std.start.time anchors\n")
    irga <- irga %>% mutate(.row = row_number())
    # Parse std.start.time anchors to seconds since midnight
    anchor_rows <- irga %>%
      filter(!is.na(std.start.time)) %>%
      mutate(
        std_str = sapply(std.start.time, parse_time_value),
        anchor_secs = sapply(std_str, function(t) {
          parts <- as.numeric(strsplit(t, ":")[[1]])
          parts[1] * 3600 + parts[2] * 60 + if (length(parts) >= 3) parts[3] else 0
        })
      ) %>%
      select(.row, anchor_secs)
    cat(sprintf("    Anchor times at rows: %s\n",
                paste(sprintf("%d (%.0fs)", anchor_rows$.row, anchor_rows$anchor_secs),
                      collapse = ", ")))
    # Linear interpolation between anchors (extrapolate at boundaries)
    interpolated_secs <- approx(
      x = anchor_rows$.row,
      y = anchor_rows$anchor_secs,
      xout = irga$.row,
      rule = 2
    )$y
    irga$time.irga <- sprintf("%02d:%02d:%02d",
                              as.integer(interpolated_secs) %/% 3600,
                              (as.integer(interpolated_secs) %% 3600) %/% 60,
                              as.integer(interpolated_secs) %% 60)
    irga <- irga %>% select(-.row)
    cat(sprintf("    Interpolated time.irga: %s to %s\n",
                irga$time.irga[1], irga$time.irga[nrow(irga)]))
  }

  # RESCUE: dec16 has cmin.id 62-120 with lab.id 1-30 mapping to Mass_3
  # Confirmed: lab.id + 180 = Mass_3 Lab_No (1->181, 2->182, ..., 30->210)
  if (sheet == "dec16") {
    cat("    RESCUE: dec16 lab.id + 180 -> Mass_3 Lab_No mapping\n")
    irga <- irga %>% mutate(lab.id = as.numeric(lab.id) + 180)
    result <- calc_cmin_irga(irga, mass3, sheet, times_sampled_default = i)
  } else {
    result <- calc_cmin_irga(irga, mass3, sheet, times_sampled_default = i)
  }

  if (!is.null(result)) {
    cmin3_results[[sheet]] <- result
  }
}

cmin3 <- bind_rows(cmin3_results) %>%
  mutate(timepoint = 3L, method = "IRGA")


# =============================================================================
# Combine all time-resolved data
# =============================================================================
cat("Combining time-resolved data...\n")

cmin_all <- bind_rows(cmin1, cmin2, cmin3) %>%
  left_join(treatment_key, by = "plot") %>%
  group_by(timepoint) %>%
  mutate(
    first_date = min(date, na.rm = TRUE),
    day = as.numeric(difftime(date, first_date, units = "days"))
  ) %>%
  ungroup() %>%
  select(plot, treatment, timepoint, replicate, date, day,
         cmin_rate_ug_co2c_hr_g, method, flag)

write.csv(cmin_all, "data/processed/cmin_timeresolved.csv", row.names = FALSE)
cat("Wrote data/processed/cmin_timeresolved.csv\n")


# =============================================================================
# Calculate cumulative C mineralization using trapezoidal integration
# =============================================================================
cat("Calculating cumulative C mineralization...\n")

# Only include unflagged data in the AUC
cmin_for_auc <- cmin_all %>%
  filter(is.na(flag)) %>%
  select(plot, treatment, timepoint, replicate, day, cmin_rate_ug_co2c_hr_g, method)

# Create day-0 baseline using backward extrapolation from t1 (first measurement)
# This avoids the bias of setting t0=0 which penalizes high initial rates
# (see lab discussion: t0 = t1 is more appropriate than t0 = 0)
day0 <- cmin_for_auc %>%
  group_by(plot, treatment, timepoint, replicate, method) %>%
  slice_min(day, n = 1) %>%
  ungroup() %>%
  mutate(day = 0)  # keep the t1 rate, set day to 0

cmin_with_baseline <- bind_rows(day0, cmin_for_auc) %>%
  arrange(plot, timepoint, replicate, day) %>%
  # Remove duplicate day-0 entries if the first measurement is already day 0
  distinct(plot, timepoint, replicate, day, .keep_all = TRUE)

# Trapezoidal integration: AUC of rate (ug CO2-C hr-1 g-1) over time (hours)
cmin_cumulative <- cmin_with_baseline %>%
  group_by(plot, treatment, timepoint, replicate, method) %>%
  summarize(
    cumulative_ug_co2c_g = if (n() >= 2) {
      AUC(day * 24, cmin_rate_ug_co2c_hr_g, method = "trapezoid")
    } else {
      NA_real_
    },
    .groups = "drop"
  )

write.csv(cmin_cumulative, "data/processed/cmin_cumulative.csv", row.names = FALSE)
cat("Wrote data/processed/cmin_cumulative.csv\n")
cat(sprintf("  Time-resolved rows: %d\n", nrow(cmin_all)))
cat(sprintf("  Cumulative rows: %d\n", nrow(cmin_cumulative)))
