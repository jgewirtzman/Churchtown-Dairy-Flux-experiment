# helpers.R
# Shared date/time parsing utilities for processing scripts
# Handles mixed Excel formats: POSIXct, serial numbers, fractions, AM/PM strings

parse_excel_date <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (inherits(x, "Date")) return(x)
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  if (!is.na(x_num) && x_num > 30000) {
    return(as.Date(x_num, origin = "1899-12-30"))
  }
  d <- suppressWarnings(as.Date(as.character(x)))
  if (!is.na(d)) return(d)
  return(NA)
}

parse_time_value <- function(x) {
  if (is.na(x) || identical(as.character(x), "NA")) return(NA_character_)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(format(x, "%H:%M:%S"))
  }
  x_str <- trimws(as.character(x))
  # Handle AM/PM strings like "16:59:01 PM"
  if (grepl("PM$", x_str, ignore.case = TRUE)) {
    x_str <- sub("\\s*(AM|PM)$", "", x_str, ignore.case = TRUE)
    parts <- strsplit(x_str, ":")[[1]]
    h <- as.integer(parts[1])
    if (h < 12) h <- h + 12
    parts[1] <- sprintf("%02d", h)
    x_str <- paste(parts, collapse = ":")
    return(x_str)
  } else if (grepl("AM$", x_str, ignore.case = TRUE)) {
    x_str <- sub("\\s*(AM|PM)$", "", x_str, ignore.case = TRUE)
    return(x_str)
  }
  # Numeric: fraction of day or seconds since midnight
  x_num <- suppressWarnings(as.numeric(x_str))
  if (!is.na(x_num)) {
    if (x_num >= 0 && x_num < 1) {
      total_secs <- round(x_num * 86400)
    } else if (x_num >= 1 && x_num < 86400) {
      total_secs <- round(x_num)
    } else {
      return(NA_character_)
    }
    h <- total_secs %/% 3600
    m <- (total_secs %% 3600) %/% 60
    s <- total_secs %% 60
    return(sprintf("%02d:%02d:%02d", h, m, s))
  }
  if (grepl("^\\d{1,2}:\\d{2}", x_str)) return(x_str)
  NA_character_
}
