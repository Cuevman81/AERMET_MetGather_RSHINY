# =============================================================================
# AERMET NOAA Data Downloader  (R Shiny)
# -----------------------------------------------------------------------------
# Gathers the raw inputs needed to build AERMOD-ready met files with the
# MDEQ AERMET pipeline (AERMET.R):
#
#   * ASOS 1-minute & 5-minute data  -> drives AERMINUTE (hourly winds/calms)
#   * GHCNh hourly surface data (.psv) -> AERMET Stage 1 surface observations
#   * IGRA2 radiosonde soundings       -> AERMET Stage 1 upper air (.PFL profile)
#
# 2025/2026 modernization notes
#   * The Integrated Surface Hourly (ISHD / DS3505) archive was retired by NCEI
#     in August 2025.  This app no longer downloads the old gzipped
#     `<USAF>-<WBAN>-YYYY.gz` files; the GHCNh tab replaces it and pulls the same
#     pipe-delimited by-year files (`GHCNh_<id>_<YYYY>.psv`) that AERMET.R uses.
#   * The station list now comes from the modern NCEI ISD-history CSV
#     (`www.ncei.noaa.gov`), not the legacy `www1.ncdc.noaa.gov` fixed-width
#     MASTER-STN-HIST.TXT (that host now 301-redirects).
#   * GHCNh output is named `<ICAO>_GHCNh_<startYr>_<endYr>.psv`, matching the
#     file AERMET.R's `download_ghcnh()` produces, and is screened with the same
#     filter_ghcnh_quality() (plus a short-SYNOP screen), so downloads are drop-in.
#
# Run:  shiny::runApp("ASOS_Met_Gather_Shiny.R")   (or open in RStudio, Run App)
# =============================================================================

library(shiny)
library(httr)
library(leaflet)
library(dplyr)
library(readr)
library(stringr)
library(shinyjs)

# --- Sources -----------------------------------------------------------------
ISD_HISTORY_URL <- "https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv"
LOCAL_STATION_FALLBACK <- "ASOS_Stations.csv"   # bundled offline copy (same schema)

URL_BASE_1MIN <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/"
URL_BASE_5MIN <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/"
GHCNH_BASE    <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"
GHCNH_STATION_LIST_URL <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/doc/ghcnh-station-list.csv"

# Upper air: IGRA2 radiosonde soundings (the data AERMET turns into the .PFL profile)
IGRA_STATION_URL <- "https://www.ncei.noaa.gov/pub/data/igra/igra2-station-list.txt"
IGRA_POR_BASE    <- "https://www.ncei.noaa.gov/pub/data/igra/data/data-por"

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

# =============================================================================
# Station list: parse the NCEI ISD-history CSV (falls back to bundled copy)
# =============================================================================
# Columns: USAF, WBAN, "STATION NAME", CTRY, STATE, ICAO, LAT, LON, "ELEV(M)",
#          BEGIN, END  (END is yyyymmdd; active ASOS now show 2025-08-27, the
#          ISHD retirement date, rather than the old 99991231 sentinel).
# isd-history.csv stopped updating when ISHD was retired, so "recently active" is
# measured against the file's own newest END year, not today's date (which would
# empty the list on 1 Jan 2028, and already empties the older bundled copy).
parse_isd_history <- function(raw_csv) {
  df <- suppressWarnings(readr::read_csv(
    raw_csv,
    col_types = readr::cols(.default = "c"),
    progress = FALSE, show_col_types = FALSE
  ))
  names(df) <- toupper(gsub("[^A-Za-z0-9]", "_", names(df)))  # STATION_NAME, ELEV_M_, ...
  has_name <- "STATION_NAME" %in% names(df)

  df <- df %>%
    mutate(
      ICAO  = str_trim(ICAO),
      WBAN  = str_trim(WBAN),
      USAF  = str_trim(USAF),
      STATE = str_trim(STATE),
      CTRY  = str_trim(CTRY),
      LAT   = suppressWarnings(as.numeric(LAT)),
      LON   = suppressWarnings(as.numeric(LON)),
      END_YR = suppressWarnings(as.integer(substr(END, 1, 4))),
      STATION_NAME = if (has_name) str_trim(STATION_NAME) else ""
    )
  ref_yr <- suppressWarnings(max(df$END_YR, na.rm = TRUE))   # the file's own vintage
  df <- df %>%
    filter(
      CTRY == "US",
      !is.na(ICAO), ICAO != "", nchar(ICAO) == 4,
      !is.na(WBAN), WBAN != "", WBAN != "99999",
      !is.na(STATE), STATE != "",
      !is.na(LAT), !is.na(LON), !(LAT == 0 & LON == 0),
      !is.na(END_YR), END_YR >= (ref_yr - 1)   # recently active ASOS
    )

  # Keep one record per ICAO: the most recently active
  df %>%
    arrange(ICAO, desc(END_YR)) %>%
    distinct(ICAO, .keep_all = TRUE) %>%
    transmute(
      ICAO,
      WBAN_ID = str_pad(WBAN, 5, "left", "0"),
      USAF_ID = USAF,
      GHCNH_ID = paste0("USW000", str_pad(WBAN, 5, "left", "0")),
      STATE,
      STATION_NAME,
      LAT, LON
    ) %>%
    arrange(STATE, ICAO)
}

fetch_and_parse_stations <- function() {
  # 1) try the live NCEI CSV
  resp <- tryCatch(httr::GET(ISD_HISTORY_URL, httr::timeout(60)), error = function(e) NULL)
  if (!is.null(resp) && httr::status_code(resp) == 200) {
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    out <- tryCatch(parse_isd_history(txt), error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) return(out)
  }
  # 2) fall back to the bundled CSV shipped with the app
  if (file.exists(LOCAL_STATION_FALLBACK)) {
    showNotification("Live NCEI list unavailable - using bundled ASOS_Stations.csv.",
                     type = "warning", duration = 8)
    out <- tryCatch(parse_isd_history(readr::read_file(LOCAL_STATION_FALLBACK)),
                    error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) return(out)
  }
  showNotification("Could not load a station list from NCEI or the local fallback.",
                   type = "error", duration = NULL)
  EMPTY_STATIONS
}

EMPTY_STATIONS <- tibble(ICAO = character(), WBAN_ID = character(), USAF_ID = character(),
                         GHCNH_ID = character(), STATE = character(),
                         STATION_NAME = character(), LAT = numeric(), LON = numeric())

# =============================================================================
# Upper-air station list: parse the IGRA2 fixed-width station inventory
# =============================================================================
# Documented IGRA2 station-list layout (note the gaps between fields):
#   IGRA_ID 1-11  LAT 13-20  LON 22-30  ELEV 32-37  STATE 39-40
#   NAME 42-71  FIRST_YEAR 73-76  LAST_YEAR 78-81  NUM_RECORDS 83-88
EMPTY_IGRA <- tibble(IGRA_ID = character(), STATE = character(),
                     STATION_NAME = character(), LAT = numeric(), LON = numeric(),
                     FIRST_YEAR = integer(), LAST_YEAR = integer())

IGRA_FWF <- readr::fwf_positions(
  start = c(1, 13, 22, 32, 39, 42, 73, 78, 83),
  end   = c(11, 20, 30, 37, 40, 71, 76, 81, 88),
  col_names = c("IGRA_ID", "LAT", "LON", "ELEV", "STATE", "STATION_NAME",
                "FIRST_YEAR", "LAST_YEAR", "NUM_RECORDS"))

fetch_igra_stations <- function() {
  resp <- tryCatch(httr::GET(IGRA_STATION_URL, httr::timeout(60)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    showNotification("Could not fetch the IGRA upper-air station list from NCEI.",
                     type = "error", duration = NULL)
    return(EMPTY_IGRA)
  }
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  df <- tryCatch(
    readr::read_fwf(I(txt), IGRA_FWF, col_types = readr::cols(.default = "c"),
                    progress = FALSE),
    error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(EMPTY_IGRA)

  df %>%
    mutate(
      IGRA_ID = str_trim(IGRA_ID), STATE = str_trim(STATE),
      STATION_NAME = str_trim(STATION_NAME),
      LAT = suppressWarnings(as.numeric(LAT)),
      LON = suppressWarnings(as.numeric(LON)),
      FIRST_YEAR = suppressWarnings(as.integer(FIRST_YEAR)),
      LAST_YEAR  = suppressWarnings(as.integer(LAST_YEAR))
    ) %>%
    filter(
      substr(IGRA_ID, 1, 2) == "US",          # US soundings
      !is.na(STATE), STATE != "",
      !is.na(LAT), !is.na(LON),
      !is.na(LAST_YEAR)        # the Upper Air tab lists the sites active in its years
    ) %>%
    transmute(IGRA_ID, STATE, STATION_NAME, LAT, LON, FIRST_YEAR, LAST_YEAR) %>%
    arrange(STATE, IGRA_ID)
}

# =============================================================================
# GHCNh quality screen -- verbatim copy of AERMET.R's filter_ghcnh_quality()
# (same block as aermet-runner backend/engine.R). Keep it verbatim so a fix made
# in AERMET.R can be copied here unchanged.
# =============================================================================
# ------------------------------ GHCNh quality control --------------------------------
#
# AERMET reads the GHCNh .psv as delivered and does not honour NCEI's per-element
# quality flags, so observations NCEI itself marked "suspect" or "erroneous" reach the
# .sfc verbatim.  At KMEI that put 50 hours of 30.16 m/s into April-May 2025 (every one
# of them qc=2 on a 3-hourly FM12 SYNOP report); KTUP 2025 carried 25 more.
#
# Three independent screens are applied.  All only ever blank a value -- the element
# becomes missing for that observation and AERMET falls back to AERMINUTE or to its own
# substitution logic.  No record is dropped and no value is altered or invented, so the
# edit stays defensible and is fully auditable from the log written beside the file.
#
#   1. Quality codes.  ISD/GHCNh codes 2 and 6 mean "suspect", 3 and 7 "erroneous";
#      0/1/4/5/9 and blank pass.  Applied to every element carrying a *_Quality_Code.
#
#   2. Wind-speed cross-check.  NCEI carries the verbatim METAR/SPECI text in REM, so
#      the decoded wind_speed can be checked against the report it came from.  KMEI
#      2025-05-01 19:55Z and 19:58Z both decode to 54.1 m/s from a METAR that plainly
#      reads 25010KT (5.1 m/s), and NCEI flags one of them qc=5 "passed all checks" --
#      a decoding error the quality flags miss entirely.  Wind direction was checked
#      the same way across 821,424 METAR groups with zero disagreement, so only speed
#      is screened.
#
#   3. Short SYNOPs.  Some ASOS sites also send short FM-12 SYNOPs that report cloud
#      base and visibility missing (iRixhVV = "xx///") and leave the Nddff group out.
#      NCEI's decoder then reads the next group as "cloud, direction, speed": the
#      report's own time group at KMEI (90558 -> 58 kt from 050), station pressure at
#      KTUP (30036 -> 36 kt "from 360"), temperature or present weather elsewhere --
#      and also takes that group's first digit as the total sky cover.  When the wind
#      group is really there its N is "/" (no cloud measured), so the misread group is
#      recognisable: it starts with a digit, and it and the groups after it carry
#      strictly increasing section-1 indicators (1snTTT 2snTdTdTd 3PoPoPoPo ... 9GGgg).
#      Wind direction, wind speed, sky_condition and ceiling_height -- everything NCEI
#      decoded from that group -- are blanked.  On the 18 MDEQ stations 2021-2025 this
#      matched 110 reports (KJAN 4, KMEI 55, KMOB 6, KTUP 45), none with a METAR that
#      agreed; the 5 short SYNOPs that did carry a wind group ("/ddff") all matched
#      their METAR and are left alone.
#
# The function is idempotent: a blanked value cannot be blanked twice, so re-running
# the pipeline over an already-filtered .psv is a no-op.

GHCNH_BAD_QC <- c("2", "3", "6", "7")   # suspect (2,6) and erroneous (3,7)
GHCNH_WS_TOL <- 5                       # m/s; decoded-vs-METAR tolerance
GHCNH_KT     <- 0.514444

ghcnh_isopen <- function(cc) tryCatch(isOpen(cc), error = function(e) FALSE)

# knots from the wind group of a METAR/SPECI report ("25010KT", "VRB03G15KT", ...)
metar_wind_kt <- function(rem) {
  m <- regmatches(rem, regexpr("\\b(\\d{3}|VRB)\\d{2,3}(G\\d{2,3})?KT\\b", rem))
  if (!length(m)) return(NA_real_)
  suppressWarnings(as.numeric(sub("^(\\d{3}|VRB)(\\d{2,3}).*$", "\\2", m)))
}

# The group NCEI decoded as the wind (Nddff) in a short FM-12 SYNOP that has no wind
# group, or "" for every other record.  REM carries "SYN" + 3-digit length + the report.
ghcnh_synop_misread <- function(rem) {
  out <- character(length(rem))
  syn <- which(startsWith(rem, "SYN"))
  if (!length(syn)) return(out)
  tk <- strsplit(trimws(sub("^SYN\\d{3}", "", rem[syn])), " +")
  out[syn] <- vapply(tk, function(g) {
    g <- sub("=$", "", g)                       # IIiii iRixhVV <group read as Nddff> ...
    if (length(g) < 3 || !grepl("^[0-9/]{2}///$", g[2]) || !grepl("^[1-9][0-9/]{4}$", g[3]))
      return("")
    s1 <- g[-(1:2)]
    end <- which(grepl("^(222[0-9/]{2}|333|444|555)$", s1))   # next section starts
    if (length(end)) s1 <- s1[seq_len(end[1] - 1L)]
    ind <- suppressWarnings(as.integer(substr(s1, 1, 1)))
    if (!all(nchar(s1) == 5L) || anyNA(ind) || any(diff(ind) <= 0)) return("")
    g[3]
  }, character(1), USE.NAMES = FALSE)
  out
}

filter_ghcnh_quality <- function(psv_file, log_file = NULL, chunk = 20000L,
                                 verbose = TRUE) {
  if (!file.exists(psv_file) || file.size(psv_file) == 0)
    stop("GHCNh file not found: ", psv_file)
  if (is.null(log_file))
    log_file <- sub("\\.psv$", "_qc_log.txt", psv_file)

  con <- file(psv_file, "r"); out <- NULL
  on.exit({
    for (cc in list(con, out))
      if (!is.null(cc) && inherits(cc, "connection") && ghcnh_isopen(cc))
        try(close(cc), silent = TRUE)
  }, add = TRUE)

  header <- readLines(con, n = 1L, warn = FALSE)
  cols   <- strsplit(header, "|", fixed = TRUE)[[1]]
  ncol   <- length(cols)

  qc_idx  <- grep("_Quality_Code$", cols); qc_idx <- qc_idx[qc_idx > 2L]
  val_idx <- qc_idx - 2L            # layout: value, Measurement_Code, Quality_Code, ...
  elem    <- sub("_Quality_Code$", "", cols[qc_idx])
  keep    <- cols[val_idx] == elem  # only trust the pairing where it really lines up
  qc_idx  <- qc_idx[keep]; val_idx <- val_idx[keep]; elem <- elem[keep]

  i_ws  <- match("wind_speed", cols)
  i_rem <- match("REM", cols)
  i_t   <- match("DATE", cols); if (is.na(i_t)) i_t <- 3L
  i_syn <- match(c("wind_direction", "wind_speed", "sky_condition", "ceiling_height"), cols)
  i_syn <- i_syn[!is.na(i_syn)]      # what NCEI decodes from a SYNOP's Nddff slot

  tmp <- paste0(psv_file, ".qctmp")
  out <- file(tmp, "w")
  writeLines(header, out)

  counts <- setNames(integer(length(elem)), elem)
  n_ws_x <- 0L; n_syn <- 0L
  audit  <- list(); xaudit <- list(); saudit <- list()
  nrec   <- 0L

  repeat {
    lines <- readLines(con, n = chunk, warn = FALSE)
    if (!length(lines)) break
    nrec <- nrec + length(lines)

    f   <- strsplit(lines, "|", fixed = TRUE)
    len <- lengths(f)
    if (any(len < ncol))
      f[len < ncol] <- lapply(f[len < ncol], function(v) c(v, rep("", ncol - length(v))))
    m <- matrix(unlist(f, use.names = FALSE), nrow = length(f), byrow = TRUE)

    # --- screen 1: NCEI quality codes ---
    for (j in seq_along(qc_idx)) {
      bad <- m[, qc_idx[j]] %in% GHCNH_BAD_QC & nzchar(m[, val_idx[j]])
      if (!any(bad)) next
      counts[j] <- counts[j] + sum(bad)
      audit[[length(audit) + 1L]] <- data.frame(
        timestamp = m[bad, i_t], element = elem[j],
        value = m[bad, val_idx[j]], qc = m[bad, qc_idx[j]], stringsAsFactors = FALSE)
      m[bad, val_idx[j]] <- ""
    }

    # --- screen 2: decoded wind speed vs the METAR it came from ---
    if (!is.na(i_ws) && !is.na(i_rem)) {
      w <- suppressWarnings(as.numeric(m[, i_ws]))
      cand <- which(!is.na(w) & nzchar(m[, i_rem]))
      if (length(cand)) {
        kt <- vapply(m[cand, i_rem], metar_wind_kt, numeric(1), USE.NAMES = FALSE)
        mw <- kt * GHCNH_KT
        off <- which(!is.na(mw) & abs(mw - w[cand]) > GHCNH_WS_TOL)
        if (length(off)) {
          r <- cand[off]
          n_ws_x <- n_ws_x + length(r)
          xaudit[[length(xaudit) + 1L]] <- data.frame(
            timestamp = m[r, i_t], decoded = m[r, i_ws],
            metar = sprintf("%.1f", mw[off]), rem_kt = sprintf("%g", kt[off]),
            stringsAsFactors = FALSE)
          m[r, i_ws] <- ""
        }
      }
    }

    # --- screen 3: short SYNOPs whose wind slot holds another group ---
    if (!is.na(i_rem) && length(i_syn)) {
      grp <- ghcnh_synop_misread(m[, i_rem])
      r <- which(nzchar(grp))
      if (length(r)) r <- r[rowSums(m[r, i_syn, drop = FALSE] != "") > 0]
      if (length(r)) {
        n_syn <- n_syn + length(r)
        saudit[[length(saudit) + 1L]] <- data.frame(
          timestamp = m[r, i_t], group = grp[r],
          values = apply(m[r, i_syn, drop = FALSE], 1L, paste, collapse = " / "),
          stringsAsFactors = FALSE)
        m[r, i_syn] <- ""
      }
    }

    writeLines(apply(m, 1L, paste, collapse = "|"), out)
  }

  close(out); out <- NULL
  close(con); con <- NULL
  if (!file.rename(tmp, psv_file)) { unlink(tmp); stop("could not replace ", psv_file) }

  hit <- counts[counts > 0]
  lg <- c(sprintf("GHCNh quality-control filter log -- %s", basename(psv_file)),
          sprintf("Applied: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "",
          "Rejected values are blanked so AERMET treats the element as missing for that",
          "observation.  No record is dropped and no value is altered or substituted.",
          "",
          sprintf("Records scanned        : %d", nrec),
          sprintf("Screen 1 (NCEI flags)  : %d values rejected", sum(counts)),
          sprintf("Screen 2 (METAR check) : %d wind speeds rejected", n_ws_x),
          sprintf("Screen 3 (short SYNOP) : %d reports' wind/sky values rejected", n_syn), "",
          "SCREEN 1 -- NCEI quality codes 2/6 (suspect) and 3/7 (erroneous)")
  if (length(hit)) {
    aud <- do.call(rbind, audit)
    lg <- c(lg, sprintf("   %-30s %6d", names(hit), hit), "",
            "   Detail (timestamp | element | rejected value | quality code):",
            sprintf("   %s | %s | %s | %s", aud$timestamp, aud$element, aud$value, aud$qc))
  } else lg <- c(lg, "   none")
  lg <- c(lg, "",
          sprintf("SCREEN 2 -- decoded wind_speed vs METAR text (tolerance %g m/s)",
                  GHCNH_WS_TOL))
  if (n_ws_x) {
    xa <- do.call(rbind, xaudit)
    lg <- c(lg, "   Detail (timestamp | decoded m/s | METAR m/s | METAR kt):",
            sprintf("   %s | %s | %s | %s", xa$timestamp, xa$decoded, xa$metar, xa$rem_kt))
  } else lg <- c(lg, "   none")
  lg <- c(lg, "",
          "SCREEN 3 -- short FM-12 SYNOPs with no wind group (NCEI decoded another group as Nddff)")
  if (n_syn) {
    sa <- do.call(rbind, saudit)
    lg <- c(lg, paste0("   Detail (timestamp | group read as wind | rejected ",
                       paste(cols[i_syn], collapse = " / "), "):"),
            sprintf("   %s | %s | %s", sa$timestamp, sa$group, sa$values))
  } else lg <- c(lg, "   none")

  # The log is the audit trail for values that are no longer present in the .psv, so
  # it has to survive a re-run.  Re-processing an already-screened file rejects
  # nothing; leave the existing log as it stands rather than overwriting it with
  # zeroes and destroying the record of the first pass.
  if (sum(counts) == 0 && n_ws_x == 0 && n_syn == 0 && file.exists(log_file)) {
    if (verbose)
      cat(sprintf("QC filter: %s already screened; existing log left intact\n",
                  basename(psv_file)))
    return(invisible(list(records = nrec, rejected = 0L, ws_crosscheck = 0L,
                          synop_misread = 0L, by_element = integer(0), log_file = log_file)))
  }
  # A pass that rejects something new over an already-screened file (e.g. after a new
  # screen is added) appends to the log, so the first pass's record is kept.  Delete
  # the log together with the .psv when the .psv itself is replaced.
  if (file.exists(log_file))
    lg <- c(readLines(log_file, warn = FALSE), "", strrep("=", 78),
            "RE-SCREEN of the already-screened file: this pass blanked the values below", lg)
  writeLines(lg, log_file)

  if (verbose)
    cat(sprintf("QC filter: %s -- %d records, %d flagged + %d METAR-mismatch + %d short-SYNOP rejected\n",
                basename(psv_file), nrec, sum(counts), n_ws_x, n_syn))
  invisible(list(records = nrec, rejected = sum(counts), ws_crosscheck = n_ws_x,
                 synop_misread = n_syn, by_element = hit, log_file = log_file))
}

# =============================================================================
# Clear helpers
# =============================================================================
clear_asos_data <- function(icao, y1, y2) {
  cleared <- FALSE
  for (yr in y1:y2) {
    yr_dir <- file.path(icao, as.character(yr))
    for (sub in c("asos_data_1min", "asos_data_5min")) {
      d <- file.path(yr_dir, sub)
      if (dir.exists(d)) { unlink(d, recursive = TRUE, force = TRUE); cleared <- TRUE }
    }
    if (dir.exists(yr_dir) &&
        length(list.files(yr_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(yr_dir, recursive = TRUE, force = TRUE)
    }
  }
  if (dir.exists(icao) &&
      length(list.files(icao, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(icao, recursive = TRUE, force = TRUE)
  }
  if (cleared) paste("Cleared ASOS data for", icao, "years", y1, "to", y2)
  else paste("No ASOS data found to clear for", icao, "years", y1, "to", y2)
}

clear_ghcnh_data <- function(icao) {
  d <- file.path(icao, "ghcnh_data")
  if (dir.exists(d)) {
    unlink(d, recursive = TRUE, force = TRUE)
    if (dir.exists(icao) &&
        length(list.files(icao, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(icao, recursive = TRUE, force = TRUE)
    }
    paste("Cleared GHCNh data for", icao)
  } else {
    paste("No GHCNh data directory found for", icao)
  }
}

clear_ua_data <- function(igra_id) {
  files <- list.files("upper_air", pattern = paste0("^", igra_id, "_UA_.*\\.txt$"),
                      full.names = TRUE)
  if (length(files)) {
    file.remove(files)
    if (dir.exists("upper_air") && length(list.files("upper_air")) == 0)
      unlink("upper_air", recursive = TRUE, force = TRUE)
    paste("Cleared", length(files), "upper-air file(s) for", igra_id)
  } else {
    paste("No upper-air data found for", igra_id)
  }
}

# =============================================================================
# Download helpers
# =============================================================================
# GET with one retry.  A 404 is NCEI saying it has no such file -- an answer, not a
# failure -- so it is not retried and is reported separately from timeouts and other
# errors.  Returns list(ok, status, reason, resp); status is NA when nothing came back.
http_get <- function(url, timeout_s, dest = NULL, tries = 2L) {
  status <- NA_integer_; reason <- ""
  for (k in seq_len(tries)) {
    r <- tryCatch(
      if (is.null(dest)) GET(url, timeout(timeout_s))
      else GET(url, timeout(timeout_s), write_disk(dest, overwrite = TRUE)),
      error = function(e) e)
    if (inherits(r, "error")) {
      status <- NA_integer_; reason <- sub(":.*$", "", conditionMessage(r))
    } else {
      status <- status_code(r)
      if (status == 200) return(list(ok = TRUE, status = 200L, reason = "", resp = r))
      reason <- paste("HTTP", status)
      if (status == 404) break
    }
  }
  list(ok = FALSE, status = status, reason = reason, resp = NULL)
}

# GHCNh id for an ASOS station.  US airports are USW000+WBAN, but Puerto Rico and the
# Virgin Islands use RQW/VQW prefixes and a few sites have ICAO-based ids (KLNN is
# USI0000KLNN), so when USW000+WBAN is not in NCEI's GHCNh station list (fetched once
# per session) the ICAO is looked up there instead.  Keeps USW000+WBAN when it is
# listed, when the list can't be fetched, or when there is no single better match.
ghcnh_list_cache <- new.env()
resolve_ghcnh_id <- function(icao, wban, default_id) {
  if (is.null(ghcnh_list_cache$df)) {
    g <- http_get(GHCNH_STATION_LIST_URL, 60)
    if (g$ok) ghcnh_list_cache$df <- tryCatch(
      readr::read_csv(I(content(g$resp, "text", encoding = "UTF-8")),
                      col_types = readr::cols(.default = "c"), progress = FALSE,
                      show_col_types = FALSE),
      error = function(e) NULL)
  }
  df <- ghcnh_list_cache$df
  if (is.null(df) || !all(c("GHCN_ID", "ICAO") %in% names(df))) return(default_id)
  if (default_id %in% df$GHCN_ID) return(default_id)   # USW000+WBAN exists: use it
  ids <- df$GHCN_ID[!is.na(df$ICAO) & df$ICAO == icao]
  if (!length(ids)) return(default_id)
  same_wban <- ids[endsWith(ids, wban)]              # e.g. TJSJ -> RQW00011641
  if (length(same_wban) == 1) return(same_wban)
  if (length(ids) == 1) return(ids)
  default_id
}

# c("1-min 2024-12", "5-min 2023-01", ...) -> one entry per month, or
# "5-min 2023 (all 12 months)" when a whole year is missing.
compact_months <- function(x) {
  key <- sub("-\\d\\d$", "", x)
  unlist(lapply(unique(key), function(k) {
    m <- x[key == k]; if (length(m) == 12) paste0(k, " (all 12 months)") else m
  }), use.names = FALSE)
}

# =============================================================================
# UI modules
# =============================================================================
station_map <- function(ns, map_id) leafletOutput(ns(map_id), height = "300px")

asos_ui <- function(id) {
  ns <- NS(id)
  fluidPage(sidebarLayout(
    sidebarPanel(
      helpText("1-minute & 5-minute ASOS observations (feed AERMINUTE for hourly winds/calms)."),
      selectInput(ns("state"), "Filter by State", choices = NULL, selectize = FALSE),
      station_map(ns, "map"),
      selectizeInput(ns("station"), "Select ASOS Station (ICAO)", choices = NULL),
      sliderInput(ns("y1"), "Start Year", min = 2000, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 5, step = 1, sep = ""),
      sliderInput(ns("y2"), "End Year", min = 2000, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 1, step = 1, sep = ""),
      actionButton(ns("download"), "Download ASOS Data", class = "btn-primary"),
      actionButton(ns("clear"), "Clear Downloaded ASOS Data", class = "btn-danger")
    ),
    mainPanel(verbatimTextOutput(ns("status")))
  ))
}

ghcnh_ui <- function(id) {
  ns <- NS(id)
  fluidPage(sidebarLayout(
    sidebarPanel(
      helpText("GHCNh hourly surface data (.psv) - the NCEI replacement for the retired ",
               "ISHD/DS3505 archive. Output feeds AERMET Stage 1."),
      selectInput(ns("state"), "Filter by State", choices = NULL, selectize = FALSE),
      station_map(ns, "map"),
      selectizeInput(ns("station"), "Select Station (ICAO)", choices = NULL),
      sliderInput(ns("y1"), "Start Year", min = 2000, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 5, step = 1, sep = ""),
      sliderInput(ns("y2"), "End Year", min = 2000, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 1, step = 1, sep = ""),
      actionButton(ns("download"), "Download GHCNh Data (.psv)", class = "btn-primary"),
      actionButton(ns("clear"), "Clear Downloaded GHCNh Data", class = "btn-danger")
    ),
    mainPanel(verbatimTextOutput(ns("status")))
  ))
}

ua_ui <- function(id) {
  ns <- NS(id)
  fluidPage(sidebarLayout(
    sidebarPanel(
      helpText("Upper-air radiosonde soundings (IGRA2) - the data AERMET uses to build ",
               "the profile (.PFL). The full period-of-record is downloaded (can be ",
               "~100 MB) and trimmed to the selected years."),
      selectInput(ns("state"), "Filter by State", choices = NULL, selectize = FALSE),
      station_map(ns, "map"),
      selectizeInput(ns("station"), "Select Sounding Station (IGRA ID)", choices = NULL),
      sliderInput(ns("y1"), "Start Year", min = 1960, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 5, step = 1, sep = ""),
      sliderInput(ns("y2"), "End Year", min = 1960, max = CURRENT_YEAR,
                  value = CURRENT_YEAR - 1, step = 1, sep = ""),
      actionButton(ns("download"), "Download Upper Air Data", class = "btn-primary"),
      actionButton(ns("clear"), "Clear Downloaded Upper Air Data", class = "btn-danger")
    ),
    mainPanel(verbatimTextOutput(ns("status")))
  ))
}

# =============================================================================
# Shared server helper: state filter + station dropdown + leaflet map
# =============================================================================
wire_state_and_map <- function(input, output, session, stations, map_id,
                               id_col = "ICAO", name_col = "STATION_NAME") {
  states <- reactive({
    df <- stations(); if (nrow(df)) sort(unique(df$STATE)) else character(0)
  })
  observe({
    ch <- states(); cur <- isolate(input$state)   # keep the user's pick if still listed
    sel <- if (!is.null(cur) && cur %in% ch) cur else if ("MS" %in% ch) "MS" else if (length(ch)) ch[1] else NULL
    updateSelectInput(session, "state", choices = ch, selected = sel)
  })
  in_state <- reactive({
    req(stations(), input$state)
    filter(stations(), STATE == input$state)
  })
  observe({
    df <- in_state()
    choices <- if (nrow(df)) setNames(df[[id_col]],
                 paste0(df[[id_col]], " - ", df[[name_col]])) else character(0)
    cur <- isolate(input$station)
    updateSelectizeInput(session, "station", choices = choices,
                         selected = if (!is.null(cur) && cur %in% choices) cur
                                    else if (length(choices)) choices[[1]] else NULL, server = TRUE)
  })
  output[[map_id]] <- renderLeaflet({
    df <- in_state()
    if (!nrow(df))
      return(leaflet() %>% addTiles() %>% setView(-98.583, 39.833, zoom = 3))
    leaflet(df) %>% addTiles() %>%
      addMarkers(lng = df$LON, lat = df$LAT,
                 label = paste0(df[[id_col]], " - ", df[[name_col]]),
                 layerId = df[[id_col]]) %>%
      setView(mean(df$LON), mean(df$LAT), zoom = 6)
  })
  observeEvent(input[[paste0(map_id, "_marker_click")]], {
    ev <- input[[paste0(map_id, "_marker_click")]]
    req(ev$id); updateSelectizeInput(session, "station", selected = ev$id)
  })
  in_state
}

# =============================================================================
# ASOS server
# =============================================================================
asos_server <- function(id, stations) {
  moduleServer(id, function(input, output, session) {
    wire_state_and_map(input, output, session, stations, "map")

    observeEvent(input$download, {
      req(input$station, input$station != "")
      icao <- input$station; y1 <- input$y1; y2 <- min(input$y2, CURRENT_YEAR)
      if (y2 < y1) { output$status <- renderText("End year must be >= start year."); return() }
      log <- c(paste0("ASOS download: ", icao, "  ", y1, "-", y2), "")
      output$status <- renderText(paste(log, collapse = "\n"))

      total <- (y2 - y1 + 1) * 12 * 2; ok1 <- 0; ok5 <- 0
      not_at_ncei <- character(0); failed <- character(0)   # a missing month = more calms
      note_miss <- function(r, what) {
        if (isTRUE(r$status == 404)) not_at_ncei <<- c(not_at_ncei, what)
        else failed <<- c(failed, sprintf("%s (%s)", what, r$reason))
      }
      withProgress(message = paste("Downloading ASOS", icao), value = 0, {
        for (yr in y1:y2) {
          d1 <- file.path(icao, yr, "asos_data_1min")
          d5 <- file.path(icao, yr, "asos_data_5min")
          dir.create(d1, recursive = TRUE, showWarnings = FALSE)
          dir.create(d5, recursive = TRUE, showWarnings = FALSE)
          for (mo in 1:12) {
            mm <- sprintf("%02d", mo)
            incProgress(1/total, detail = paste("1-min", yr, mm))
            u1 <- paste0(URL_BASE_1MIN, yr, "/", mm, "/asos-1min-pg1-", icao, "-", yr, mm, ".dat")
            r1 <- http_get(u1, 60)
            if (r1$ok) {
              writeBin(content(r1$resp, "raw"), file.path(d1, paste0(icao, "_", yr, mm, "_1min.dat"))); ok1 <- ok1 + 1
            } else note_miss(r1, paste0("1-min ", yr, "-", mm))
            incProgress(1/total, detail = paste("5-min", yr, mm))
            u5 <- paste0(URL_BASE_5MIN, yr, "/", mm, "/asos-5min-", icao, "-", yr, mm, ".dat")
            r5 <- http_get(u5, 60)
            if (r5$ok) {
              writeBin(content(r5$resp, "raw"), file.path(d5, paste0(icao, "_", yr, mm, "_5min.dat"))); ok5 <- ok5 + 1
            } else note_miss(r5, paste0("5-min ", yr, "-", mm))
          }
        }
      })
      log <- c(log,
               paste0("Saved ", ok1, " one-minute and ", ok5, " five-minute monthly files."),
               paste0("Location: ", normalizePath(icao, mustWork = FALSE)),
               if (length(not_at_ncei))
                 paste0("Not at NCEI (HTTP 404): ", paste(compact_months(not_at_ncei), collapse = ", ")) else NULL,
               if (length(failed)) paste0("FAILED, download again: ", paste(failed, collapse = ", ")) else NULL,
               if (length(failed)) "Incomplete."
               else if (ok1 + ok5 == 0) "No files returned - check ICAO/years (some sites lack 1-min data)."
               else "Done.")
      output$status <- renderText(paste(log, collapse = "\n"))
    })

    observeEvent(input$clear, {
      req(input$station, input$station != "")
      output$status <- renderText(clear_asos_data(input$station, input$y1, input$y2))
    })
  })
}

# =============================================================================
# GHCNh server  (replaces the retired ISH/DS3505 downloader)
# =============================================================================
ghcnh_server <- function(id, stations) {
  moduleServer(id, function(input, output, session) {
    wire_state_and_map(input, output, session, stations, "map")

    observeEvent(input$download, {
      req(input$station, input$station != "")
      icao <- input$station; y1 <- input$y1; y2 <- min(input$y2, CURRENT_YEAR)
      if (y2 < y1) { output$status <- renderText("End year must be >= start year."); return() }

      info <- filter(stations(), ICAO == icao) %>% slice(1)
      if (!nrow(info) || is.na(info$GHCNH_ID) || info$GHCNH_ID == "") {
        output$status <- renderText(paste("No GHCNh id (USW000+WBAN) available for", icao)); return()
      }
      ghcn_id <- resolve_ghcnh_id(icao, info$WBAN_ID, info$GHCNH_ID)
      out_dir <- file.path(icao, "ghcnh_data")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_file <- file.path(out_dir, sprintf("%s_GHCNh_%d_%d.psv", icao, y1, y2))
      qc_log   <- sub("\\.psv$", "_qc_log.txt", out_file)
      if (file.exists(out_file)) file.remove(out_file)
      if (file.exists(qc_log)) file.remove(qc_log)     # belongs to the file just removed

      log <- c(paste0("GHCNh download: ", icao, " (", ghcn_id, ")  ", y1, "-", y2),
               if (ghcn_id != info$GHCNH_ID)
                 paste0("  id from NCEI's GHCNh station list (", info$GHCNH_ID, " does not apply here)") else NULL,
               "")
      output$status <- renderText(paste(log, collapse = "\n"))

      years <- y1:y2; got <- character(0); no_file <- character(0); failed <- character(0)
      header <- NULL
      fetch_year <- function(yr) {
        url <- sprintf("%s/%d/psv/GHCNh_%s_%d.psv", GHCNH_BASE, yr, ghcn_id, yr)
        tmp <- file.path(out_dir, sprintf("_tmp_%d.psv", yr))
        on.exit(if (file.exists(tmp)) file.remove(tmp))
        g <- http_get(url, 300, dest = tmp)
        if (g$ok && !(file.exists(tmp) && file.info(tmp)$size > 1000)) {
          g$ok <- FALSE; g$reason <- "empty or truncated file"
        }
        if (g$ok) {
          g$lines <- readLines(tmp, warn = FALSE)
          if (!is.null(header) && g$lines[1] != header) {
            g$ok <- FALSE; g$reason <- paste("column layout differs from", got[1])
          }
        }
        g
      }
      # GHCNh by-year files hold UTC years and AERMET subtracts tadjust to reach local
      # standard time, so the evening of 31 Dec of the last year -- and the whole of
      # (y2+1)/01/01, where AERMET.R's XDATES end -- sit in the next year's file.
      # Records up to (y2+1)-01-02 12:00 UTC cover that day for every US time zone.
      tail_yr <- y2 + 1L; n_tail <- NA_integer_; tail_note <- NULL
      old_to <- getOption("timeout"); options(timeout = 900); on.exit(options(timeout = old_to))
      withProgress(message = paste("Downloading GHCNh", icao), value = 0, {
        for (yr in years) {
          incProgress(1/(length(years) + 1), detail = paste("Year", yr))
          g <- fetch_year(yr)
          if (g$ok) {
            if (is.null(header)) { header <- g$lines[1]; writeLines(g$lines, out_file) }
            else write(g$lines[-1], out_file, append = TRUE)
            got <- c(got, as.character(yr))
          } else if (isTRUE(g$status == 404)) {
            no_file <- c(no_file, as.character(yr))
          } else {
            failed <- c(failed, sprintf("%d (%s)", yr, g$reason)); break
          }
        }
        if (!length(failed) && length(got)) {
          incProgress(1/(length(years) + 1), detail = paste("First day of", tail_yr))
          g <- if (tail_yr <= CURRENT_YEAR) fetch_year(tail_yr) else list(ok = FALSE, status = 404L)
          if (g$ok) {
            cut <- sprintf("%d-01-02T12:00:00", tail_yr)
            dt  <- sub("^[^|]*\\|[^|]*\\|([^|]*)\\|.*$", "\\1", g$lines[-1])
            keep <- g$lines[-1][dt < cut]
            if (length(keep)) write(keep, out_file, append = TRUE)
            n_tail <- length(keep)
          } else {
            tail_note <- sprintf(paste0("NOTE: %s, so the file stops at 31 Dec %d 23:59 UTC and ",
                                        "the evening of 31 Dec %d (local standard time) is missing."),
                                 if (isTRUE(g$status == 404)) sprintf("NCEI has no %d file yet", tail_yr)
                                 else sprintf("the %d file could not be fetched (%s); download again", tail_yr, g$reason),
                                 y2, y2)
          }
        }
      })

      qc <- NULL
      if (length(failed)) {
        if (file.exists(out_file)) file.remove(out_file)
        log <- c(log, paste0("Download FAILED for: ", paste(failed, collapse = ", ")),
                 "Nothing was written. Check the connection and try again.")
      } else if (!length(got)) {
        log <- c(log, paste0("NCEI has no GHCNh file for any year (HTTP 404). ",
                             "Verify the station is a USW-type ASOS (id ", ghcn_id, ")."))
      } else {
        # AERMET ignores NCEI's quality codes, so screen the file exactly as AERMET.R does
        # before anyone uses it (idempotent: AERMET.R re-screening it changes nothing).
        qc <- tryCatch(filter_ghcnh_quality(out_file, verbose = FALSE), error = function(e) e)
      }
      if (inherits(qc, "error")) {
        if (file.exists(out_file)) file.remove(out_file)
        log <- c(log, paste0("Quality screen FAILED (", conditionMessage(qc), ")."),
                 "The unscreened file was removed: NCEI's raw .psv is not safe to use in AERMET.")
      } else if (!is.null(qc)) {
        log <- c(log,
                 paste0("Combined file: ", normalizePath(out_file, mustWork = FALSE)),
                 paste0("Years included: ", paste(got, collapse = ", "),
                        if (!is.na(n_tail)) sprintf("  (+ %d records of %d, to %d-01-02 12:00 UTC)",
                                                    n_tail, tail_yr, tail_yr) else ""),
                 tail_note,
                 if (length(no_file)) paste0("WARNING: NCEI has no GHCNh file for ", paste(no_file, collapse = ", "),
                                             ". The file is named ", y1, "-", y2, " but lacks ",
                                             if (length(no_file) > 1) "those years" else "that year",
                                             ", and AERMET.R will treat it as complete.") else NULL,
                 "", sprintf(paste0("Quality screen (same as AERMET.R's filter_ghcnh_quality): %d values ",
                                    "NCEI flagged suspect/erroneous, %d METAR wind mismatches and %d ",
                                    "short-SYNOP wind/sky decodes blanked."),
                             qc$rejected, qc$ws_crosscheck, qc$synop_misread),
                 paste0("QC log: ", basename(qc$log_file)),
                 "", "Drop-in for AERMET.R: same name and same screening as download_ghcnh() + filter_ghcnh_quality().")
      }
      output$status <- renderText(paste(log, collapse = "\n"))
    })

    observeEvent(input$clear, {
      req(input$station, input$station != "")
      output$status <- renderText(clear_ghcnh_data(input$station))
    })
  })
}

# =============================================================================
# Upper Air server  (IGRA2 radiosonde soundings -> AERMET .PFL profile input)
# =============================================================================
ua_server <- function(id, igra) {
  moduleServer(id, function(input, output, session) {
    # List the sites with soundings in the selected years, not just today's active
    # ones, so retired sites stay available for past windows (Denver ended in 2022).
    igra_win <- reactive({
      df <- igra(); a <- input$y1; b <- input$y2
      if (is.null(a) || is.null(b)) return(df)
      filter(df, is.na(FIRST_YEAR) | FIRST_YEAR <= b, LAST_YEAR >= a)
    })
    wire_state_and_map(input, output, session, igra_win, "map", "IGRA_ID", "STATION_NAME")

    observeEvent(input$download, {
      req(input$station, input$station != "")
      igra_id <- input$station; y1 <- input$y1; y2 <- min(input$y2, CURRENT_YEAR)
      if (y2 < y1) { output$status <- renderText("End year must be >= start year."); return() }

      info <- filter(igra(), IGRA_ID == igra_id) %>% slice(1)
      cover <- if (nrow(info)) paste0(info$FIRST_YEAR, "-", info$LAST_YEAR) else "?"
      dir.create("upper_air", showWarnings = FALSE)
      out_file <- file.path("upper_air", sprintf("%s_UA_%d_%d.txt", igra_id, y1, y2))

      log <- c(paste0("Upper-air download: ", igra_id,
                      if (nrow(info)) paste0(" (", info$STATION_NAME, ")") else "",
                      "  years ", y1, "-", y2, "   [record ", cover, "]"),
               "Fetching full period-of-record zip from IGRA (this can take a minute)...")
      output$status <- renderText(paste(log, collapse = "\n"))

      old_to <- getOption("timeout"); options(timeout = 900); on.exit(options(timeout = old_to))
      url <- sprintf("%s/%s-data.txt.zip", IGRA_POR_BASE, igra_id)
      tmp_zip <- tempfile(fileext = ".zip"); tmp_dir <- tempfile()
      n_kept <- 0L; ok <- FALSE; kept_ym <- character(0)

      withProgress(message = paste("Downloading IGRA", igra_id), value = 0.1, {
        dl <- http_get(url, 600, dest = tmp_zip)
        if (dl$ok && file.exists(tmp_zip) && file.info(tmp_zip)$size > 1000) {
          incProgress(0.5, detail = "Unzipping and trimming to selected years...")
          dir.create(tmp_dir, showWarnings = FALSE)
          unzip(tmp_zip, exdir = tmp_dir)
          data_file <- file.path(tmp_dir, paste0(igra_id, "-data.txt"))
          if (file.exists(data_file)) {
            con_in <- file(data_file, "r"); con_out <- file(out_file, "w")
            cur_hdr <- NULL; cur_dat <- character(0)
            flush_snd <- function() {
              if (!is.null(cur_hdr)) {
                yr <- suppressWarnings(as.numeric(substr(cur_hdr, 14, 17)))
                if (!is.na(yr) && yr >= y1 && yr <= y2) {
                  writeLines(c(cur_hdr, cur_dat), con_out); n_kept <<- n_kept + 1L
                  kept_ym <<- c(kept_ym, paste0(substr(cur_hdr, 14, 17), "-", substr(cur_hdr, 19, 20)))
                }
              }
            }
            repeat {
              line <- readLines(con_in, n = 1)
              if (length(line) == 0) break
              if (substr(line, 1, 1) == "#") { flush_snd(); cur_hdr <- line; cur_dat <- character(0) }
              else cur_dat <- c(cur_dat, line)
            }
            flush_snd(); close(con_in); close(con_out); ok <- TRUE
          }
        }
        incProgress(0.4, detail = "Done")
      })
      unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE)

      if (ok && n_kept > 0) {
        per_yr <- table(substr(kept_ym, 1, 4))
        # months inside the station's record and before this month that have no sounding
        # at all (e.g. KLZK's Feb-Jul 2026 launch suspension)
        a <- max(y1, info$FIRST_YEAR, na.rm = TRUE); b <- min(y2, info$LAST_YEAR, na.rm = TRUE)
        want <- if (a <= b) as.vector(outer(a:b, sprintf("%02d", 1:12), paste, sep = "-")) else character(0)
        want <- sort(want[want < format(Sys.Date(), "%Y-%m")])
        empty <- setdiff(want, kept_ym)
        log <- c(log, "",
                 paste0("Kept ", n_kept, " soundings in ", y1, "-", y2, "."),
                 paste0("Per year: ", paste(names(per_yr), as.integer(per_yr), collapse = ", ")),
                 if (length(empty)) paste0("WARNING: no soundings in ", paste(compact_months(empty), collapse = ", "),
                                           ". AERMET will have no upper air for those months.") else NULL,
                 paste0("Output: ", normalizePath(out_file, mustWork = FALSE)),
                 "IGRA2 format - use as the AERMET Stage 1 upper-air (UPPERAIR) input.")
      } else {
        if (file.exists(out_file) && file.info(out_file)$size == 0) file.remove(out_file)
        log <- c(log, "",
                 if (!dl$ok) paste0("Download failed (", dl$reason,
                                    if (isTRUE(dl$status == 404)) ": IGRA has no file for this station id" else
                                      " - check your connection and try again", ").")
                 else if (!ok) "Unzip failed - the download was not a valid IGRA zip; try again."
                 else paste0("No soundings found in ", y1, "-", y2,
                             " (station record is ", cover, ")."))
      }
      output$status <- renderText(paste(log, collapse = "\n"))
    })

    observeEvent(input$clear, {
      req(input$station, input$station != "")
      output$status <- renderText(clear_ua_data(input$station))
    })
  })
}

# =============================================================================
# App
# =============================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .btn-primary { background-color:#005ea2; border-color:#005ea2; color:#fff; }
    .btn-danger  { background-color:#b50909; border-color:#b50909; color:#fff; }
    #load_status { font-weight:600; color:#005ea2; }
  "))),
  titlePanel("AERMET NOAA Data Downloader - ASOS 1/5-min + GHCNh + Upper Air"),
  p(id = "load_status", "Initializing..."),
  tabsetPanel(
    id = "tabs",
    tabPanel("ASOS 1/5-min Winds", asos_ui("asos")),
    tabPanel("GHCNh Surface", ghcnh_ui("ghcnh")),
    tabPanel("Upper Air (IGRA)", ua_ui("ua"))
  ),
  tags$hr(),
  tags$small(HTML(paste0(
    "Surface: <b>GHCNh</b> (NCEI) replaces the retired ISHD/DS3505 archive (Aug 2025). ",
    "Winds: <b>1-minute ASOS</b> via AERMINUTE. Upper air: <b>IGRA2</b> radiosonde ",
    "soundings (AERMET .PFL). Station metadata: NCEI isd-history.csv. ",
    "Companion to the MDEQ <b>AERMET.R</b> pipeline."
  )))
)

server <- function(input, output, session) {
  stations <- reactiveVal(EMPTY_STATIONS)
  igra     <- reactiveVal(EMPTY_IGRA)

  isolate({
    shinyjs::html("load_status", "Fetching station lists from NCEI, please wait...")
    showModal(modalDialog(title = "Loading Station Data",
      "Fetching the NCEI ISD-history (surface) and IGRA2 (upper-air) station lists...",
      easyClose = FALSE, footer = NULL))
    s <- tryCatch(fetch_and_parse_stations(), error = function(e) NULL)
    ig <- tryCatch(fetch_igra_stations(), error = function(e) NULL)
    removeModal()
    if (!is.null(s) && nrow(s) > 0) stations(s) else stations(EMPTY_STATIONS)
    if (!is.null(ig) && nrow(ig) > 0) igra(ig) else igra(EMPTY_IGRA)
    n_sfc <- if (!is.null(s)) nrow(s) else 0
    n_ua  <- if (!is.null(ig)) nrow(ig) else 0
    if (n_sfc > 0) {
      shinyjs::html("load_status",
        paste0("Loaded ", n_sfc, " active US ASOS stations and ", n_ua,
               " US upper-air (IGRA) sites (listed by the years selected)."))
    } else {
      shinyjs::html("load_status",
        "ERROR: could not load the surface station list. Check network / bundled CSV.")
    }
  })

  asos_server("asos", stations)
  ghcnh_server("ghcnh", stations)
  ua_server("ua", igra)
}

shinyApp(ui, server)
