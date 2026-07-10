# =============================================================================
# AERMET NOAA Data Downloader  (R Shiny)
# -----------------------------------------------------------------------------
# Gathers the raw surface inputs needed to build AERMOD-ready met files with the
# MDEQ AERMET pipeline (AERMET.R):
#
#   * ASOS 1-minute & 5-minute data  -> drives AERMINUTE (hourly winds/calms)
#   * GHCNh hourly surface data (.psv) -> AERMET Stage 1 surface observations
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
#     file AERMET.R's `download_ghcnh()` produces, so downloads are drop-in.
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
    ) %>%
    filter(
      CTRY == "US",
      !is.na(ICAO), ICAO != "", nchar(ICAO) == 4,
      !is.na(WBAN), WBAN != "", WBAN != "99999",
      !is.na(STATE), STATE != "",
      !is.na(LAT), !is.na(LON), !(LAT == 0 & LON == 0),
      !is.na(END_YR), END_YR >= (CURRENT_YEAR - 2)   # recently active ASOS
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
      !is.na(LAST_YEAR), LAST_YEAR >= (CURRENT_YEAR - 2)   # recently active
    ) %>%
    transmute(IGRA_ID, STATE, STATION_NAME, LAT, LON, FIRST_YEAR, LAST_YEAR) %>%
    arrange(STATE, IGRA_ID)
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
    ch <- states()
    sel <- if ("MS" %in% ch) "MS" else if (length(ch)) ch[1] else NULL
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
    updateSelectizeInput(session, "station", choices = choices,
                         selected = if (length(choices)) choices[[1]] else NULL, server = TRUE)
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
            r1 <- tryCatch(GET(u1, timeout(60)), error = function(e) NULL)
            if (!is.null(r1) && status_code(r1) == 200) {
              writeBin(content(r1, "raw"), file.path(d1, paste0(icao, "_", yr, mm, "_1min.dat"))); ok1 <- ok1 + 1
            }
            incProgress(1/total, detail = paste("5-min", yr, mm))
            u5 <- paste0(URL_BASE_5MIN, yr, "/", mm, "/asos-5min-", icao, "-", yr, mm, ".dat")
            r5 <- tryCatch(GET(u5, timeout(60)), error = function(e) NULL)
            if (!is.null(r5) && status_code(r5) == 200) {
              writeBin(content(r5, "raw"), file.path(d5, paste0(icao, "_", yr, mm, "_5min.dat"))); ok5 <- ok5 + 1
            }
          }
        }
      })
      log <- c(log,
               paste0("Saved ", ok1, " one-minute and ", ok5, " five-minute monthly files."),
               paste0("Location: ", normalizePath(icao, mustWork = FALSE)),
               if (ok1 + ok5 == 0) "No files returned - check ICAO/years (some sites lack 1-min data)." else "Done.")
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
      ghcn_id <- info$GHCNH_ID
      out_dir <- file.path(icao, "ghcnh_data")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_file <- file.path(out_dir, sprintf("%s_GHCNh_%d_%d.psv", icao, y1, y2))
      if (file.exists(out_file)) file.remove(out_file)

      log <- c(paste0("GHCNh download: ", icao, " (", ghcn_id, ")  ", y1, "-", y2), "")
      output$status <- renderText(paste(log, collapse = "\n"))

      years <- y1:y2; got <- character(0); missed <- character(0); header_written <- FALSE
      old_to <- getOption("timeout"); options(timeout = 900); on.exit(options(timeout = old_to))
      withProgress(message = paste("Downloading GHCNh", icao), value = 0, {
        for (yr in years) {
          incProgress(1/length(years), detail = paste("Year", yr))
          url <- sprintf("%s/%d/psv/GHCNh_%s_%d.psv", GHCNH_BASE, yr, ghcn_id, yr)
          tmp <- file.path(out_dir, sprintf("_tmp_%d.psv", yr))
          r <- tryCatch(GET(url, timeout(300), write_disk(tmp, overwrite = TRUE)),
                        error = function(e) NULL)
          if (!is.null(r) && status_code(r) == 200 && file.exists(tmp) && file.info(tmp)$size > 1000) {
            ln <- readLines(tmp, warn = FALSE)
            if (!header_written) { writeLines(ln, out_file); header_written <- TRUE }
            else write(ln[-1], out_file, append = TRUE)
            got <- c(got, as.character(yr))
          } else {
            missed <- c(missed, as.character(yr))
          }
          if (file.exists(tmp)) file.remove(tmp)
        }
      })

      if (length(got)) {
        log <- c(log,
                 paste0("Combined file: ", normalizePath(out_file, mustWork = FALSE)),
                 paste0("Years included: ", paste(got, collapse = ", ")),
                 if (length(missed)) paste0("No GHCNh data for: ", paste(missed, collapse = ", ")) else NULL,
                 "", "This .psv matches AERMET.R's download_ghcnh() output and can be used directly as SURFDATA.")
      } else {
        log <- c(log, paste0("No GHCNh data returned for any year. ",
                             "Verify the station is a USW-type ASOS (id ", ghcn_id, ")."))
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
    wire_state_and_map(input, output, session, igra, "map", "IGRA_ID", "STATION_NAME")

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
      n_kept <- 0L; ok <- FALSE

      withProgress(message = paste("Downloading IGRA", igra_id), value = 0.1, {
        dl <- tryCatch({
          GET(url, timeout(600), write_disk(tmp_zip, overwrite = TRUE)); TRUE
        }, error = function(e) FALSE)
        if (dl && file.exists(tmp_zip) && file.info(tmp_zip)$size > 1000) {
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
        log <- c(log, "",
                 paste0("Kept ", n_kept, " soundings in ", y1, "-", y2, "."),
                 paste0("Output: ", normalizePath(out_file, mustWork = FALSE)),
                 "IGRA2 format - use as the AERMET Stage 1 upper-air (UPPERAIR) input.")
      } else {
        if (file.exists(out_file) && file.info(out_file)$size == 0) file.remove(out_file)
        log <- c(log, "",
                 if (!ok) "Download or unzip failed - check the station id and your connection."
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
  titlePanel("AERMET NOAA Data Downloader - ASOS 1/5-min + GHCNh"),
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
               " active US upper-air (IGRA) sites."))
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
