library(shiny)
library(httr)
library(leaflet)
library(R.utils) # For gunzip
library(dplyr)
library(readr) # For read_fwf
library(stringr) # For str_trim
library(shinyjs) # For shinyjs::delay and other UI interactions

# --- Helper function to parse MASTER-STN-HIST.TXT ---
fetch_and_parse_stations <- function() {
  station_history_url <- "https://www1.ncdc.noaa.gov/pub/data/inventories/MASTER-STN-HIST.TXT"
  
  col_positions <- fwf_positions(
    start = c(13,  23,  29,  46,  51,  72, 174, 183, 192, 193, 196, 199, 202, 203, 207, 210, 245),
    end   = c(18,  27,  33,  49,  70,  73, 181, 190, 192, 194, 197, 200, 202, 205, 208, 211, 294),
    col_names = c("USAF_COOP", "WBAN_NCDC", "WMO_ID", "ICAO", "COUNTRY_NAME", "STATE", "BEGIN_DATE", "END_DATE",
                  "LAT_DIR", "LAT_DEG", "LAT_MIN", "LAT_SEC",
                  "LON_DIR", "LON_DEG", "LON_MIN", "LON_SEC",
                  "STATION_TYPES")
  )
  
  response <- tryCatch({
    httr::GET(station_history_url, timeout(60))
  }, error = function(e) {
    showNotification(paste("Network error fetching station list URL:", e$message), type = "error", duration = NULL)
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  if (httr::status_code(response) != 200) {
    showNotification(paste("Failed to download station list. HTTP Status:", httr::status_code(response)), type = "error", duration = NULL)
    return(NULL)
  }
  
  file_content_text <- httr::content(response, "text", encoding = "UTF-8")
  if (is.null(file_content_text) || nchar(file_content_text) == 0) {
    showNotification("Downloaded station list file content is empty.", type = "error", duration = NULL)
    return(NULL)
  }
  
  stations_raw <- tryCatch({
    readr::read_fwf(file_content_text, col_positions = col_positions, skip = 1,
                    guess_max = 20000, 
                    col_types = readr::cols(.default = "c"),
                    progress = FALSE)
  }, error = function(e) {
    showNotification(paste("Error parsing station list file content:", e$message), type = "error", duration = NULL)
    return(NULL)
  })
  
  if (is.null(stations_raw) || nrow(stations_raw) == 0) {
    return(stations_raw) 
  }
  
  parse_lat_lon <- function(dir_char, deg_char, min_char, sec_char, coord_type = "Unknown") {
    if(!is.character(deg_char) || length(deg_char) != 1) deg_char <- as.character(deg_char[1])
    if(!is.character(min_char) || length(min_char) != 1) min_char <- as.character(min_char[1])
    if(!is.character(sec_char) || length(sec_char) != 1) sec_char <- as.character(sec_char[1])
    if(!is.character(dir_char) || length(dir_char) != 1) dir_char <- as.character(dir_char[1])
    
    deg <- suppressWarnings(as.numeric(deg_char))
    min_val <- suppressWarnings(as.numeric(min_char))
    sec_val <- suppressWarnings(as.numeric(sec_char))
    
    deg[is.na(deg) | deg_char == ""] <- 0
    min_val[is.na(min_val) | min_char == ""] <- 0
    sec_val[is.na(sec_val) | sec_char == ""] <- 0
    
    value <- deg + min_val/60 + sec_val/3600
    
    if (!is.na(dir_char) && dir_char == "-") {
      value <- -value
    }
    if(length(value) != 1 || !is.numeric(value)) {
      warning(paste("parse_lat_lon for", coord_type, "produced non-numeric or multi-value result for inputs:",
                    dir_char, deg_char, min_char, sec_char))
      return(NA_real_)
    }
    return(value)
  }
  
  stations_intermediate <- stations_raw %>%
    mutate_all(str_trim) %>% 
    filter(END_DATE == "99991231", 
           str_to_upper(COUNTRY_NAME) == "UNITED STATES", 
           !is.na(ICAO) & ICAO != "",
           !is.na(STATE) & STATE != "",
           !is.na(STATION_TYPES) & str_detect(toupper(STATION_TYPES), "ASOS"),
           !is.na(WMO_ID) & WMO_ID != "", 
           !is.na(WBAN_NCDC) & WBAN_NCDC != "" 
    ) 
  
  if (nrow(stations_intermediate) == 0) {
    showNotification("No suitable ASOS stations with WMO & WBAN IDs found after initial filtering.", type = "warning", duration = 7)
    return(tibble()) 
  }
  
  stations_df <- stations_intermediate %>%
    rowwise() %>% 
    mutate(
      LAT = tryCatch({parse_lat_lon(LAT_DIR, LAT_DEG, LAT_MIN, LAT_SEC, "LAT")}, 
                     error = function(e) {cat("Error parsing LAT for ICAO:", if_else(exists("ICAO"), ICAO, "UNKNOWN"), "- Error:", e$message, "\n"); NA_real_}),
      LON = tryCatch({parse_lat_lon(LON_DIR, LON_DEG, LON_MIN, LON_SEC, "LON")}, 
                     error = function(e) {cat("Error parsing LON for ICAO:", if_else(exists("ICAO"), ICAO, "UNKNOWN"), "- Error:", e$message, "\n"); NA_real_})
    ) %>%
    ungroup() %>% 
    mutate(CTRY = "US", 
           USAF_COOP_ID = USAF_COOP, 
           WBAN_ID = WBAN_NCDC 
    ) %>%
    select(ICAO, WMO_ID, WBAN_ID, USAF_COOP_ID, CTRY, STATE, LAT, LON) %>% 
    filter(!is.na(LAT) & !is.na(LON))
  
  if (nrow(stations_df) == 0) {
    showNotification("All stations filtered out after LAT/LON parsing. Check console for errors.", type = "warning", duration = 10)
  }
  
  stations_df <- stations_df %>%
    distinct(ICAO, .keep_all = TRUE) 
  
  return(stations_df)
}


# --- ASOS Configuration ---
url_base_1min <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/"
url_base_5min <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/"

createOutputDirectories <- function(selected_station_icao, selected_year) { # selected_station is ICAO
  # Main station directory is just the ICAO
  main_station_dir <- selected_station_icao
  year_specific_dir <- file.path(main_station_dir, selected_year)
  
  station_output_dir_1min <- file.path(year_specific_dir, "asos_data_1min")
  station_output_dir_5min <- file.path(year_specific_dir, "asos_data_5min")
  
  if (!dir.exists(station_output_dir_1min)) {
    dir.create(station_output_dir_1min, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(station_output_dir_5min)) {
    dir.create(station_output_dir_5min, recursive = TRUE, showWarnings = FALSE)
  }
  # No return needed as paths are constructed directly in download logic, but could be useful
  # For this structure, main_station_dir is simply selected_station_icao.
}

clearDownloadedData_asos <- function(selected_station_icao, selected_start_year, selected_end_year) {
  cat("Attempting to clear ASOS data for station:", selected_station_icao, 
      "Years:", selected_start_year, "to", selected_end_year, "\n")
  main_station_dir <- selected_station_icao
  cleared_any_asos_data <- FALSE
  
  for (year_val in selected_start_year:selected_end_year) {
    year_char <- as.character(year_val)
    year_specific_dir <- file.path(main_station_dir, year_char)
    
    dir_1min_to_remove <- file.path(year_specific_dir, "asos_data_1min")
    dir_5min_to_remove <- file.path(year_specific_dir, "asos_data_5min")
    
    if (dir.exists(dir_1min_to_remove)) {
      unlink(dir_1min_to_remove, recursive = TRUE, force = TRUE)
      cat("Removed:", dir_1min_to_remove, "\n")
      cleared_any_asos_data <- TRUE
    }
    if (dir.exists(dir_5min_to_remove)) {
      unlink(dir_5min_to_remove, recursive = TRUE, force = TRUE)
      cat("Removed:", dir_5min_to_remove, "\n")
      cleared_any_asos_data <- TRUE
    }
    
    # If the year-specific directory is now empty, remove it
    if (dir.exists(year_specific_dir) && 
        length(list.files(year_specific_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(year_specific_dir, recursive = TRUE, force = TRUE)
      cat("Removed empty year directory for ASOS data:", year_specific_dir, "\n")
    }
  }
  
  # After clearing specific ASOS data, check if the main station directory is empty
  # (e.g., if ISH data was also cleared or never downloaded)
  if (dir.exists(main_station_dir) && 
      length(list.files(main_station_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(main_station_dir, recursive = TRUE, force = TRUE)
    cat("Removed empty main station directory after clearing ASOS data:", main_station_dir, "\n")
  }
  
  if (cleared_any_asos_data) {
    return(paste("ASOS data cleared for station", selected_station_icao, "for years", selected_start_year, "to", selected_end_year))
  } else {
    return(paste("No ASOS data found to clear for station", selected_station_icao, "for years", selected_start_year, "to", selected_end_year))
  }
}


# --- ISH Configuration ---
clearDownloadedData_ish <- function(selected_icao_for_ish) { 
  main_station_dir <- selected_icao_for_ish
  ish_data_specific_dir_to_clear <- file.path(main_station_dir, "ish_gz_data")
  
  cat("Attempting to clear ISH (.gz) data from directory:", ish_data_specific_dir_to_clear, "\n")
  
  if (dir.exists(ish_data_specific_dir_to_clear)) {
    unlink(ish_data_specific_dir_to_clear, recursive = TRUE, force = TRUE)
    cat("Removed ISH (.gz) data directory:", ish_data_specific_dir_to_clear, "\n")
    
    if (dir.exists(main_station_dir) && 
        length(list.files(main_station_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(main_station_dir, recursive = TRUE, force = TRUE)
      cat("Removed empty main station directory after clearing ISH data:", main_station_dir, "\n")
    }
    return(paste("Cleared ISH (.gz) data for station", selected_icao_for_ish))
  } else {
    return(paste("No ISH (.gz) data directory found at", ish_data_specific_dir_to_clear, "for station", selected_icao_for_ish))
  }
}

# --- UI Definitions ---
asos_ui_module <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("state_asos"), "Filter by State", choices = NULL, selectize = FALSE),
        leafletOutput(ns("map_asos"), height = "300px"), 
        selectizeInput(ns("station_asos"), "Select ASOS Station (by ICAO)", choices = NULL),
        sliderInput(ns("start_year_asos"), "Start Year", min = 2000, max = as.integer(format(Sys.Date(), "%Y")) -1, value = as.integer(format(Sys.Date(), "%Y")) - 3, step = 1, sep = ""),
        sliderInput(ns("end_year_asos"), "End Year", min = 2000, max = as.integer(format(Sys.Date(), "%Y"))-1, value = as.integer(format(Sys.Date(), "%Y")) - 2, step = 1, sep = ""),
        actionButton(ns("downloadButton_asos"), "Download ASOS Data", class = "btn-primary"),
        actionButton(ns("clearDataButton_asos"), "Clear Downloaded ASOS Data", class = "btn-danger")
      ),
      mainPanel(
        textOutput(ns("status_asos"))
      )
    )
  )
}

ish_ui_module <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("state_ish"), "Filter by State", choices = NULL, selectize = FALSE),
        leafletOutput(ns("map_ish"), height = "300px"), 
        selectizeInput(ns("station_ish"), "Select ISH Station (by ICAO)", choices = NULL),
        sliderInput(ns("start_year_ish"), "Start Year", min = 1990, max = as.integer(format(Sys.Date(), "%Y"))-1, value = as.integer(format(Sys.Date(), "%Y")) - 3, step = 1, sep = ""),
        sliderInput(ns("end_year_ish"), "End Year", min = 1990, max = as.integer(format(Sys.Date(), "%Y"))-1, value = as.integer(format(Sys.Date(), "%Y")) - 2, step = 1, sep = ""),
        actionButton(ns("downloadButton_ish"), "Download ISH Data (.gz format)", class = "btn-primary"),
        actionButton(ns("clearDataButton_ish"), "Clear Downloaded ISH Data", class = "btn-danger")
      ),
      mainPanel(
        textOutput(ns("status_ish"))
      )
    )
  )
}

# --- Server Logic Modules ---
asos_server_module <- function(id, ASOS_Stations_df_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    available_states_asos <- reactive({
      req(ASOS_Stations_df_reactive())
      df <- ASOS_Stations_df_reactive()
      if(!is.null(df) && nrow(df) > 0 && "STATE" %in% names(df)) {
        sort(unique(df$STATE))
      } else {
        character(0)
      }
    })
    
    observe({
      states_choices <- available_states_asos()
      updateSelectInput(session, "state_asos", choices = states_choices, selected = if(length(states_choices) > 0) states_choices[1] else NULL)
    })
    
    asos_data_filtered_by_state <- reactive({
      req(ASOS_Stations_df_reactive(), input$state_asos)
      df <- ASOS_Stations_df_reactive()
      filter(df, STATE == input$state_asos, !is.na(ICAO) & ICAO != "")
    })
    
    observe({
      df_stations <- asos_data_filtered_by_state()
      stations_in_state <- character(0)
      if(nrow(df_stations) > 0) {
        stations_in_state <- sort(unique(df_stations$ICAO))
      }
      updateSelectizeInput(session, "station_asos", choices = stations_in_state, 
                           selected = if(length(stations_in_state)>0) stations_in_state[1] else NULL, 
                           server = TRUE)
    })
    
    output$map_asos <- renderLeaflet({
      df_map_data <- asos_data_filtered_by_state()
      if (is.null(df_map_data) || nrow(df_map_data) == 0) {
        return(leaflet() %>% addTiles() %>% setView(lng = -98.583333, lat = 39.833333, zoom = 3)) 
      }
      leaflet(data = df_map_data) %>%
        addTiles() %>%
        addMarkers(
          lat = ~LAT, lng = ~LON, label = ~ICAO, layerId = ~ICAO
        ) %>% setView(lng = mean(df_map_data$LON, na.rm = TRUE), lat = mean(df_map_data$LAT, na.rm = TRUE), zoom = 6)
    })
    
    observeEvent(input$map_asos_marker_click, {
      event <- input$map_asos_marker_click
      req(event$id)
      updateSelectizeInput(session, "station_asos", selected = event$id)
    })
    
    observeEvent(input$downloadButton_asos, {
      req(input$station_asos, input$start_year_asos, input$end_year_asos, input$station_asos != "")
      selected_station_icao <- input$station_asos # This is the ICAO code
      selected_start_year <- input$start_year_asos
      selected_end_year <- input$end_year_asos
      
      output$status_asos <- renderText(paste("Starting ASOS download for station", selected_station_icao, "..."))
      
      # Main station directory is simply the ICAO
      main_station_dir <- selected_station_icao
      
      total_ops <- (selected_end_year - selected_start_year + 1) * 12 * 2 
      
      withProgress(message = 'Downloading ASOS Data', value = 0, {
        op_count <- 0
        for (year in selected_start_year:selected_end_year) {
          # ASOS data goes into ICAO/YEAR/asos_data_type
          year_specific_dir <- file.path(main_station_dir, as.character(year))
          output_dir_1min <- file.path(year_specific_dir, "asos_data_1min")
          output_dir_5min <- file.path(year_specific_dir, "asos_data_5min")
          
          if (!dir.exists(output_dir_1min)) dir.create(output_dir_1min, recursive = TRUE, showWarnings = FALSE)
          if (!dir.exists(output_dir_5min)) dir.create(output_dir_5min, recursive = TRUE, showWarnings = FALSE)
          
          for (month in 1:12) {
            op_count <- op_count + 1 
            incProgress(1/total_ops, detail = paste("1-min: Yr", year, "Mo", sprintf("%02d", month)))
            
            url_1min <- paste0(url_base_1min, year, "/", sprintf("%02d", month), "/asos-1min-pg1-", selected_station_icao, "-", year, sprintf("%02d", month), ".dat")
            file_name_1min <- paste0(selected_station_icao, "_", year, sprintf("%02d", month), "_1min.dat")
            file_path_1min <- file.path(output_dir_1min, file_name_1min)
            
            tryCatch({
              response_1min <- GET(url_1min, timeout(30))
              if (status_code(response_1min) == 200) {
                writeBin(content(response_1min, "raw"), file_path_1min)
                cat("1-min data: ", selected_station_icao, year, month, "saved.\n")
              } else {
                cat("Failed 1-min: ", selected_station_icao, year, month, "Status:", status_code(response_1min), "URL:", url_1min, "\n")
                stop_for_status(response_1min, task = paste("download 1-min data"))
              }
            }, error = function(e) {
              cat("Error 1-min: ", selected_station_icao, year, month, ":", e$message, "\n")
              output$status_asos <- renderText(paste("Error (1-min) Yr", year, "Mo", month)) 
            })
            
            op_count <- op_count + 1 
            incProgress(1/total_ops, detail = paste("5-min: Yr", year, "Mo", sprintf("%02d", month)))
            
            url_5min <- paste0(url_base_5min, year, "/", sprintf("%02d", month), "/asos-5min-", selected_station_icao, "-", year, sprintf("%02d", month), ".dat")
            file_name_5min <- paste0(selected_station_icao, "_", year, sprintf("%02d", month), "_5min.dat")
            file_path_5min <- file.path(output_dir_5min, file_name_5min)
            
            tryCatch({
              response_5min <- GET(url_5min, timeout(30))
              if (status_code(response_5min) == 200) {
                writeBin(content(response_5min, "raw"), file_path_5min)
                cat("5-min data: ", selected_station_icao, year, month, "saved.\n")
              } else {
                cat("Failed 5-min: ", selected_station_icao, year, month, "Status:", status_code(response_5min), "URL:", url_5min, "\n")
                stop_for_status(response_5min, task = paste("download 5-min data"))
              }
            }, error = function(e) {
              cat("Error 5-min: ", selected_station_icao, year, month, ":", e$message, "\n")
              output$status_asos <- renderText(paste("Error (5-min) Yr", year, "Mo", month))
            })
          }
        }
      })
      output$status_asos <- renderText(paste("ASOS download process completed for station", selected_station_icao,". Check console for details."))
    })
    
    observeEvent(input$clearDataButton_asos, {
      req(input$station_asos, input$start_year_asos, input$end_year_asos, input$station_asos != "")
      msg <- clearDownloadedData_asos(input$station_asos, input$start_year_asos, input$end_year_asos)
      output$status_asos <- renderText(msg)
    })
  })
}

ish_server_module <- function(id, ASOS_Stations_df_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    available_states_ish <- reactive({
      req(ASOS_Stations_df_reactive())
      df <- ASOS_Stations_df_reactive()
      if(!is.null(df) && nrow(df) > 0 && "STATE" %in% names(df)) {
        sort(unique(df$STATE))
      } else {
        character(0)
      }
    })
    
    observe({
      states_choices <- available_states_ish()
      updateSelectInput(session, "state_ish", choices = states_choices, selected = if(length(states_choices) > 0) states_choices[1] else NULL)
    })
    
    ish_data_filtered_by_state <- reactive({
      req(ASOS_Stations_df_reactive(), input$state_ish)
      df <- ASOS_Stations_df_reactive()
      filter(df, STATE == input$state_ish) 
    })
    
    observe({ 
      df_stations <- ish_data_filtered_by_state()
      stations_choices_list <- list() 
      if(nrow(df_stations) > 0) {
        df_stations <- df_stations %>%
          mutate(
            Padded_WBAN_ID = sprintf("%05s", WBAN_ID),
            Modified_WMO_ID = if_else(nchar(WMO_ID) == 5, paste0(WMO_ID, "0"), WMO_ID),
            ISH_DISPLAY_LABEL = paste(ICAO, paste0("(", Modified_WMO_ID, "-", Padded_WBAN_ID, ")"))
          )
        stations_choices_list <- setNames(as.list(df_stations$ICAO), df_stations$ISH_DISPLAY_LABEL)
      }
      updateSelectizeInput(session, "station_ish", choices = stations_choices_list, 
                           selected = if(length(stations_choices_list)>0) stations_choices_list[[1]] else NULL, 
                           server = TRUE)
    })
    
    output$map_ish <- renderLeaflet({
      df_map_data <- ish_data_filtered_by_state()
      if (is.null(df_map_data) || nrow(df_map_data) == 0) {
        return(leaflet() %>% addTiles() %>% setView(lng = -98.583333, lat = 39.833333, zoom = 3))
      }
      leaflet(data = df_map_data) %>%
        addTiles() %>%
        addMarkers(
          lat = ~LAT, lng = ~LON, label = ~ICAO, layerId = ~ICAO 
        ) %>% setView(lng = mean(df_map_data$LON, na.rm = TRUE), lat = mean(df_map_data$LAT, na.rm = TRUE), zoom = 6)
    })
    
    observeEvent(input$map_ish_marker_click, {
      event <- input$map_ish_marker_click
      req(event$id) 
      updateSelectizeInput(session, "station_ish", selected = event$id)
    })
    
    observeEvent(input$downloadButton_ish, {
      req(input$station_ish, input$start_year_ish, input$end_year_ish, input$station_ish != "")
      
      selected_icao_for_ish <- input$station_ish 
      start_year <- input$start_year_ish
      end_year <- input$end_year_ish
      
      station_info <- ASOS_Stations_df_reactive() %>% 
        filter(ICAO == selected_icao_for_ish) %>% 
        slice(1) 
      
      if(nrow(station_info) == 0 || is.na(station_info$WMO_ID) || is.na(station_info$WBAN_ID)) {
        output$status_ish <- renderText(paste("Could not find required WMO/WBAN ID for selected ICAO:", selected_icao_for_ish))
        return()
      }
      
      effective_wmo_id <- station_info$WMO_ID
      if (nchar(effective_wmo_id) == 5) { 
        effective_wmo_id <- paste0(effective_wmo_id, "0")
      }
      padded_wban_id <- sprintf("%05s", station_info$WBAN_ID)
      station_file_id_prefix <- paste(effective_wmo_id, padded_wban_id, sep = "-") 
      
      output$status_ish <- renderText(paste("Starting ISH (.gz format) download for ICAO", selected_icao_for_ish, 
                                            "(using ID", station_file_id_prefix, ")..."))
      
      main_station_output_dir <- selected_icao_for_ish 
      ish_data_specific_dir <- file.path(main_station_output_dir, "ish_gz_data") 
      
      if (!dir.exists(ish_data_specific_dir)) {
        dir.create(ish_data_specific_dir, showWarnings = FALSE, recursive = TRUE)
        cat("Created ISH data directory:", ish_data_specific_dir, "\n")
      }
      
      combined_file_name <- paste0(selected_icao_for_ish, "_", station_file_id_prefix, "_", start_year, "_", end_year, "_combined.ish")
      combined_file_path <- file.path(ish_data_specific_dir, combined_file_name) 
      
      if (file.exists(combined_file_path)) {
        file.remove(combined_file_path) 
        cat("Removed existing combined ISH file:", combined_file_path, "\n")
      }
      
      years_to_download <- start_year:end_year
      total_years <- length(years_to_download)
      all_downloads_successful <- TRUE
      
      withProgress(message = 'Downloading ISH (.gz) Data', value = 0, {
        for (i in seq_along(years_to_download)) {
          year <- years_to_download[i]
          incProgress(1/total_years, detail = paste("Year:", year))
          
          file_on_server <- paste0(station_file_id_prefix, "-", year, ".gz")
          ish_url <- paste0("https://www1.ncdc.noaa.gov/pub/data/noaa/", year, "/", file_on_server)
          
          temp_gz_filename <- file.path(ish_data_specific_dir, file_on_server)
          temp_unzipped_filename <- file.path(ish_data_specific_dir, gsub(".gz$", ".txt", file_on_server))
          
          download_success_this_year <- FALSE
          tryCatch({
            response <- GET(ish_url, timeout(90), write_disk(temp_gz_filename, overwrite = TRUE))
            if (status_code(response) == 200) {
              cat("Downloaded:", temp_gz_filename, "from URL:", ish_url, "\n")
              R.utils::gunzip(temp_gz_filename, destname = temp_unzipped_filename, overwrite = TRUE, remove = FALSE) 
              cat("Unzipped to:", temp_unzipped_filename, "\n")
              data_content_lines <- readLines(temp_unzipped_filename)
              write(data_content_lines, combined_file_path, append = TRUE)
              cat("Appended data for", year, "to", combined_file_path, "\n")
              if(file.exists(temp_unzipped_filename)) file.remove(temp_unzipped_filename)
              if(file.exists(temp_gz_filename)) file.remove(temp_gz_filename)
              download_success_this_year <- TRUE
            } else {
              cat("Failed ISH download: ", selected_icao_for_ish, year, "Status:", status_code(response), "URL:", ish_url, "\n")
              stop_for_status(response, task = paste("download ISH .gz data from", ish_url))
            }
          }, error = function(e) {
            all_downloads_successful <- FALSE 
            cat("Error ISH .gz: ", selected_icao_for_ish, year, "URL:", ish_url, ":", e$message, "\n")
            output$status_ish <- renderText(paste("Error ISH .gz Yr", year, "(Check console)"))
            if (file.exists(temp_gz_filename)) file.remove(temp_gz_filename)
            if (file.exists(temp_unzipped_filename)) file.remove(temp_unzipped_filename)
          })
        }
      })
      if(all_downloads_successful && file.exists(combined_file_path) && file.info(combined_file_path)$size > 0) { 
        output$status_ish <- renderText(paste("ISH (.gz) download completed. Combined file:", combined_file_path))
      } else if (file.exists(combined_file_path) && file.info(combined_file_path)$size > 0) {
        output$status_ish <- renderText(paste("ISH (.gz) download had some issues. Partial combined file created:", combined_file_path, ". Check console."))
      } else {
        output$status_ish <- renderText(paste("ISH (.gz) download failed or produced no data. Check console for details."))
      }
    })
    
    observeEvent(input$clearDataButton_ish, {
      req(input$station_ish, input$station_ish != "") 
      msg <- clearDownloadedData_ish(input$station_ish) 
      output$status_ish <- renderText(msg)
    })
  })
}

# --- Main App UI and Server ---
ui <- fluidPage(
  useShinyjs(), 
  tags$head(
    tags$style(HTML("
      .btn-primary { background-color: #007bff; border-color: #007bff; color: white; }
      .btn-danger { background-color: #dc3545; border-color: #dc3545; color: white; }
    "))
  ),
  titlePanel("AERMET NOAA Data Downloaders"),
  p(id="station_load_status_text", "Initializing..."), 
  tabsetPanel(
    id = "main_tabs", 
    tabPanel("ASOS Data", asos_ui_module("asos_tab")),
    tabPanel("ISH Data", ish_ui_module("ish_tab"))
  )
)

server <- function(input, output, session) {
  ASOS_Stations_df_reactive <- reactiveVal(
    tibble(ICAO = character(), WMO_ID = character(), WBAN_ID = character(), USAF_COOP_ID = character(),
           CTRY = character(), STATE = character(), LAT = numeric(), LON = numeric())
  )
  
  isolate({
    shinyjs::html("station_load_status_text", "Fetching station list from NOAA, please wait...")
    showModal(modalDialog(
      title = "Loading Station Data",
      "Fetching and processing the master station list from NOAA. This may take a minute or two...",
      easyClose = FALSE, footer = NULL
    ))
    
    stations <- tryCatch({
      fetch_and_parse_stations()
    }, error = function(e) {
      showNotification(paste("Critical error during fetch_and_parse_stations MAIN execution:", e$message), type="error", duration=NULL)
      NULL
    })
    
    removeModal() 
    
    if(!is.null(stations) && nrow(stations) > 0) {
      ASOS_Stations_df_reactive(stations)
      shinyjs::html("station_load_status_text", paste("Station list loaded with", nrow(stations), "ASOS stations suitable for download."))
      shinyjs::delay(200, showNotification("Station list loaded successfully.", type = "message", duration = 5))
    } else {
      ASOS_Stations_df_reactive(
        tibble(ICAO = character(), WMO_ID = character(), WBAN_ID = character(), USAF_COOP_ID = character(),
               CTRY = character(), STATE = character(), LAT = numeric(), LON = numeric())
      )
      shinyjs::html("station_load_status_text", "ERROR: Failed to load/parse station list, or no suitable stations found. App functionality may be limited. Check console/notifications.")
      shinyjs::delay(200,showNotification("Station list is empty, could not be parsed, or no stations matched all criteria. Some features may not work.", type = "warning", duration = NULL))
    }
  })
  
  asos_server_module("asos_tab", ASOS_Stations_df_reactive)
  ish_server_module("ish_tab", ASOS_Stations_df_reactive)
}

shinyApp(ui, server)
