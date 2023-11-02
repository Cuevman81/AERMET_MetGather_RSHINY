library(shiny)
library(httr)
library(leaflet)
library(R.utils)
library(dplyr)

url_base_1min <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/"
url_base_5min <- "https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/"

ASOS_Stations <- ASOS_Stations

ASOS_Stations$WBAN <- as.integer(ASOS_Stations$WBAN)
ASOS_Stations$WBAN <- sprintf("%05d", ASOS_Stations$WBAN)

ASOS_Stations <- dplyr::filter(ASOS_Stations, CTRY == 'US' & !is.na(ICAO) & ICAO != "")

createOutputDirectories <- function(selected_station, selected_year) {
  station_output_dir_1min <- file.path(selected_station, selected_year, "asos_data_1min")
  station_output_dir_5min <- file.path(selected_station, selected_year, "asos_data_5min")
  
  if (!dir.exists(station_output_dir_1min)) {
    dir.create(station_output_dir_1min, recursive = TRUE)
  }
  
  if (!dir.exists(station_output_dir_5min)) {
    dir.create(station_output_dir_5min, recursive = TRUE)
  }
  
  return(list(output_dir_1min = station_output_dir_1min, output_dir_5min = station_output_dir_5min))
}

clearDownloadedData <- function(selected_station, selected_start_year, selected_end_year) {
  station_dir <- file.path(selected_station, selected_start_year)
  
  files <- list.files(station_dir)
  
  selected_files <- list.files(station_dir, pattern = paste0(selected_start_year, selected_station, "|", selected_end_year))
  
  file_paths <- file.path(station_dir, selected_files)
  file.remove(file_paths)
  cat("Selected files deleted for ASOS.\n")
}

clearDownloadedData_ish <- function(selected_usaf, start_year, end_year) {
  station_dir <- paste0(selected_usaf, "_ISH")
  
  if (dir.exists(station_dir)) {
    years <- start_year:end_year
    
    for (year in years) {
      file_path <- file.path(station_dir, paste0(selected_usaf, "-", year, ".txt"))
      
      if (file.exists(file_path)) {
        file.remove(file_path)
      }
    }
    
    cat("Selected files deleted for ISH.\n")
  }
}

asos_ui <- fluidPage(
  titlePanel("ASOS 1_5 Minute NOAA Data Downloader"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_asos", "Filter by State", choices = c(sort(unique(ASOS_Stations$STATE))),
                  selectize = FALSE),
      div(leafletOutput("map_asos"), style = "height:400px; width:800px;"),
      selectizeInput("station_asos", "Select ASOS Station",
                     choices = NULL),
      sliderInput("start_year_asos", "Start Year", min = 2010, max = 2023, value = 2018, step = 1),
      sliderInput("end_year_asos", "End Year", min = 2010, max = 2023, value = 2022, step = 1),
      actionButton("downloadButton_asos", "Download ASOS Data", class = "btn-custom"),  # Add a custom class
      actionButton("clearDataButton_asos", "Clear Downloaded ASOS Data", class = "btn-custom")
    ),
    mainPanel(
      textOutput("status_asos")
    )
  )
)

asos_server <- function(input, output, session) {
  asos_data_filtered <- reactive({
    selected_state <- input$state_asos
    ASOS_Stations[ASOS_Stations$STATE == selected_state, ]
  })
  
  output$map_asos <- renderLeaflet({
    leaflet(data = asos_data_filtered()) %>%
      addTiles() %>%
      addMarkers(
        lat = ~LAT,
        lng = ~LON,
        label = ~ICAO,
        group = "ASOS Stations"
      )
  })
  
  observe({
    selected_state <- input$state_asos
    stations_in_state <- unique(ASOS_Stations$ICAO[ASOS_Stations$STATE == selected_state])
    updateSelectizeInput(session, "station_asos", choices = stations_in_state)
  })
  
  observeEvent(input$map_asos_marker_click, {
    event <- input$map_asos_marker_click
    if (!is.null(event)) {
      clicked_station <- asos_data_filtered()[
        asos_data_filtered()$LAT == event$lat & asos_data_filtered()$LON == event$lng,
      ]
      if (!is.null(clicked_station)) {  # Corrected "is null" to "is.null"
        updateSelectizeInput(session, "station_asos", selected = clicked_station$ICAO)
      }
    }
  })
  
  observeEvent(input$downloadButton_asos, {
    selected_station <- input$station_asos
    selected_start_year <- input$start_year_asos
    selected_end_year <- input$end_year_asos
    
    for (year in selected_start_year:selected_end_year) {
      output_directories <- createOutputDirectories(selected_station, year)
      output_dir_1min <- output_directories$output_dir_1min
      output_dir_5min <- output_directories$output_dir_5min
      
      for (month in 1:12) {  # Iterate over all months
        url_1min <- paste0(url_base_1min, year, "/", sprintf("%02d", month), "/asos-1min-pg1-", selected_station, "-", year, sprintf("%02d", month), ".dat")
        file_name_1min <- paste0(selected_station, year, sprintf("%02d", month), ".dat")
        file_path_1min <- file.path(output_dir_1min, file_name_1min)
        
        response_1min <- GET(url_1min)
        data_1min <- content(response_1min, "text")
        write(data_1min, file_path_1min)
        cat("One minute data for month", month, "and year", year, "downloaded and saved to", file_path_1min, "\n")
        
        url_5min <- paste0(url_base_5min, year, "/", sprintf("%02d", month), "/asos-5min-", selected_station, "-", year, sprintf("%02d", month), ".dat")
        file_name_5min <- paste0(selected_station, year, sprintf("%02d", month), ".dat")
        file_path_5min <- file.path(output_dir_5min, file_name_5min)
        
        response_5min <- GET(url_5min)
        data_5min <- content(response_5min, "text")
        write(data_5min, file_path_5min)
        cat("Five minute data for month", month, "and year", year, "downloaded and saved to", file_path_5min, "\n")
      }
    }
  })
  
  
  
  observeEvent(input$clearDataButton_asos, {
    selected_station <- input$station_asos
    selected_start_year <- input$start_year_asos
    selected_end_year <- input$end_year_asos
    clearDownloadedData(selected_station, selected_start_year, selected_end_year)
  })
}

ish_ui <- fluidPage(
  titlePanel("ISH NOAA Data Downloader"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_ish", "Filter by State", choices = sort(unique(ASOS_Stations$STATE)),
                  selectize = FALSE),
      div(leafletOutput("map_ish"), style = "height:400px; width:800px;"),
      selectizeInput("station_ish", "Select ISH Station",
                     choices = NULL),
      sliderInput("start_year_ish", "Start Year", min = 2010, max = 2023, value = 2018, step = 1),
      sliderInput("end_year_ish", "End Year", min = 2010, max = 2023, value = 2022, step = 1),
      actionButton("downloadButton_ish", "Download ISH Data", class = "btn-custom")  # Add a custom class
      # No "Clear Downloaded ISH Data" button in this section
    ),
    mainPanel(
      textOutput("status_ish")
    )
  )
)

ish_server <- function(input, output, session) {
  observe({
    stations_in_state <- unique(paste(ASOS_Stations$USAF, ASOS_Stations$WBAN, sep = "-")[ASOS_Stations$STATE == input$state_ish])
    updateSelectizeInput(session, "station_ish", choices = stations_in_state)
  })
  
  observeEvent(input$downloadButton_ish, {
    selected_usaf <- input$station_ish
    start_year <- input$start_year_ish
    end_year <- input$end_year_ish
    
    # Extract the USAF-WBAN identifier
    wbans <- ASOS_Stations$WBAN[ASOS_Stations$USAF == selected_usaf]
    usaf_wban <- paste(selected_usaf, wbans, sep = "-")
    
    # Create a folder name that includes the identifier
    station_dir <- paste0(usaf_wban, "_ISH")
    
    # Remove the trailing hyphen
    station_dir <- gsub("-_", "_", station_dir)
    
    # Check if the directory exists and create it if not
    if (!dir.exists(station_dir)) {
      dir.create(station_dir, showWarnings = FALSE)
    }
    
    # Create a single output file for the selected years with the desired format
    combined_file_path <- file.path(station_dir, paste0(usaf_wban, "_", start_year, "_", end_year, ".ish"))
    
    years <- start_year:end_year
    
    for (year in years) {
      url <- paste0("https://www1.ncdc.noaa.gov/pub/data/noaa/", year, "/", selected_usaf, "-", wbans, year, ".gz")
      filename <- file.path(station_dir, paste0(selected_usaf, "-", year, ".gz"))
      
      download.file(url, destfile = filename, mode = "wb")
      
      # Append the downloaded data to the combined file after gunzipping
      gunzip(filename)
      data <- readLines(gsub(".gz$", "", filename)) # Remove the '.gz' extension
      write(data, combined_file_path, append = TRUE)
    }
  })
  
  
  observeEvent(input$map_ish_marker_click, {
    event <- input$map_ish_marker_click
    if (!is.null(event)) {
      clicked_station <- ASOS_Stations[
        ASOS_Stations$LAT == event$lat & ASOS_Stations$LON == event$lng,
      ]
      if (!is.null(clicked_station)) {
        selected_usaf <- paste(clicked_station$USAF, clicked_station$WBAN, sep = "-")
        updateSelectInput(session, "station_ish", selected = selected_usaf)
      }
    }
  })
  
  output$map_ish <- renderLeaflet({
    leaflet(data = ASOS_Stations[ASOS_Stations$STATE == input$state_ish, ]) %>%
      addTiles() %>%
      addMarkers(
        lat = ~LAT,
        lng = ~LON,
        label = ~ICAO,
        group = "ASOS Stations"
      )
  })
}

shinyApp(
  ui = fluidPage(
    titlePanel("AERMET NOAA Data Downloaders"),
    tabsetPanel(
      tabPanel("ASOS Data", asos_ui),
      tabPanel("ISH Data", ish_ui)
    )
  ),
  server = function(input, output, session) {
    ASOS_Stations <- read.csv("ASOS_Stations.csv")
    asos_server(input, output, session)
    ish_server(input, output, session)
  }
)
