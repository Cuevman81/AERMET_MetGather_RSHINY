# Enhanced NOAA ASOS and ISH Data Downloader (R Shiny App)

## Overview

The Enhanced NOAA ASOS and ISH Data Downloader is an R Shiny web application designed to help users select and download historical weather data from the National Oceanic and Atmospheric Administration (NOAA). It focuses on:

- **Automated Surface Observing System (ASOS) data**: 1-minute and 5-minute resolution.
- **Integrated Surface Hourly (ISH) data**: Original gzipped fixed-format from the NOAA pub/data/noaa/ archive.

A key improvement in this version is its ability to dynamically fetch and process the official NOAA master station list (MASTER-STN-HIST.TXT) on startup. This ensures the app uses up-to-date station information and eliminates the need for a local ASOS_Stations.csv file. The app intelligently filters this master list to identify suitable ASOS-type stations with the correct identifiers for data retrieval.

## Key Features

- **Dynamic Station List**: Automatically fetches and parses the comprehensive NOAA master station list at launch.

- **Intelligent Station Filtering**:
  - Identifies and prioritizes ASOS-type stations for data downloads.
  - Ensures stations have valid ICAO, WMO, and WBAN identifiers necessary for the different NOAA data services.

- **ASOS Data Downloads**:
  - Downloads 1-minute and 5-minute ASOS data for selected stations and year ranges.
  - Select ASOS stations by filtering by state and ICAO.
  - Visualize ASOS stations on an interactive map.

- **ISH Data Downloads**:
  - Downloads Integrated Surface Hourly (ISH) data in its original gzipped fixed-format (often referred to as "DS3505" or "global hourly").
  - Uses the correct WMO and WBAN identifiers (e.g., `722350-03940-YYYY.gz`) for fetching from the https://www1.ncdc.noaa.gov/pub/data/noaa/ archive.
  - Combines downloaded ISH data for multiple years of a single station into one `.ish` text file.
  - Select ISH stations by filtering by state and ICAO (the app then uses the associated WMO/WBAN for download).
  - Visualize ISH-capable stations on an interactive map.

- **Organized Data Storage**:
  - Creates a main folder for each selected station (named by its ICAO code, e.g., `KJAN/`).
  - Within the station's main folder, data is organized into subdirectories:
    - ASOS data: `[ICAO]/[Year]/asos_data_1min/` and `[ICAO]/[Year]/asos_data_5min/`
    - ISH data: `[ICAO]/ish_gz_data/`

- **User-Friendly Interface**:
  - Interactive maps for station selection.
  - Sliders for selecting start and end years.
  - Progress bars and status messages during download operations.
  - Loading indicator during the initial station list fetch.

- **Data Management**:
  - Buttons to clear downloaded ASOS or ISH data for selected stations/year ranges.

## How It Works

1. **Station List Initialization**: On startup, the app downloads and processes the `MASTER-STN-HIST.TXT` file from NOAA. It filters this list to find operational ASOS stations in the US that have valid ICAO, WMO, and WBAN identifiers.

2. **User Selection**: The user selects a data type (ASOS or ISH), filters by state, and then selects a specific station (by ICAO) using a dropdown or by clicking on a map. A year range for data download is also chosen.

3. **Data Download**:
   - **ASOS**: The app constructs URLs using the selected ICAO, year, and month to download 1-minute and 5-minute `.dat` files.
   - **ISH**: The app uses the WMO and WBAN IDs associated with the selected ICAO to construct URLs for the gzipped fixed-format ISH files. These files are downloaded, unzipped, and their content is appended to a combined text file for the station.

4. **Output**: Data is saved to the local filesystem in the structured directories described above, relative to the app's working directory.

## Usage

1. **Launch the App**: Open the `app.R` file in RStudio and click "Run App", or use `shiny::runApp()` pointing to the app's directory.

2. **Initial Load**: Wait for the app to fetch and process the station list (a loading message will be displayed). This may take a minute or two.

3. **Select Data Tab**: Choose either the "ASOS Data" or "ISH Data" tab.

4. **Filter by State**: Select a state from the dropdown to populate the station list and map.

5. **Select Station**:
   - Choose a station from the "Select Station (by ICAO)" dropdown.
   - Alternatively, click on a station marker on the interactive map.

6. **Select Year Range**: Use the sliders to define the start and end years for data download.

7. **Download Data**: Click the "Download ASOS Data" or "Download ISH Data" button. Monitor the progress bar and status messages. Console logs provide detailed information on successes or failures (e.g., HTTP 404 if data is not found).

8. **Clear Data (Optional)**: Use the "Clear Downloaded Data" buttons to remove previously downloaded files for the selected station and parameters.

## Installation and Setup

1. **Clone Repository**: Clone this GitHub repository to your local machine or download the source code.

2. **Install R and RStudio**: Ensure you have a recent version of R and RStudio (recommended) installed.

3. **Install Required R Packages**: Open R or RStudio and run the following command in the console to install the necessary packages:

   ```r
   install.packages(c("shiny", "httr", "leaflet", "R.utils", "dplyr", "readr", "stringr", "shinyjs"))
   ```

4. **Set Working Directory (if needed)**: Ensure your R session's working directory is set to the folder containing `app.R`. RStudio typically does this automatically when you open the file.

5. **Run the App**:
   - In RStudio, open `app.R` and click the "Run App" button in the RStudio interface.
   - Alternatively, run `shiny::runApp()` in the R console from the app's directory.

6. **Internet Connection**: A stable internet connection is required for the initial station list download and for all subsequent data downloads from NOAA.

## Data Sources

- **Master Station List**: [NOAA MASTER-STN-HIST.TXT](https://www.ncei.noaa.gov/pub/data/noaa/MASTER-STN-HIST.TXT)
- **ASOS 1-Minute Data**: [NOAA NCEI ASOS 1-Minute Data Access](https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/)
- **ASOS 5-Minute Data**: [NOAA NCEI ASOS 5-Minute Data Access](https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/) (Note: The app currently uses the 1-minute base URL structure; actual 5-minute data might be at a different specific path if direct 5-min files are targeted, but ASOS 1-min often contains 5-min summaries too).
  - App specifically uses: https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/ and https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/
- **ISH Data (DS3505 / Global Hourly)**: [NOAA NCEI ISH Data Archive](https://www1.ncdc.noaa.gov/pub/data/noaa/) (pub/data/noaa)

## Important Notes & Considerations

- **Internet Connection**: Required for fetching the master station list on startup and for all data downloads.

- **Initial Load Time**: The app fetches and processes a large station list on startup, which may take a minute or two. A loading indicator will be displayed.

- **Data Availability**: Not all stations have data for all years/months or for all data types (1-min, 5-min, ISH). HTTP 404 errors logged in the R console indicate data is not available from NOAA for the specific request.

- **ISH File Identifiers**: The app uses WMO IDs and WBAN IDs to construct filenames for ISH data downloads (e.g., `722350-03940-YYYY.gz`). The heuristic for WMO ID modification (e.g., appending '0' if it's 5 digits long) is based on observed patterns and might require adjustments for some stations if 404 errors persist for expected ISH files.

- **Output Directory**: Data is saved in subdirectories within the app's working directory, under a folder named after the station's ICAO code.

- **Error Checking**: While the app includes error handling, users should monitor the R console for detailed messages, especially if downloads fail or data seems incomplete.
