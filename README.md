# AERMET_GATHER_RSHINY
An RSHINY app to acquire AERMET data from various ASOS Stations.

ASOS and ISH Data Downloader

Overview:
The ASOS and ISH Data Downloader is a Shiny web application that allows users to download historical weather data from the Automated Surface Observing System (ASOS) and International Surface Hourly (ISH) data sources provided by the National Oceanic and Atmospheric Administration (NOAA). Users can select specific ASOS stations, choose the time range, and download data in various formats.  Make sure you have downloaded or copied ASOS_Stations.csv and ASOS_Stations <- ASOS_Stations in Environment.

Features:

    Select ASOS stations by filtering by state.
    Visualize ASOS stations on an interactive map.
    Download one-minute and five-minute ASOS data for selected stations and years.
    Clear downloaded ASOS data for specific stations and years.
    Select ISH stations by filtering by state.
    Visualize ISH stations on an interactive map.
    Download ISH data for selected stations and years.
    Automatically create output directories for downloaded data.
    Combine downloaded ISH data for multiple years into a single file.

Usage:

    Choose the state to filter ASOS or ISH stations.
    Click on the map markers to select a specific station.
    Select the start and end years.
    Download ASOS or ISH data for the selected station and time range.
    Clear downloaded ASOS data for specific stations and years.
    View status updates on data downloads.

Installation:

    Clone this GitHub repository to your local machine.
    Open R or RStudio.
    Run the following command to install required packages: install.packages(c("shiny", "httr", "leaflet", "R.utils", "dplyr")).
    Start the Shiny app by running the app.R file.

Data Source:

    ASOS data source: [NOAA ASOS Data](https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/)
    ISH data source: [NOAA ISH Data](https://www1.ncdc.noaa.gov/pub/data/noaa/)
