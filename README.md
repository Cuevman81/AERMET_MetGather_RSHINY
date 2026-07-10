# AERMET NOAA Data Downloader (R Shiny)

An R Shiny app that gathers the raw NOAA surface data needed to build
AERMOD‑ready meteorological files with the MDEQ **AERMET** pipeline. It pulls
two things, one per tab:

| Tab | Data | Feeds |
|-----|------|-------|
| **ASOS 1/5‑min Winds** | 1‑minute & 5‑minute ASOS observations | **AERMINUTE** (hourly winds & calms) |
| **GHCNh Surface** | GHCNh hourly surface data (`.psv`) | **AERMET Stage 1** surface observations |
| **Upper Air (IGRA)** | IGRA2 radiosonde soundings | **AERMET** upper‑air / profile (`.PFL`) |

It is the front‑end companion to the [`AERMET.R`](../AERMINUTE/AERMET.R)
processing script: the GHCNh files it produces are named exactly like the ones
`AERMET.R`'s `download_ghcnh()` writes, so they drop straight into a station run.

---

## What changed in the 2025/2026 update

- **ISHD / DS3505 retired → GHCNh.** NCEI stopped updating the Integrated
  Surface Hourly (ISHD / DS3505) archive in **August 2025**. The old **ISH tab**
  (which downloaded gzipped `<USAF>-<WBAN>-YYYY.gz` files from
  `www1.ncdc.noaa.gov/pub/data/noaa/`) has been replaced by a **GHCNh tab** that
  downloads the pipe‑delimited by‑year files
  (`GHCNh_<id>_<YYYY>.psv`) NCEI now publishes — the same source the AERMET
  pipeline uses.
- **Modern station list.** The station dropdown/map is now built from the NCEI
  **ISD‑history CSV** (`https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv`)
  instead of the legacy fixed‑width `MASTER-STN-HIST.TXT` on `www1.ncdc.noaa.gov`
  (that host now 301‑redirects). Active‑station filtering was fixed for the new
  reality that operating ASOS stations show an END date of `2025‑08‑27` (the
  ISHD retirement date) rather than the old `99991231` sentinel.
- **All hosts moved to `www.ncei.noaa.gov`.**
- **GHCNh output matches the pipeline:** `ICAO/ghcnh_data/<ICAO>_GHCNh_<startYr>_<endYr>.psv`.
- **Year sliders extend to the current year;** default range is the last five
  years (the AERMET 5‑year‑window workflow).

---

## How it works

1. **Startup** – the app downloads two NCEI lists: `isd-history.csv` (filtered to
   active US ASOS stations with a valid WBAN, for the surface tabs) and
   `igra2-station-list.txt` (filtered to active US radiosonde sites, for the
   upper‑air tab). If the surface fetch fails it falls back to the bundled
   `ASOS_Stations.csv`.
2. **Select** – pick a state (defaults to **MS**), then a station from the
   dropdown or by clicking a map marker, and a start/end year.
3. **Download**
   - **ASOS:** `…/automated-surface-observing-system-one-minute-pg1/access/YYYY/MM/asos-1min-pg1-<ICAO>-YYYYMM.dat` (and the 5‑minute equivalent).
   - **GHCNh:** the station id is assembled as `USW000` + zero‑padded WBAN
     (e.g. KJAN WBAN `03940` → `USW00003940`); each year's
     `…/global-historical-climatology-network/hourly/access/by-year/YYYY/psv/GHCNh_<id>_YYYY.psv`
     is downloaded and concatenated (header kept once). Years with no data are
     skipped and reported rather than aborting the run.
   - **Upper Air:** the station's full period‑of‑record zip
     (`…/igra/data/data-por/<IGRA_ID>-data.txt.zip`, which can be ~100 MB) is
     downloaded, and its soundings are **trimmed to the selected year range**
     (matching how `AERMET.R` prepares the upper‑air input).
4. **Output** (relative to the app's working directory):

   ```
   <ICAO>/
     <YEAR>/asos_data_1min/<ICAO>_YYYYMM_1min.dat
     <YEAR>/asos_data_5min/<ICAO>_YYYYMM_5min.dat
     ghcnh_data/<ICAO>_GHCNh_<startYr>_<endYr>.psv
   upper_air/<IGRA_ID>_UA_<startYr>_<endYr>.txt
   ```

---

## Install & run

```r
install.packages(c("shiny", "httr", "leaflet", "dplyr", "readr", "stringr", "shinyjs"))
shiny::runApp("ASOS_Met_Gather_Shiny.R")
```

Or open `ASOS_Met_Gather_Shiny.R` in RStudio and click **Run App**. A stable
internet connection is required for the station list and all downloads.

> The GHCNh id is built as `USW000`+WBAN, which is the correct convention for
> airport ASOS stations (the app's target). A non‑USW station will simply return
> no GHCNh data, which the app reports.

---

## Data sources

- **Surface station metadata:** <https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv>
- **ASOS 1‑minute:** <https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/>
- **ASOS 5‑minute:** <https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/>
- **GHCNh hourly:** <https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year/>
- **Upper‑air station list:** <https://www.ncei.noaa.gov/pub/data/igra/igra2-station-list.txt>
- **Upper‑air soundings (IGRA2):** <https://www.ncei.noaa.gov/pub/data/igra/data/data-por/>

---

## Files

| File | Purpose |
|------|---------|
| `ASOS_Met_Gather_Shiny.R` | The Shiny app |
| `ASOS_Stations.csv` | Offline fallback station list (NCEI isd‑history schema) |
| `.gitignore` | Excludes downloaded data and R session files |

Downloaded station data is intentionally **not** committed (see `.gitignore`).
