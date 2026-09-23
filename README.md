# AERMET NOAA Data Downloader (R Shiny)

An R Shiny app that gathers the raw NOAA surface and upper‑air data needed to
build AERMOD‑ready meteorological files with the MDEQ **AERMET** pipeline. It
pulls three things, one per tab:

| Tab | Data | Feeds |
|-----|------|-------|
| **ASOS 1/5‑min Winds** | 1‑minute & 5‑minute ASOS observations | **AERMINUTE** (hourly winds & calms) |
| **GHCNh Surface** | GHCNh hourly surface data (`.psv`) | **AERMET Stage 1** surface observations |
| **Upper Air (IGRA)** | IGRA2 radiosonde soundings | **AERMET** upper‑air / profile (`.PFL`) |

It is the front‑end companion to MDEQ's `AERMET.R` processing script (kept
outside this repo; the same engine is public as
[aermet-runner](https://github.com/Cuevman81/aermet-runner)'s `backend/engine.R`).
The GHCNh files it produces are named like the ones `AERMET.R`'s
`download_ghcnh()` writes and are quality‑screened the same way (see
[Quality screen](#quality-screen-ghcnh)), so they drop straight into a station run.
The ASOS and upper‑air files keep their own names (`AERMET.R` looks for
`64050<ICAO>YYYYMM.dat` / `64010<ICAO>YYYYMM.dat` and `<UA>yy-yyUA.txt`), so
`AERMET.R` downloads its own copies of those instead of reusing them.

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
  reality that operating ASOS stations show an END date in late August 2025
  (`20250825`–`20250827`, when ISHD stopped updating) rather than the old
  `99991231` sentinel.
- **All hosts moved to `www.ncei.noaa.gov`.**
- **GHCNh output matches the pipeline:** `ICAO/ghcnh_data/<ICAO>_GHCNh_<startYr>_<endYr>.psv`.
- **Year sliders extend to the current year;** default range is the last five
  years (the AERMET 5‑year‑window workflow).
- **Sept 2026 fixes:** the GHCNh file is quality‑screened before it is saved;
  gaps are named instead of hidden; the first day of the following year is
  added so 31 Dec is complete in local standard time; the station list no
  longer depends on today's date (`isd-history.csv` has been frozen since Aug
  2025); the upper‑air list follows the selected years.

---

## How it works

1. **Startup** – the app downloads two NCEI lists: `isd-history.csv` (filtered to
   active US ASOS stations with a valid WBAN, for the surface tabs) and
   `igra2-station-list.txt` (all US radiosonde sites, for the upper‑air tab,
   which lists those with soundings in the selected years). If the surface
   fetch fails it falls back to the bundled
   `ASOS_Stations.csv`.
2. **Select** – pick a state (defaults to **MS**), then a station from the
   dropdown or by clicking a map marker, and a start/end year.
3. **Download**
   - **ASOS:** `…/automated-surface-observing-system-one-minute-pg1/access/YYYY/MM/asos-1min-pg1-<ICAO>-YYYYMM.dat` (and the 5‑minute equivalent).
     Months NCEI does not have (HTTP 404) and months that failed to download
     are listed by name; a missing month of 1‑minute winds means AERMINUTE falls
     back to hourly winds for it.
   - **GHCNh:** the station id is assembled as `USW000` + zero‑padded WBAN
     (e.g. KJAN WBAN `03940` → `USW00003940`); each year's
     `…/global-historical-climatology-network/hourly/access/by-year/YYYY/psv/GHCNh_<id>_YYYY.psv`
     is downloaded and concatenated (header kept once), plus the first day of
     the following year (to `<endYr+1>-01-02 12:00 UTC`): GHCNh hours are UTC,
     so the evening of 31 Dec in local standard time is in the next year's
     file. A year NCEI does not have (HTTP 404) is named and the file is flagged
     as incomplete; any other failure (timeout, server error) writes nothing.
     Each request is retried once. The joined file is then quality‑screened.
     Puerto Rico / Virgin Islands airports and a few other sites whose
     `USW000`+WBAN id does not exist are looked up by ICAO in NCEI's
     `ghcnh-station-list.csv`.
   - **Upper Air:** the station's full period‑of‑record zip
     (`…/igra/data/data-por/<IGRA_ID>-data.txt.zip`, which can be ~100 MB) is
     downloaded, and its soundings are **trimmed to the selected year range**
     (matching how `AERMET.R` prepares the upper‑air input). The status shows
     soundings per year and names any month with none (e.g. a launch
     suspension). The station list shows the sites with soundings in the
     selected years, so retired sites are available for past windows.
4. **Output** (relative to the app's working directory):

   ```
   <ICAO>/
     <YEAR>/asos_data_1min/<ICAO>_YYYYMM_1min.dat
     <YEAR>/asos_data_5min/<ICAO>_YYYYMM_5min.dat
     ghcnh_data/<ICAO>_GHCNh_<startYr>_<endYr>.psv
     ghcnh_data/<ICAO>_GHCNh_<startYr>_<endYr>_qc_log.txt
   upper_air/<IGRA_ID>_UA_<startYr>_<endYr>.txt
   ```

---

## Quality screen (GHCNh)

AERMET reads a GHCNh `.psv` as delivered and does not act on NCEI's per‑value
quality codes, so the app screens the joined file with the same
`filter_ghcnh_quality()` that `AERMET.R` runs after its own download. Values
are only ever **blanked** (AERMET then treats them as missing); no record is
dropped and nothing is altered or invented. Three screens:

1. **NCEI quality codes** 2/6 (suspect) and 3/7 (erroneous), for every element.
2. **Decoded wind speed vs. the METAR text** carried in `REM` (more than 5 m/s
   apart), which catches decoder errors NCEI flags as good.
3. **Short SYNOPs with no wind group.** When an FM‑12 SYNOP reports cloud base
   and visibility missing and omits the Nddff group, NCEI reads the next group
   (time, pressure, temperature…) as the wind and cloud cover; those wind,
   sky‑condition and ceiling values are blanked.

Every blanked value is listed in `<file>_qc_log.txt` beside the `.psv`. The
screen is idempotent, so `AERMET.R` re‑screening the file changes nothing.

---

## Install & run

```r
install.packages(c("shiny", "httr", "leaflet", "dplyr", "readr", "stringr", "shinyjs"))
shiny::runApp("ASOS_Met_Gather_Shiny.R")
```

Or open `ASOS_Met_Gather_Shiny.R` in RStudio and click **Run App**. A stable
internet connection is required for the station list and all downloads.

Last tested (Sept 2026) with R 4.6.1, shiny 1.14.0, httr 1.4.9, leaflet 2.2.3,
dplyr 1.2.1, readr 2.2.0, stringr 1.6.0 and shinyjs 2.1.1. Package versions are
not pinned (there is no `renv.lock`).

> The GHCNh id is built as `USW000`+WBAN, which is the correct convention for
> airport ASOS stations (the app's target). When NCEI does not list that id
> (Puerto Rico and Virgin Islands airports use `RQW`/`VQW`; a few sites have
> ICAO‑based ids), the app looks the ICAO up in NCEI's GHCNh station list.

---

## Data sources

- **Surface station metadata:** <https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv>
- **ASOS 1‑minute:** <https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/>
- **ASOS 5‑minute:** <https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/>
- **GHCNh hourly:** <https://www.ncei.noaa.gov/oa/global-historical-climatology-network/index.html#hourly/access/by-year/>
  (documentation and station list: <https://www.ncei.noaa.gov/oa/global-historical-climatology-network/index.html#hourly/doc/>)
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
