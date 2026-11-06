# Citi Bike Rebalancing Analysis

An end-to-end R and SQLite data engineering and analysis project investigating Citi Bike demand dynamics, station capacity constraints, rebalancing requirements, weather effects, and transit proximity across the New York metropolitan area.

---

## Table of Contents

- [Objectives](#objectives)
- [Datasets & Sources](#datasets--sources)
- [Repository Structure](#repository-structure)
- [Environment & Technical Guidelines](#environment--technical-guidelines)
- [Getting Started & Run Steps](#getting-started--run-steps)
- [Database Architecture](#database-architecture)
- [SQL Validation Queries](#sql-validation-queries)

---

## Objectives

1. **System Rebalancing Modeling**: Characterize bike supply-demand imbalances across docking stations to identify persistent empty-dock and full-dock bottleneck conditions.
2. **Transit Multimodality**: Quantify spatial interactions between Citi Bike stations and MTA subway entrances to evaluate first/last-mile transit connectivity.
3. **Exogenous Driver Analysis**: Assess the sensitivity of hourly trip volumes and dock turnover to meteorological variables (temperature, precipitation, wind speed, humidity).
4. **Reproducible Pipeline**: Establish a modular, automated ingestion and database pipeline using R 4.6.1 and SQLite without requiring external compiler toolchains (no Rtools).

---

## Datasets & Sources

| Dataset | Source URL / Provider | Local Path | Description |
|---|---|---|---|
| **Citi Bike Trip Data** | [Citi Bike S3 Data Bucket](https://s3.amazonaws.com/tripdata/index.html) | `data/raw/trips/*.csv` | Individual trip records with timestamps, station IDs/names, coordinates, and membership type. |
| **Station Information** | [Citi Bike GBFS Feed](https://gbfs.citibikenyc.com/gbfs/en/station_information.json) | `data/raw/station_information.csv` | Live dock station metadata: `short_name`, `name`, `lat`, `lon`, `capacity`. |
| **Historical Weather** | [Open-Meteo Archive API](https://archive-api.open-meteo.com/v1/archive) | `data/raw/weather_sept2024.csv` | Hourly observations for NYC (Lat 40.7128, Lon -74.0060) across September 2024 (temp, precip, wind, humidity). |
| **MTA Subway Entrances** | [New York State Open Data](https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD) | `data/raw/subway_entrances.csv` | Geographic coordinates and lines for all NYC subway station entrances/exits. |

---

## Repository Structure

```
citibike-rebalancing/
├── .gitignore                      # Excludes raw data, SQLite databases, and R session state
├── README.md                       # Project documentation and run guide
├── data/
│   ├── .gitkeep                    # Retains data/ folder in version control
│   ├── citibike.sqlite             # Generated SQLite database (ignored by git)
│   └── raw/                        # Generated raw CSV extracts (ignored by git)
│       ├── trips/                  # Raw trip CSV files (e.g., September 2024 data)
│       ├── station_information.csv # GBFS station metadata
│       ├── subway_entrances.csv    # MTA subway entrances extract
│       └── weather_sept2024.csv    # Open-Meteo September 2024 hourly weather
├── docs/                           # Documentation, project specs, and notes
├── presentation/                   # Slide decks, executive summaries, and visual assets
├── results/                        # Generated plots, reports, and analytical outputs
└── R/
    ├── 00_setup.R                  # Verifies packages, manages namespaces, creates folders
    ├── 01_fetch_apis.R             # API fetchers for GBFS, Open-Meteo, MTA, and trip data
    └── 02_build_database.R         # Ingestion into SQLite with data.table::fread and DBI queries
```

---

## Environment & Technical Guidelines

This project conforms strictly to the following constraints:
- **R Version**: R 4.6.1 on Windows.
- **No Rtools Dependency**: Uses pre-compiled CRAN Windows binaries (`type = "binary"`) exclusively.
- **Strict Package Policy**: Any missing CRAN Windows binary triggers an immediate stop without silent substitution.
- **Explicit Namespacing**: Conflicting functions are explicitly namespaced across all scripts (`lubridate::hour`, `lubridate::wday`, `dplyr::filter`, `dplyr::select`, `dplyr::mutate`).
- **Unattached `data.table`**: `data.table` is **never attached** to the R search path with `library(data.table)`. All file reads utilize `data.table::fread()` directly with explicit `colClasses`.
- **Modular Function Structure**: Every script is structured as small, documented functions accompanied by an executable runner block at the bottom.

---

## Getting Started & Run Steps

All scripts can be executed via terminal using `Rscript` or sourced inside an interactive R session:

### Step 1: Initialize Setup and Verify Environment
```powershell
Rscript R/00_setup.R
```
- Installs any missing packages as CRAN Windows binaries.
- Loads required tidyverse/spatial/database packages.
- Verifies `data.table` namespace without attaching it to search path.
- Creates local directory hierarchy (`data/raw/trips/`, `docs/`, `results/`, `presentation/`).

### Step 2: Fetch External Datasets via APIs
```powershell
Rscript R/01_fetch_apis.R
```
- Fetches station metadata from Citi Bike GBFS API and writes `data/raw/station_information.csv`.
- Fetches September 2024 hourly weather from Open-Meteo API and writes `data/raw/weather_sept2024.csv`.
- Streams MTA subway entrances dataset to `data/raw/subway_entrances.csv`.
- Downloads and unzips September 2024 trip data to `data/raw/trips/`.

### Step 3: Ingest Data into SQLite & Run Validation Queries
```powershell
Rscript R/02_build_database.R
```
- Ingests all trip CSVs using `lapply` + `data.table::fread` with explicit `colClasses`.
- Ingests station, weather, and subway entrance datasets.
- Creates/overwrites tables in `data/citibike.sqlite`.
- Executes and prints 3 validation SQL queries.

---

## Database Architecture

The central SQLite database is stored at `data/citibike.sqlite` and comprises 4 relational tables:

1. **`trips`**:
   - `ride_id` (TEXT, PRIMARY KEY)
   - `rideable_type` (TEXT)
   - `started_at` (TEXT)
   - `ended_at` (TEXT)
   - `start_station_name` (TEXT)
   - `start_station_id` (TEXT)
   - `end_station_name` (TEXT)
   - `end_station_id` (TEXT)
   - `start_lat` (REAL), `start_lng` (REAL)
   - `end_lat` (REAL), `end_lng` (REAL)
   - `member_casual` (TEXT)

2. **`stations`**:
   - `short_name` (TEXT)
   - `name` (TEXT)
   - `lat` (REAL)
   - `lon` (REAL)
   - `capacity` (INTEGER)

3. **`weather`**:
   - `time` (TEXT)
   - `temperature_2m` (REAL)
   - `precipitation` (REAL)
   - `wind_speed_10m` (REAL)
   - `relative_humidity_2m` (INTEGER)

4. **`subway`**:
   - `division` (TEXT)
   - `line` (TEXT)
   - `borough` (TEXT)
   - `stop_name` (TEXT)
   - `entrance_latitude` (REAL)
   - `entrance_longitude` (REAL)
   - Sanitized entrance attributes and georeference info.

---

## SQL Validation Queries

When running `R/02_build_database.R`, the script automatically executes three validation queries via `DBI::dbGetQuery`:

### 1. Row Counts across Tables
```sql
SELECT 'trips' AS table_name, COUNT(*) AS row_count FROM trips
UNION ALL
SELECT 'stations' AS table_name, COUNT(*) AS row_count FROM stations
UNION ALL
SELECT 'weather' AS table_name, COUNT(*) AS row_count FROM weather
UNION ALL
SELECT 'subway' AS table_name, COUNT(*) AS row_count FROM subway;
```

### 2. Trips Per Day (September 2024)
```sql
SELECT
  substr(started_at, 1, 10) AS trip_date,
  COUNT(*)                  AS trip_count
FROM trips
WHERE started_at IS NOT NULL AND started_at != ''
GROUP BY substr(started_at, 1, 10)
ORDER BY trip_date ASC;
```

### 3. Top 10 Start Stations by Trip Volume
```sql
SELECT
  start_station_name,
  COUNT(*) AS trip_count
FROM trips
WHERE start_station_name IS NOT NULL AND start_station_name != ''
GROUP BY start_station_name
ORDER BY trip_count DESC
LIMIT 10;
```
