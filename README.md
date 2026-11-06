# Citi Bike Rebalancing Analysis

An end-to-end R and SQLite data engineering and analytics pipeline investigating Citi Bike demand dynamics, station capacity constraints, rebalancing requirements, weather effects, and public transit proximity across the New York metropolitan footprint for September 2024.

---

## Table of Contents

- [Objectives](#objectives)
- [Datasets & Sources](#datasets--sources)
- [Repository Structure](#repository-structure)
- [Environment & Technical Guidelines](#environment--technical-guidelines)
- [Pipeline Architecture & Run Order](#pipeline-architecture--run-order)
- [Database Architecture](#database-architecture)
- [Cleaning Audit & Validation Summary](#cleaning-audit--validation-summary)
- [Visualizations & Key Findings](#visualizations--key-findings)

---

## Objectives

1. **System Rebalancing & Flow Modeling**: Quantify net bike accumulation and depletion across 2,266 docking stations to identify persistent "drainers" (asymmetry < 0) and "collectors" (asymmetry > 0).
2. **Transit Multimodality via Spatial Analysis**: Calculate walking-shed proximity (EPSG:32618 UTM Zone 18N) from each Citi Bike station to MTA subway entrances (400-meter buffer counts and minimum Euclidean distance).
3. **Exogenous Driver & Panel Construction**: Construct a balanced station $\times$ 2-hour diurnal panel (797,040 rows) combining trip volume deltas, capacity pressure ratios, autoregressive lags, and contemporaneous weather conditions (temperature, precipitation, wind speed, relative humidity).
4. **Reproducible Production Pipeline**: Guarantee pure CRAN Windows binary compatibility on R 4.6.1 without external compiler toolchains (no Rtools), preserving memory efficiency through namespaced `data.table::fread()` ingest without attaching `data.table` to the search path.

---

## Datasets & Sources

| Dataset | Source Provider | Path | Scope & Volume |
|---|---|---|---|
| **Citi Bike Trips (NYC)** | [Citi Bike S3 Data Bucket](https://s3.amazonaws.com/tripdata/index.html) | `data/raw/trips/*.csv` | 5 monthly partition CSVs (`202409-citibike-tripdata_1.csv` through `_5.csv`) totaling 4,997,898 raw records (4,708,761 cleaned). |
| **GBFS Station Metadata** | [Citi Bike GBFS API](https://gbfs.citibikenyc.com/gbfs/en/station_information.json) | `data/raw/station_information.csv` | Live dock station coordinates, names, short names, and physical dock capacities (2,518 stations). |
| **Historical Weather** | [Open-Meteo Archive API](https://archive-api.open-meteo.com/v1/archive) | `data/raw/weather_sept2024.csv` | Hourly observations for NYC (Lat 40.7128, Lon -74.0060) across September 2024 (720 hourly rows). |
| **MTA Subway Entrances** | [New York State Open Data](https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD) | `data/raw/subway_entrances.csv` | Precise coordinates, entrance types, and routes for 2,120 subway entrances across NYC. |

---

## Repository Structure

```
citibike-rebalancing/
├── .gitignore                      # Excludes data/raw, data/processed, *.sqlite, and R session files
├── README.md                       # Comprehensive system documentation and run guide
├── data/
│   ├── .gitkeep
│   ├── citibike.sqlite             # Central SQLite database (8 tables; ignored by git)
│   ├── processed/                  # Heavy cached RDS artifacts (ignored by git)
│   │   ├── panel.rds               # Station x 2-hour balanced panel (797,040 rows)
│   │   └── trips_clean.rds         # Cleaned trip records (4,708,761 rows)
│   └── raw/                        # Ingested raw datasets (ignored by git)
│       ├── trips/                  # 5 official NYC September 2024 trip partition files
│       ├── station_information.csv # GBFS station metadata
│       ├── subway_entrances.csv    # MTA subway entrances extract
│       └── weather_sept2024.csv    # Open-Meteo hourly weather observations
├── docs/                           # Documentation, notes, and project specs
├── presentation/                   # Slide decks and executive presentations
├── results/
│   ├── figures/                    # High-resolution ggplot2 figures (200 dpi)
│   │   ├── bottleneck_heatmap_top25.png      # Top 25 stations x 24-hour departure matrix
│   │   ├── distance_duration_gam.png         # Distance vs. duration with GAM smooth (30k sample)
│   │   ├── duration_member_casual_boxplot.png# Duration boxplot by rider type on log scale
│   │   ├── heatmap_dow_hour.png              # Day of week x hour citywide trip density
│   │   └── hourly_departure_ridges.png       # Ridge density of departures (weekday vs. weekend)
│   ├── maps/                       # Standalone self-contained HTML widgets
│   │   ├── hourly_demand_weather.html        # Interactive Plotly dual-axis demand & weather chart
│   │   ├── station_asymmetry.html            # Leaflet choropleth map of station asymmetry index
│   │   └── station_heatmaps.html             # Multi-layer Leaflet heatmap (departures, arrivals, net flow)
│   └── tables/                     # CSV audit logs and summary metrics
│       ├── cleaning_audit.csv      # Sequential attrition audit across cleaning stages
│       ├── eda_summary.csv         # Distribution metrics (mean, median, sd, min, max, skewness)
│       ├── missingness_raw.csv     # Raw column-level missing value breakdown
│       └── station_features.csv    # Station-level flow asymmetry, capacity, and subway proximity
└── R/
    ├── 00_setup.R                  # Dependency checks, CRAN binary verification, directory init
    ├── 01_fetch_apis.R             # API fetchers for GBFS, Open-Meteo, and MTA datasets
    ├── 02_build_database.R         # Fast multi-file SQLite ingest with explicit colClasses
    ├── 03_clean_features.R         # Cleaning pipeline, SQL flows, sf subway proximity, 2h panel
    ├── 04_eda_heatmaps.R           # Leaflet interactive maps, ggplot2 visual exports, Plotly charts
    └── utils.R                     # Vectorized Haversine distance, Tukey IQR filter, skewness
```

---

## Environment & Technical Guidelines

This project conforms strictly to the following engineering constraints:
- **R Version**: R 4.6.1 on Windows.
- **No Rtools Dependency**: Exclusively utilizes pre-compiled CRAN Windows binaries (`type = "binary"`).
- **Explicit Function Namespacing**: Any functions with potential namespace collisions are explicitly prefixed (`lubridate::hour`, `lubridate::wday`, `dplyr::filter`, `dplyr::select`, `dplyr::mutate`, `dplyr::bind_rows`, `sf::st_as_sf`, `sf::st_distance`).
- **Unattached `data.table`**: `data.table` is **never attached** to the R search path with `library()`. High-speed reads use `data.table::fread()` directly with explicit column types.
- **Cache Persistence**: Heavy intermediate and final datasets are serialized as RDS binaries in `data/processed/` (git-ignored) for zero-latency downstream consumption.
- **Functional Modularity**: Every script is structured as small, documented functions accompanied by a dedicated runner block at the bottom.

---

## Pipeline Architecture & Run Order

To execute the complete pipeline from scratch:

```powershell
# Step 1: Environment verification & directory initialization
Rscript R/00_setup.R

# Step 2: Ingest external APIs (GBFS station metadata, Open-Meteo weather, MTA subway entrances)
Rscript R/01_fetch_apis.R

# Step 3: Populate central SQLite database with raw trips and external tables
Rscript R/02_build_database.R

# Step 4: Run sequential cleaning, SQL station aggregations, sf proximity, and panel construction
Rscript R/03_clean_features.R

# Step 5: Render interactive Leaflet maps, high-res figures, Plotly charts, and summary tables
Rscript R/04_eda_heatmaps.R
```

---

## Database Architecture

The SQLite database at `data/citibike.sqlite` manages 8 relational tables:

| Table Name | Row Count | Primary Key / Index Keys | Description |
|---|---|---|---|
| `trips` | 4,997,898 | `ride_id` | Raw ingested transaction records from the 5 NYC partition CSVs. |
| `trips_clean` | 4,708,761 | `ride_id` | Validated trips post-timestamp parsing, IQR duration, and speed filtering. |
| `stations` | 2,518 | `short_name` | Live GBFS docking station locations, capacities, and region identifiers. |
| `weather` | 720 | `time` | Hourly meteorological observations for NYC (September 2024). |
| `subway` | 2,120 | `station_id`, `line` | MTA subway entrance coordinates, lines, and complex IDs. |
| `station_features` | 2,266 | `station_id` | Station departures, arrivals, net flow, asymmetry index, capacity, and subway proximity. |
| `panel` | 797,040 | `station_id`, `window_start` | Balanced 2-hour time-series panel (2,214 stations $\times$ 360 bins) with deltas, pressure, lags, and weather. |
| `hourly_demand` | 720 | `hour_start` | Citywide hourly demand split by member vs. casual riders, merged with contemporaneous weather. |

---

## Cleaning Audit & Validation Summary

Raw trips are cleaned through a strict 7-stage deterministic filter (`results/tables/cleaning_audit.csv`):

| Step | Stage Description | Rows Remaining | Rows Dropped | % Retained |
|---|---|---|---|---|
| **0** | **Raw Ingested Records** | **4,997,898** | - | 100.00% |
| **1** | Parse Timestamps (`America/New_York`) | 4,997,898 | 0 | 100.00% |
| **2** | Drop Missing Coordinates & Station IDs | 4,981,399 | 16,499 | 99.67% |
| **3** | Drop Invalid Chronology (`started_at >= ended_at`) | 4,981,399 | 0 | 99.67% |
| **4** | Drop Non-positive Duration (`trip_duration_mins <= 0`) | 4,981,399 | 0 | 99.67% |
| **5** | Tukey IQR Duration Filter ($[Q_1 - 1.5\text{IQR},\, Q_3 + 1.5\text{IQR}]$) | 4,708,792 | 272,607 | 94.22% |
| **6** | Compute Haversine Distance & Speed | 4,708,792 | 0 | 94.22% |
| **7** | Drop Implausible Speed ($> 45\text{ km/h}$) | **4,708,761** | 31 | **94.21%** |

### Statistical Summary of Cleaned Metrics (`results/tables/eda_summary.csv`)

| Metric | Sample $n$ | Mean | Median | Std. Dev. | Min | Max | Skewness |
|---|---|---|---|---|---|---|---|
| **Trip Duration (mins)** | 4,708,761 | 10.93 | 9.08 | 7.16 | 1.00 | 32.87 | 0.96 |
| **Trip Distance (km)** | 4,708,761 | 1.94 | 1.52 | 1.49 | 0.00 | 14.16 | 1.49 |
| **Trip Speed (km/h)** | 4,708,761 | 10.96 | 10.90 | 4.11 | 0.00 | 35.50 | -0.09 |
| **Hourly Demand (trips/h)** | 720 | 6,539.03 | 6,327.00 | 4,653.96 | 253.00 | 18,518.00 | 0.46 |

---

## Visualizations & Key Findings

1. **Station Imbalance & Asymmetry**:
   - `results/maps/station_heatmaps.html`: Standalone Leaflet map featuring layer controls to toggle between aggregate departures, arrivals, drainer intensity (negative net flow), and collector intensity (positive net flow).
   - `results/maps/station_asymmetry.html`: Interactive choropleth visualizing station asymmetry index with popups reporting physical dock capacity, subway proximity within 400 meters, and net trip differential.
2. **Temporal Commuter Dynamics**:
   - `results/figures/hourly_departure_ridges.png`: Shows distinct bimodal weekday commuter peaks (08:00 morning rush, 17:00-18:00 evening rush) contrasted with unimodal midday leisure curves on weekends.
   - `results/figures/bottleneck_heatmap_top25.png`: Details diurnal departure bottlenecks across the 25 busiest Manhattan and Brooklyn stations.
   - `results/figures/heatmap_dow_hour.png`: Citywide heatmap demonstrating trip density across days of the week and hours of the day.
3. **Behavioral & Kinematic Relationships**:
   - `results/figures/duration_member_casual_boxplot.png`: Log-scale comparison confirming casual users exhibit significantly longer median trip durations than annual commuter members.
   - `results/figures/distance_duration_gam.png`: Cubic spline GAM smooth capturing the non-linear relationship between trip distance and travel duration.
   - `results/maps/hourly_demand_weather.html`: Dual-axis Plotly time-series tracking total and member trip volume fluctuations against temperature and rain events across September 2024.
