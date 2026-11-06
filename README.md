# Citi Bike Rebalancing Analysis

An end-to-end R and SQLite data engineering, econometric modeling, spatial clustering, and time-series forecasting pipeline investigating Citi Bike demand dynamics, station capacity bottlenecks, rebalancing requirements, weather effects, and transit proximity across the New York metropolitan footprint for September 2024.

> **Final Comprehensive Report**:
> The fully rendered, self-contained HTML report with interactive data tables, high-resolution figures, and embedded spatial maps is available at:
> **[`docs/final_report.html`](docs/final_report.html)** (Open in any modern web browser).

> **Operational Data Measurement Note**:
> Throughout this project, station inventory stress is operationalized as **net-flow imbalance (arrivals minus departures in a 2-hour window)**. Citi Bike does not publish historical bike counts or dock occupancy per station over time, so true empty/full events cannot be observed directly from trip data.

---

## Table of Contents

- [Objectives](#objectives)
- [Datasets & Sources](#datasets--sources)
- [Repository Structure](#repository-structure)
- [Environment & Technical Guidelines](#environment--technical-guidelines)
- [Pipeline Architecture & Complete Run Order](#pipeline-architecture--complete-run-order)
- [Database Architecture](#database-architecture)
- [Phase Summary & Core Findings](#phase-summary--core-findings)
  - [Phase 1: Project Setup & Ingestion](#phase-1-project-setup--ingestion)
  - [Phase 2: Cleaning, Features & 2-Hour Panel](#phase-2-cleaning-features--2-hour-panel)
  - [Phase 3: Exploratory Data Analysis & Heatmaps](#phase-3-exploratory-data-analysis--heatmaps)
  - [Phase 4: Inferential Statistics & Negative Binomial Regression](#phase-4-inferential-statistics--negative-binomial-regression)
  - [Phase 5: Spatial Autocorrelation & Station Typology](#phase-5-spatial-autocorrelation--station-typology)
  - [Phase 6: Time-Series Decomposition & Forecasting](#phase-6-time-series-decomposition--forecasting)
  - [Phase 7: Machine Learning Benchmark & 2-Hour Rebalancing Forecasting](#phase-7-machine-learning-benchmark--2-hour-rebalancing-forecasting)
  - [Summary of Models and Methods](#summary-of-models-and-methods)

---

## Objectives

1. **System Rebalancing & Flow Modeling**: Quantify net bike accumulation and depletion across 2,266 docking stations to identify persistent "drainers" (asymmetry < 0) and "collectors" (asymmetry > 0).
2. **Transit Multimodality via Spatial Analysis**: Calculate walking-shed proximity (EPSG:32618 UTM Zone 18N) from each Citi Bike station to MTA subway entrances (400-meter buffer counts and minimum Euclidean distance).
3. **Diurnal Typology & Corridor Clustering**: Discover spatial density corridors using DBSCAN ($eps = 400$ m, $minPts = 5$) and establish diurnal station personalities through 24-hour departure share PCA and k-means clustering.
4. **Econometric & Weather Demand Analysis**: Model hourly trip count sensitivity to temperature, precipitation, wind speed, and humidity using overdispersed Negative Binomial GLMs.
5. **Time-Series Forecasting**: Benchmark out-of-sample multi-step forecasts (Seasonal Naïve, STL+ETS, Dynamic Harmonic Regression with Fourier terms) on a 7-day held-out test week.
6. **Production Standards**: Maintain strict CRAN Windows binary compatibility on R 4.6.1 without Rtools, zero-latency RDS caching, and complete functional modularity.

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
│   ├── citibike.sqlite             # Central SQLite database (9 tables; ignored by git)
│   ├── processed/                  # Heavy cached RDS artifacts (ignored by git)
│   │   ├── best_model.rds          # Fitted finalized Random Forest rebalancing workflow
│   │   ├── panel.rds               # Station x 2-hour balanced panel with clusters (797,040 rows)
│   │   ├── test_predictions.rds    # Full 9-day out-of-sample predictions (239,112 rows)
│   │   └── trips_clean.rds         # Cleaned trip records (4,708,761 rows)
│   └── raw/                        # Ingested raw datasets (ignored by git)
│       ├── trips/                  # 5 official NYC September 2024 trip partition files
│       ├── station_information.csv # GBFS station metadata
│       ├── subway_entrances.csv    # MTA subway entrances extract
│       └── weather_sept2024.csv    # Open-Meteo hourly weather observations
├── docs/                           # Documentation, notes, and rendered final report
│   ├── final_report.Rmd            # Comprehensive dynamic academic report R Markdown source
│   └── final_report.html           # Knitted self-contained HTML production report
├── presentation/                   # Slide decks and executive presentations
├── results/
│   ├── figures/                    # High-resolution figures (PNG, 200 dpi)
│   │   ├── acf_hourly_trips.png                      # 72-hour Autocorrelation Function (ACF)
│   │   ├── bottleneck_heatmap_top25.png              # Top 25 stations x 24-hour departure matrix
│   │   ├── confusion_matrices_all_models.png         # Faceted confusion matrix heatmaps across ML models
│   │   ├── correlation_heatmap_hourly_weather.png    # Spearman rank correlation matrix
│   │   ├── dbscan_knndist_elbow.png                  # 5-NN distance elbow plot (eps = 400m)
│   │   ├── distance_duration_gam.png                 # Distance vs. duration with GAM smooth (30k sample)
│   │   ├── duration_member_casual_boxplot.png        # Duration boxplot by rider type on log scale
│   │   ├── feature_importance_rf_xgb.png             # Native engine feature importance (RF Impurity vs XGB Gain)
│   │   ├── feature_selection.png                     # Multi-criteria feature selection consensus bar chart
│   │   ├── heatmap_dow_hour.png                      # Day of week x hour citywide trip density
│   │   ├── hourly_departure_ridges.png               # Ridge density of departures (weekday vs. weekend)
│   │   ├── irr_forest_plot.png                       # Incidence Rate Ratios (IRR) forest plot (95% CI)
│   │   ├── kmeans_personality_elbow_silhouette.png   # K-means silhouette by k (k = 2 to 6)
│   │   ├── model_comparison_roc_auc.png              # Macro ROC-AUC benchmark across 9 models + baseline
│   │   ├── moran_scatterplot.png                     # Standardized Moran scatterplot (I = 0.278)
│   │   ├── pca_scree_plot.png                        # PCA scree plot of 24h departure shares
│   │   ├── pr_curves_by_class.png                    # One-vs-rest Precision-Recall curves (Drainer & Collector)
│   │   ├── regression_asymmetry_diagnostics.png      # Residuals vs. Fitted & Normal Q-Q plots
│   │   ├── roc_curves_by_class.png                   # One-vs-rest ROC curves (Drainer & Collector)
│   │   ├── stl_decomposition.png                     # 4-panel STL decomposition (Observed/Trend/Season/Remainder)
│   │   ├── training_time_vs_roc_auc.png              # Pareto frontier scatter of training time vs. Macro ROC-AUC
│   │   └── tuning_results.png                        # Validation ROC-AUC across Latin Hypercube grid candidates
│   ├── maps/                       # Standalone self-contained HTML widgets
│   │   ├── dbscan_clusters.html                      # Interactive Leaflet map of 18 DBSCAN corridors
│   │   ├── hourly_demand_weather.html                # Interactive Plotly dual-axis demand & weather chart
│   │   ├── local_moran.html                          # Interactive Leaflet map of Local Moran (LISA) hotspots
│   │   ├── personality_hourly_profiles.html          # Interactive Plotly curves of diurnal personality profiles
│   │   ├── station_asymmetry.html                    # Leaflet choropleth map of station asymmetry index
│   │   ├── station_heatmaps.html                     # Multi-layer Leaflet heatmap (departures, arrivals, flows)
│   │   ├── station_personalities.html                # Leaflet map colored by station personality cluster
│   │   └── ts_forecast_comparison.html               # Interactive Plotly 7-day out-of-sample forecast comparison
│   └── tables/                     # CSV audit logs and summary metrics
│       ├── best_params.csv                     # Optimal tuned hyperparameters for top 2 models
│       ├── cleaning_audit.csv                  # Sequential attrition audit across cleaning stages
│       ├── cluster_silhouette_comparison.csv   # DBSCAN vs. K-means average silhouette width
│       ├── eda_summary.csv                     # Distribution metrics (mean, median, sd, min, max, skewness)
│       ├── feature_selection.csv               # Multi-criteria screening results (NZV/Corr, LASSO, RF Impurity)
│       ├── missingness_raw.csv                 # Raw column-level missing value breakdown
│       ├── ml_interpretation.md                # Comprehensive plain-English machine learning benchmark report
│       ├── model_ci.csv                        # 95% Bootstrap confidence intervals for top 2 models
│       ├── model_comparison.csv                # 9-model test benchmark metrics sorted by Macro ROC-AUC
│       ├── station_asymmetry_regression.csv    # OLS coefficients for transit/capacity asymmetry model
│       ├── station_features.csv                # Initial station flow metrics and subway proximity
│       ├── station_features_final.csv          # Enriched station features with DBSCAN and personality
│       ├── stats_interpretation.md             # Comprehensive plain-English statistical report
│       ├── stats_summary.csv                   # Formatted summary of non-parametric hypothesis tests
│       ├── ts_accuracy.csv                     # Time-series hold-out forecast accuracy (RMSE, MAE, MAPE)
│       ├── tuning_before_after.csv             # Performance comparison before and after hyperparameter tuning
│       ├── tuning_results.csv                  # Full validation results across 8 hyperparameter configurations
│       └── weather_nb_irr.csv                  # Negative Binomial regression coefficients & IRRs
└── R/
    ├── 00_setup.R                  # Dependency checks, CRAN binary verification, directory init
    ├── 01_fetch_apis.R             # API fetchers for GBFS, Open-Meteo, and MTA datasets
    ├── 02_build_database.R         # Fast multi-file SQLite ingest with explicit colClasses
    ├── 03_clean_features.R         # Cleaning pipeline, SQL flows, sf subway proximity, 2h panel
    ├── 04_eda_heatmaps.R           # Leaflet interactive maps, ggplot2 visual exports, Plotly charts
    ├── 05_statistics.R             # Inferential hypothesis tests, Spearman heatmap, Negative Binomial GLM
    ├── 06_spatial_clusters.R       # Moran's I, Local Moran LISA, DBSCAN, PCA station personalities
    ├── 07_time_series.R            # STL decomposition, 7-day holdout forecasting benchmark (snaive, stlf, ARIMA)
    ├── 08_ml_comparison.R          # Supervised ML benchmark, tuning, chunked test scoring, evaluation plots
    └── utils.R                     # Vectorized Haversine distance, Tukey IQR filter, skewness
```

---

## Environment & Technical Guidelines

This project conforms strictly to the following engineering constraints:
- **R Version**: R 4.6.1 on Windows.
- **No Rtools Dependency**: Exclusively utilizes pre-compiled CRAN Windows binaries (`type = "binary"`).
- **Explicit Function Namespacing**: Any functions with potential namespace collisions are explicitly prefixed (`lubridate::hour`, `lubridate::wday`, `dplyr::filter`, `dplyr::select`, `dplyr::mutate`, `dplyr::bind_rows`, `sf::st_as_sf`, `sf::st_distance`, `spdep::moran.test`, `forecast::auto.arima`).
- **Unattached `data.table`**: `data.table` is **never attached** to the R search path with `library()`. High-speed reads use `data.table::fread()` directly with explicit column types.
- **Cache Persistence**: Heavy intermediate and final datasets are serialized as RDS binaries in `data/processed/` (git-ignored) for zero-latency downstream consumption.
- **Functional Modularity**: Every script is structured as small, documented functions accompanied by a dedicated runner block at the bottom.

---

## Pipeline Architecture & Complete Run Order

Execute the end-to-end analytical pipeline in order:

```powershell
# Step 1: Environment verification & directory initialization
Rscript R/00_setup.R

# Step 2: Fetch external APIs (GBFS station metadata, Open-Meteo weather, MTA subway entrances)
Rscript R/01_fetch_apis.R

# Step 3: Populate central SQLite database with raw trips and external tables
Rscript R/02_build_database.R

# Step 4: Run sequential cleaning, SQL station aggregations, sf proximity, and panel construction
Rscript R/03_clean_features.R

# Step 5: Render interactive Leaflet heatmaps, high-res figures, Plotly charts, and summary tables
Rscript R/04_eda_heatmaps.R

# Step 6: Inferential hypothesis testing, Spearman correlations, and Negative Binomial regression
Rscript R/05_statistics.R

# Step 7: Spatial autocorrelation (Moran's I), DBSCAN corridor clustering, and PCA station personalities
Rscript R/06_spatial_clusters.R

# Step 8: Time-series STL decomposition, ACF diagnostics, and 7-day out-of-sample forecast benchmark
Rscript R/07_time_series.R

# Step 9: 9-Model ML benchmark, tuning, 100-replicate bootstrap validation, and chunked scoring
Rscript R/08_ml_comparison.R
```

---

## Database Architecture

The SQLite database at `data/citibike.sqlite` manages 9 relational tables:

| Table Name | Row Count | Primary Key / Index Keys | Description |
|---|---|---|---|
| `trips` | 4,997,898 | `ride_id` | Raw ingested transaction records from the 5 NYC partition CSVs. |
| `trips_clean` | 4,708,761 | `ride_id` | Validated trips post-timestamp parsing, IQR duration, and speed filtering. |
| `stations` | 2,518 | `short_name` | Live GBFS docking station locations, capacities, and region identifiers. |
| `weather` | 720 | `time` | Hourly meteorological observations for NYC (September 2024). |
| `subway` | 2,120 | `station_id`, `line` | MTA subway entrance coordinates, lines, and complex IDs. |
| `station_features` | 2,266 | `station_id` | Flows, asymmetry, capacity, subway proximity, `dbscan_cluster`, and `personality`. |
| `panel` | 797,040 | `station_id`, `window_start` | Balanced 2-hour panel (2,214 stations $\times$ 360 bins) with deltas, pressure, lags, weather, and clusters. |
| `hourly_demand` | 720 | `hour_start` | Citywide hourly demand split by member vs. casual riders, merged with contemporaneous weather. |
| `test_predictions` | 239,112 | `station_id`, `bin_start` | Out-of-sample predicted class, probabilities, delta, coordinates, and station metadata from best tuned model. |

---

## Phase Summary & Core Findings

### Phase 1: Project Setup & Ingestion
- Ingested 5 raw partition CSVs (`202409-citibike-tripdata_1.csv` through `_5.csv`) totaling 4,997,898 records.
- Verified top 10 start stations are all authentic Manhattan and Brooklyn hubs (e.g., W 21 St & 6 Ave, Broadway & E 14 St).

### Phase 2: Cleaning, Features & 2-Hour Panel
- Executed strict sequential filtering: missing coordinates/stations dropped 16,499 rows (0.33%), Tukey IQR duration filter dropped 272,607 rows (5.45%), and speed $> 45$ km/h dropped 31 rows, yielding **4,708,761 clean trips** (94.21% retention).
- Extracted flow metrics for 2,266 stations via DBI SQL `GROUP BY`.
- Projecting subway entrances to UTM Zone 18N (EPSG:32618) via `sf` resolved entrance counts within 400 meters and minimum distance.
- Constructed balanced station $\times$ 2-hour time-series panel (**797,040 rows** across 2,214 active stations $\ge 50$ trips).

### Phase 3: Exploratory Data Analysis & Heatmaps
- Generated 3 interactive self-contained HTML maps and 5 high-resolution ggplot figures (200 dpi).
- Confirmed marked bimodal weekday commute peaks (08:00 and 17:00-18:00) vs. unimodal weekend leisure plateaus.
- Casual riders exhibit longer median trip duration (11.82 min) than annual members (8.53 min).

### Phase 4: Inferential Statistics & Negative Binomial Regression
- **Normality**: Shapiro-Wilk ($W = 0.9172$, $p < 2.2 \times 10^{-16}$) and skewness ($0.96$) firmly reject normality for trip duration.
- **Rider Comparison**: Wilcoxon rank-sum ($W = 8.83 \times 10^9$, $p < 2.2 \times 10^{-16}$, rank-biserial $r = -0.250$); 2,000 bootstrap replicates establish that casual riders ride **3.00 to 3.68 minutes longer** (95% CI).
- **Overdispersion**: Poisson GLM dispersion ratio = 547.25, necessitating Negative Binomial modeling ($\theta = 7.191$, $\text{AIC} = 12,694.9$).
- **Weather Incidence Rate Ratios (IRRs)**:
  - **Temperature raises hourly demand by about 3.6% per degree C (significant, p < 0.001).** ($\text{IRR} = 1.036$, $p = 7.66 \times 10^{-9}$).
  - **Rain, wind and humidity showed no statistically significant effect in September 2024, a month with little rain; this is a limitation.** (Precipitation $p = 0.295$, wind speed $p = 0.310$, relative humidity $p = 0.139$; 95% CIs include 1.0).

### Phase 5: Spatial Autocorrelation & Station Typology
- **Global Moran's I**: $I = 0.2780$ ($p = 1.87 \times 10^{-168}$, Monte Carlo $p = 0.0010$ with 999 permutations), confirming strong spatial clustering of drainer and collector stations.
- **Local Moran (LISA)**: Identified major High-High collector clusters and Low-Low drainer corridors in Midtown and Lower Manhattan.
- **DBSCAN vs. K-Means Silhouette**:
  - The silhouette score favours round clusters and counts noise stations as a cluster, so it cannot judge DBSCAN fairly. DBSCAN was chosen because station corridors are not round and it can flag isolated stations as noise.
  - Average silhouette width on non-noise stations: **-0.0871** (18 corridors, 101 noise stations excluded).
  - Silhouette on all stations (including noise as cluster 0): **-0.1179**.
  - K-Means ($k=18$): **0.3815** (forces spherical Euclidean partitioning).
- **Diurnal Personalities**: PCA on 24-hour departure shares across 1,951 stations ($\ge 200$ trips) explained $51.8\%$ of variance across the first 4 PCs. K-means ($k = 4$) isolated 4 operational profiles:
  1. *Morning Commute Origin* (peak departure at 08:00)
  2. *Evening Commute Destination* (peak departure at 17:00-18:00)
  3. *Midday Leisure & Commerce* (peak departure between 11:00-15:00)
  4. *Balanced Diurnal Plateau* (uniform departures across daylight hours)
- **Asymmetry Regression**: $R^2 = 0.3068$ ($F = 310.1$, $p < 2.2 \times 10^{-16}$); both subway entrances within 400 m ($\beta = +0.00218$, $p = 1.57 \times 10^{-9}$) and distance to nearest subway ($\beta = +0.000189$, $p < 2.2 \times 10^{-16}$) significantly drive accumulation asymmetry.

### Phase 6: Time-Series Decomposition & Forecasting
- **STL Decomposition**: Isolated clear 24-hour diurnal seasonal oscillations; ACF confirms sharp harmonic spikes at lags 24, 48, and 72.
- **7-Day Hold-Out Benchmark (168 hours)**:
  1. **STL + ETS (`stlf`)**: $\text{RMSE} = 2,720.35$, $\text{MAE} = 2,028.14$, $\text{MAPE} = 73.51\%$.
  2. **Seasonal Naïve (`snaive`)**: $\text{RMSE} = 2,725.58$, $\text{MAE} = 1,709.62$, $\text{MAPE} = 50.86\%$.
  3. **Dynamic Harmonic ARIMA (Fourier $K = 4$)**: $\text{RMSE} = 2,958.44$, $\text{MAE} = 2,010.40$, $\text{MAPE} = 68.70\%$.
- **Model Commentary**: A forecast from the daily pattern alone is about as good as repeating last week; the extra signal must come from weather and station features, which is why the classification models use them. (STL+ETS and Seasonal Naïve are essentially tied on RMSE).

### Phase 7: Machine Learning Benchmark & 2-Hour Rebalancing Forecasting
- **Rebalancing Target Engineering**:
  - `Drainer` ($\Delta \le -5$): Station net inventory depletion requiring mobile rebalancing supply vans.
  - `Collector` ($\Delta \ge 5$): Station dock blockage risk requiring outbound corral van clearance.
  - `Balanced` ($-5 < \Delta < 5$): Self-regulating inventory equilibrium.
- **Strict Leakage Prevention**:
  - Station $\times$ hour historical mean delta and standard deviation computed strictly on the training period (2024-09-01 to 2024-09-21) and mapped forward.
  - Test period (2024-09-22 to 2024-09-30) evaluated purely with information available prior to window $t$, plus calendar and forecasted weather. Window $t$ arrivals, departures, delta, and pressure strictly excluded.
- **Class Imbalance & Recipe Architecture**:
  - Training pool (150,000 rows) balanced with `themis::step_downsample(under_ratio = 2)` during model prep; test set (40,000 benchmark sample) left in its natural class proportion (~85.8% Balanced, ~7.1% Drainer, ~7.1% Collector).
- **Benchmark Comparison (9 Supervised Classifiers + Baseline)**:
  1. **Random Forest (Ranger)**: $\text{Macro ROC-AUC} = \mathbf{0.8988}$, $\text{Bal Acc} = 0.7939$, $\text{Macro F1} = 0.5946$.
  2. **XGBoost (Extreme Gradient Boosting)**: $\text{Macro ROC-AUC} = 0.8890$ (Tuned: $\mathbf{0.8955}$), $\text{Bal Acc} = 0.7851$, $\text{Macro F1} = 0.5809$.
  3. **Neural Network (MLP)**: $\text{Macro ROC-AUC} = 0.8871$, $\text{Bal Acc} = 0.7821$, $\text{Macro F1} = 0.5758$.
  4. **Bagged Trees**: $\text{Macro ROC-AUC} = 0.8818$, $\text{Bal Acc} = 0.7832$, $\text{Macro F1} = 0.5739$.
  5. **Multinomial Logistic Regression (glmnet)**: $\text{Macro ROC-AUC} = 0.8790$, $\text{Bal Acc} = 0.7408$, $\text{Macro F1} = 0.6046$.
  6. **LDA (MASS)**: $\text{Macro ROC-AUC} = 0.8644$, $\text{Bal Acc} = 0.7049$, $\text{Macro F1} = 0.5892$.
  7. **KNN ($k=15$)**: $\text{Macro ROC-AUC} = 0.8275$, $\text{Bal Acc} = 0.7234$, $\text{Macro F1} = 0.5365$.
  8. **Decision Tree (CART)**: $\text{Macro ROC-AUC} = 0.8172$, $\text{Bal Acc} = 0.7881$, $\text{Macro F1} = 0.5301$.
  9. **Naive Bayes**: $\text{Macro ROC-AUC} = 0.7855$, $\text{Bal Acc} = 0.5567$, $\text{Macro F1} = 0.1768$.
  10. **Baseline (Null Model)**: $\text{Macro ROC-AUC} = 0.5000$, $\text{Bal Acc} = 0.5000$, $\text{Macro PR-AUC} = 0.3333$, $\text{Macro F1} = 0.3079$ (Balanced F1 = 0.9238, Drainer/Collector Recall = 0.0%, Raw Accuracy = 85.84%).
  11. *SVM RBF (kernlab)*: Optimization line search failure during multiclass Platt scaling sigmoid calibration; recorded as **Failed**.
- **Hyperparameter Tuning & Statistical Significance**:
  - Tuning improved XGBoost (+0.0065 Macro ROC-AUC) but not Random Forest (-0.0018).
  - 100-replicate bootstrap 95% confidence intervals show that **Random Forest [0.8932, 0.9006] and tuned XGBoost [0.8915, 0.8992] are statistically tied**.
- **Full 9-Day Out-of-Sample Test Scoring (239,112 windows)**:
  - Best Model (**Random Forest**) scored on all 2,214 stations across 108 2-hour windows (September 22 to 30):
    - **Macro ROC-AUC**: **0.9005**
    - **Balanced Accuracy**: **0.7977**
    - **Raw Accuracy**: **0.7840**
    - **Macro F1-Score**: **0.5943**

### Operational Alert Quality Breakdown (Full Test Horizon)

| Rebalancing Class | Actual Windows | True Positives (TP) | False Positives (FP) | False Negatives (FN) | Precision | Recall | Class F1 |
|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Drainer** ($\Delta \le -5$) | 16,989 (7.1%) | 11,837 | 22,077 | 5,152 | 34.90% | **69.67%** | 46.52% |
| **Collector** ($\Delta \ge 5$) | 16,842 (7.0%) | 11,440 | 23,276 | 5,402 | 32.95% | **67.93%** | 44.38% |
| **Balanced** ($-5 < \Delta < 5$) | 205,281 (85.9%) | 164,197 | 6,285 | 41,084 | 96.31% | 79.99% | 87.39% |

**Alert Quality Takeaway**:
The production model captures approximately **70% of real Drainer and Collector net-flow imbalance windows** in advance. However, because the system is heavily dominated by Balanced windows (~85.9%), approximately two out of three rebalancing alerts are false alarms (Precision ~33–35%). In operational deployment, dispatchers should use predicted probabilities to prioritize replenishment vans to the highest-confidence alerts rather than treating binary thresholds equally.

---

## Summary of Models and Methods

**Models and methods used: 9 supervised classifiers (plus baseline), DBSCAN, k-means, PCA, Poisson and negative-binomial regression, linear regression, STL, ETS, ARIMA. (SVM failed during Platt scaling calibration).**
