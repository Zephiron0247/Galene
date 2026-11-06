# Citi Bike Fleet Optimization Core
### Spatio-Temporal Demand Diagnostics, Econometric Modeling, and Predictive Rebalancing

An end-to-end data science and operational dispatch engine built in **R 4.6.1** and **SQLite** investigating Citi Bike demand dynamics, station inventory bottlenecks, transit multimodality, weather elasticities, and machine learning rebalancing forecasts across New York City for **September 2024**.

> **Comprehensive Research Report**:  
> The complete, self-contained academic report with interactive tables, vector figures, and embedded Leaflet maps is available at:  
> **[`docs/final_report.html`](docs/final_report.html)** *(Open in any web browser)*.

> **Operational Data Measurement Note**:  
> Station inventory stress is operationalized as **net-flow imbalance ($\Delta = \text{arrivals} - \text{departures}$ in a 2-hour window)**. Citi Bike does not publish historical bike counts or dock occupancy per station over time, so true empty/full dock events cannot be observed directly from trip data.

---

## Key Results at a Glance

| Domain | Finding / Production Metric |
| :--- | :--- |
| **Data Throughput** | **4,708,761 clean trips** retained from 4,997,898 raw records (**94.21% retention**) across **2,266 active stations**. |
| **Duration & Demographics** | Right-skewed distribution ($\text{skewness} = 0.96$, Tukey IQR cutoff at 32.87 min). Annual members ride a median of **8.53 min** vs. **11.82 min** for casual riders ($p < 2.2 \times 10^{-16}$). |
| **Weather Elasticity** | Negative Binomial count model ($\theta = 7.191$): Temperature raises hourly demand by **+3.6% per $^\circ\text{C}$** ($\text{IRR} = 1.0361, p < 0.001$). Rain, wind, and humidity showed no significant effect in dry September 2024. |
| **Spatial Autocorrelation** | Global Moran's $I = \mathbf{0.2780}$ ($p < 0.001$), confirming strong geographic clustering of net outflows and accumulations. |
| **Corridors & Personalities** | **18 linear transit corridors** identified via DBSCAN ($\varepsilon = 400\text{ m}, \text{minPts} = 5$). PCA + $k$-means ($k = 4$) segmented stations into 4 diurnal profiles (Morning Origins, Evening Destinations, Leisure Hubs, Balanced). |
| **Time Series Baseline** | STL + ETS ($\text{RMSE} = 2,720$) essentially ties Seasonal Naïve ($\text{RMSE} = 2,726$), showing temporal patterns alone cannot beat historical lookback without spatial/weather features. |
| **Best Predictive Model** | **Tuned Random Forest (`ranger`)** achieved **Macro ROC-AUC = 0.9005** on 239,112 held-out out-of-sample windows (Sept 22–30), capturing **~70% of real Drainer and Collector events**. |
| **Decision Support UI** | Production-ready **Shiny Dashboard** (`app/app.R`) running on port `4040` with greedy nearest-neighbor rebalancing truck dispatch and weather counterfactual simulations. |

---

## System Architecture & Data Sources

The project ingests four multimodal data sources into a central SQLite database (`data/citibike.sqlite`) containing 9 indexed tables:

```
                  ┌─────────────────────────────────────────────────────────┐
                  │                   4 RAW DATA SOURCES                    │
                  ├────────────────────┬────────────────────┬───────────────┤
                  │ Citi Bike Trips    │ GBFS Live Metadata │ Open-Meteo API│ MTA Subway Entrances
                  │ 4,997,898 rides    │ 2,518 stations     │ 720 hourly obs│ 2,120 subway portals
                  └─────────┬──────────┴─────────┬──────────┴───────┬───────┴───────────────┬─────┘
                            │                    │                  │                       │
                            ▼                    ▼                  ▼                       ▼
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│                           CENTRAL RELATIONAL DATABASE (data/citibike.sqlite)                    │
│   trips ──► trips_clean ──► hourly_demand ──► station_features ──► panel (797k) ──► test_preds  │
└───────────────────────────────────────────────┬─────────────────────────────────────────────────┘
                                                │
       ┌────────────────────────────────────────┼────────────────────────────────────────┐
       ▼                                        ▼                                        ▼
┌──────────────┐                        ┌──────────────┐                        ┌────────────────┐
│ STATS & GLM  │                        │ SPATIAL & TS │                        │ MACHINE LEARN. │
│ Wilcoxon     │                        │ Moran's I    │                        │ 9 Classifiers  │
│ Kruskal-W.   │                        │ DBSCAN (18)  │                        │ Downsampling   │
│ Neg-Binomial │                        │ STL + ETS    │                        │ Random Forest  │
└──────┬───────┘                        └──────┬───────┘                        └────────┬───────┘
       │                                       │                                         │
       └───────────────────────────────────────┼─────────────────────────────────────────┘
                                               ▼
                              ┌──────────────────────────────────┐
                              │  INTERACTIVE SHINY DASHBOARD     │
                              │  http://127.0.0.1:4040           │
                              │  Dispatch Map • What-If Sim      │
                              └──────────────────────────────────┘
```

### The 4 External Data Sources

1. **Citi Bike Trips (NYC)**: 5 monthly partition CSVs (`data/raw/trips/`) totaling 4,997,898 raw rides for September 2024 from the [Citi Bike S3 Bucket](https://s3.amazonaws.com/tripdata/index.html).
2. **GBFS Station Metadata**: Live JSON feed via [Citi Bike GBFS API](https://gbfs.citibikenyc.com/gbfs/en/station_information.json), providing coordinates and dock capacities for 2,518 stations.
3. **Hourly Meteorological Data**: Open-Meteo Historical Archive API (Central Park / NYC), providing 720 hourly records of temperature, precipitation, wind speed, and humidity.
4. **MTA Subway Entrances**: NY State Open Data portal (2,120 entrance portals with coordinates), projected to UTM Zone 18N (`EPSG:32618`) to compute 400-meter buffer counts and distance metrics.

---

## Analytical Pipeline & Workflow

The repository is organized into numbered, reproducible R scripts in [`R/`](R/):

```
R/
├── 00_setup.R            # Package verification (CRAN binaries only) & directory tree init
├── 01_fetch_apis.R       # HTTP GET & JSON parsing for GBFS, Open-Meteo, and MTA Subway
├── 02_build_database.R   # Fast list-based ingestion via data.table::fread & DBI table creation
├── 03_clean_features.R   # Tukey IQR cleaning, Haversine speed checks, sf GIS, & 2-hour panel
├── 04_eda_heatmaps.R     # Multi-layer Leaflet maps, ggplot2 tile heatmaps, & Plotly charts
├── 05_statistics.R       # Non-parametric tests, Spearman matrix, & Negative Binomial count GLM
├── 06_spatial_clusters.R # Global/Local Moran's I, DBSCAN corridors, & PCA station personalities
├── 07_time_series.R      # STL decomposition, ACF harmonic checks, & 7-day holdout benchmarks
├── 08_ml_comparison.R    # 10-model Tidymodels benchmark, temporal tuning, & 239k test scoring
├── 09_app_data.R         # Pre-computes lightweight RDS artifacts for sub-second dashboard startup
└── utils.R               # Vectorized Haversine distance, Tukey IQR filter mask, sample skewness
```

---

## Machine Learning & Alert Quality

### Target Definition (2-Hour Horizon)
- **`Drainer`** ($\Delta \le -5$): Station experiences acute net outflow; requires replenishment supply vans.
- **`Collector`** ($\Delta \ge +5$): Station experiences acute net accumulation; risks dock overflow.
- **`Balanced`** ($-5 < \Delta < 5$): System equilibrium; no immediate intervention required.

### Benchmark on 40,000 Out-of-Sample Test Pool

Models trained strictly on **Sept 01–21** (150,000-row pool downsampled via `themis::step_downsample`) and evaluated on held-out **Sept 22–30** with 100-replicate bootstrap 95% confidence intervals:

| Model Architecture | Engine / Package | Test Macro ROC-AUC | Balanced Accuracy | Macro F1-Score | Status |
| :--- | :--- | :---: | :---: | :---: | :---: |
| **Random Forest** | `ranger` | **0.8988** (Tuned: **0.9005**) | **0.7939** | **0.5946** | **Top Performer** |
| **Extreme Gradient Boosting** | `xgboost` | 0.8890 (Tuned: 0.8955) | 0.7851 | 0.5809 | Statistically tied |
| **Multilayer Perceptron (NNet)** | `nnet` | 0.8871 | 0.7821 | 0.5758 | Competitive |
| **Bagged Decision Trees** | `baguette` | 0.8818 | 0.7832 | 0.5739 | Strong ensemble |
| **Multinomial Elastic Net** | `glmnet` | 0.8790 | 0.7408 | 0.6046 | Fast linear baseline |
| **Linear Discriminant Analysis**| `MASS` | 0.8644 | 0.7049 | 0.5892 | Baseline classifier |
| **K-Nearest Neighbors ($k=15$)** | `kknn` | 0.8275 | 0.7234 | 0.5365 | Local similarity |
| **Decision Tree (CART)** | `rpart` | 0.8172 | 0.7881 | 0.5301 | Interpretable rule set |
| **Naive Bayes** | `naivebayes` | 0.7855 | 0.5567 | 0.1768 | Conditional independence |
| **Null Model (Majority Class)** | Base R | 0.5000 | 0.5000 | 0.3079 | Zero recall on minorities |
| *Support Vector Machine (RBF)* | `kernlab` | — | — | — | *Failed Platt scaling* |

### Production Test Scoring (239,112 Held-Out Windows)

The finalized Random Forest was scored across all 2,214 active stations for the entire 9-day out-of-sample test horizon (108 two-hour windows):

```
                   Actual Drainer        Actual Collector        Actual Balanced
Predicted Drainer     11,837 (TP)               478                   21,599
Predicted Collector      412                11,440 (TP)               22,864
Predicted Balanced     4,740                  4,924                  160,818 (TN)
```

- **Drainer Recall**: **69.67%** (Precision: 34.90%)
- **Collector Recall**: **67.93%** (Precision: 32.95%)
- **Balanced Accuracy**: **79.77%** | **Macro ROC-AUC**: **0.9005**
- **Operational Reality**: Due to natural class rarity (~7.1% Drainers, ~7.0% Collectors), roughly 2 out of 3 raw alerts are false alarms at a fixed 0.50 threshold. The dashboard provides dynamic probability threshold sliders so dispatchers can prioritize vans to highest-confidence alerts first.

---

## Interactive Shiny Dashboard

An interactive operations dashboard is available in [`app/`](app/):

```powershell
# Launch the dashboard locally on port 4040
./run_app.bat
# Or from R:
shiny::runApp("app", port = 4040, host = "127.0.0.1")
```

### Dashboard Features
- **Top Navigation Bar**: 8 dedicated panels with dark (`#0B1220`) and light (`#F6F8FB`) themes and offline system font stacks.
- **Dispatch Map**: Live Leaflet map with greedy nearest-neighbor rebalancing truck route generation ($\le 1.5\text{ km}$ transfer pairs).
- **Interactive Heatmaps**: Day-of-week and hourly departure density visualizations.
- **Station Explorer**: Detailed 24-hour departure profile curves for 2,266 stations.
- **Model Lab**: Interactive precision/recall and economic cost curves to find cost-optimal probability thresholds.
- **What-If Simulator**: Counterfactual weather adjustment sliders (temperature, rain, wind) that re-score the Random Forest model on the fly.

---

## Running the Complete Pipeline

To run the full end-to-end pipeline from scratch:

```powershell
Rscript R/00_setup.R            # 1. Environment & packages
Rscript R/01_fetch_apis.R       # 2. Ingest APIs (GBFS, weather, subway)
Rscript R/02_build_database.R   # 3. Build central SQLite database
Rscript R/03_clean_features.R   # 4. Clean trips & build 797k panel
Rscript R/04_eda_heatmaps.R     # 5. Generate EDA figures & heatmaps
Rscript R/05_statistics.R       # 6. Statistical tests & Negative Binomial GLM
Rscript R/06_spatial_clusters.R # 7. Moran's I, DBSCAN, & PCA personalities
Rscript R/07_time_series.R      # 8. STL decomposition & holdout forecasting
Rscript R/08_ml_comparison.R    # 9. 10-model ML comparison & final test scoring
Rscript R/09_app_data.R         # 10. Pre-compute lightweight dashboard RDS files
```

---

## Operational Limitations & Academic Honesty

1. **Unmeasured Dock Inventories**: Citi Bike publishes trip origins and destinations, not real-time dock occupancy. Inventory stress is modeled as net-flow ($\Delta$).
2. **Latent / Suppressed Demand**: When a station is completely starved of bikes, unmet user demand cannot be observed in departure logs.
3. **Single-Month Scope**: Observations are limited to September 2024. Weather effects reflect a historically dry autumn month.
4. **Euclidean Transit Proxies**: Subway proximity is measured via projected Euclidean buffer distance, which acts as a lower bound for real urban street-grid walking paths.

---

## License & Attribution

- **Source Code**: [MIT License](LICENSE)
- **Citi Bike Data**: Published under the [Citi Bike Data License Agreement](https://citibikenyc.com/data-sharing-policy).
- **Weather Data**: Courtesy of [Open-Meteo](https://open-meteo.com) under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
- **Subway Data**: Courtesy of [MTA / New York State Open Data](https://data.ny.gov/).
