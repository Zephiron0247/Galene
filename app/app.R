# app/app.R
# Project: citibike-rebalancing
# Title: Citi Bike Fleet Optimization Core - Interactive Shiny Dashboard
# Purpose: High-performance operational dashboard for fleet rebalancing,
#          spatio-temporal demand diagnostics, and machine learning model evaluation.
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(dplyr)
library(htmltools)
library(bsicons)

# ------------------------------------------------------------------------------
# 1. Global Setup, Color Codes & Fast Asset Ingestion
# ------------------------------------------------------------------------------
COLOR_DRAINER   <- "#EF4444" # Red
COLOR_COLLECTOR <- "#3B82F6" # Blue
COLOR_BALANCED  <- "#94A3B8" # Slate / Grey

# Base path resolution (works when launched from app/ or project root)
get_data_path <- function(rel) {
  if (file.exists(file.path("data", rel))) return(file.path("data", rel))
  if (file.exists(file.path("app", "data", rel))) return(file.path("app", "data", rel))
  file.path("data", rel)
}

# Load precomputed lightweight artifacts (loads in < 1 second)
test_preds   <- readRDS(get_data_path("test_predictions.rds"))
flow_hourly  <- readRDS(get_data_path("station_hour_daytype.rds"))
station_prof <- readRDS(get_data_path("station_departure_profiles.rds"))
top25_stns   <- readRDS(get_data_path("top25_stations.rds"))
tbl_bundle   <- readRDS(get_data_path("tables_bundle.rds"))
thresh_curve <- readRDS(get_data_path("threshold_curve.rds"))
whatif_df    <- readRDS(get_data_path("whatif_data.rds"))

# Extract distinct test dates and windows
avail_dates   <- sort(unique(test_preds$date))
avail_windows <- sort(unique(test_preds$time_window))
avail_stns    <- sort(unique(test_preds$station_name))
whatif_stns   <- sort(unique(whatif_df$station_name))

# ------------------------------------------------------------------------------
# 2. Math & Algorithm Helpers
# ------------------------------------------------------------------------------
# Vectorized Haversine distance in kilometres
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  p1 <- lat1 * pi / 180
  p2 <- lat2 * pi / 180
  dp <- (lat2 - lat1) * pi / 180
  dl <- (lon2 - lon1) * pi / 180
  a  <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Greedy nearest-neighbour matching of Collector surpluses to Drainer deficits
match_greedy_moves <- function(collectors, drainers, max_dist_km = 1.5) {
  if (nrow(collectors) == 0 || nrow(drainers) == 0) {
    return(data.frame(
      From_Collector = character(0),
      To_Drainer = character(0),
      Distance_km = numeric(0),
      Collector_Surplus_Prob = character(0),
      Drainer_Deficit_Prob = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  matched <- list()
  avail_d <- drainers
  
  for (i in seq_len(nrow(collectors))) {
    if (nrow(avail_d) == 0) break
    c_lat <- collectors$lat[i]
    c_lng <- collectors$lng[i]
    
    dists <- haversine_km(c_lat, c_lng, avail_d$lat, avail_d$lng)
    min_i <- which.min(dists)
    
    if (length(min_i) > 0 && dists[min_i] <= max_dist_km) {
      d_stn <- avail_d[min_i, ]
      matched[[length(matched) + 1]] <- data.frame(
        From_Collector = collectors$station_name[i],
        To_Drainer     = d_stn$station_name,
        Distance_km    = round(dists[min_i], 2),
        Collector_Surplus_Prob = sprintf("%.1f%%", collectors$prob_collector[i] * 100),
        Drainer_Deficit_Prob   = sprintf("%.1f%%", d_stn$prob_drainer * 100),
        stringsAsFactors = FALSE
      )
      avail_d <- avail_d[-min_i, ] # Greedy consumption
    }
  }
  
  if (length(matched) == 0) {
    return(data.frame(
      From_Collector = "No station pair within 1.5 km",
      To_Drainer     = "-",
      Distance_km    = NA_real_,
      Collector_Surplus_Prob = "-",
      Drainer_Deficit_Prob   = "-",
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, matched)
}

# Thematic styling for Plotly charts (dark/light transparent background)
apply_plotly_theme <- function(p, is_dark = TRUE, title = "", x_title = "", y_title = "") {
  tc <- if (is_dark) "#E6EDF7" else "#0F172A"
  gc <- if (is_dark) "#1E2A44" else "#E2E8F0"
  
  plotly::layout(
    p,
    title = list(text = title, font = list(color = tc, size = 15, family = "Space Grotesk")),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(color = tc, size = 13, family = "Inter"),
    xaxis = list(
      title = list(text = x_title, font = list(size = 13)),
      color = tc,
      gridcolor = gc,
      zerolinecolor = gc
    ),
    yaxis = list(
      title = list(text = y_title, font = list(size = 13)),
      color = tc,
      gridcolor = gc,
      zerolinecolor = gc
    ),
    legend = list(font = list(color = tc, size = 12))
  )
}

# ------------------------------------------------------------------------------
# 3. Application UI Definition
# ------------------------------------------------------------------------------
ui <- bslib::page_navbar(
  id = "nav_tabs",
  title = "Citi Bike Fleet Optimization Core",
  navbar_options = bslib::navbar_options(collapsible = FALSE),
  theme = bslib::bs_theme(
    version = 5,
    bg = "#0B1220",
    fg = "#E6EDF7",
    primary = "#38BDF8",
    secondary = "#94A3B8",
    success = "#3B82F6",
    danger = "#EF4444",
    base_font = bslib::font_collection("Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Space Grotesk", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif"),
    code_font = bslib::font_collection("JetBrains Mono", "Consolas", "Courier New", "monospace"),
    font_scale = 1.02
  ),
  
  # Custom CSS injection for cards, borders, typography and projector legibility
  header = htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML("
      :root {
        --bs-border-radius: 12px;
      }
      body, button, input, select, textarea, .navbar, .nav-link, .card, p, span, li, div, table, th, td {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif !important;
      }
      h1, h2, h3, h4, h5, h6, .navbar-brand, .card-header {
        font-family: 'Space Grotesk', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif !important;
      }
      code, pre, .font-monospace {
        font-family: 'JetBrains Mono', Consolas, 'Courier New', monospace !important;
      }
      body {
        font-size: 16px;
      }
      [data-bs-theme='dark'] {
        --bs-body-bg: #0B1220;
        --bs-body-color: #E6EDF7;
        --bs-card-bg: #121B2E;
        --bs-border-color: #1E2A44;
        --bs-secondary-color: #94A3B8;
      }
      [data-bs-theme='light'] {
        --bs-body-bg: #F6F8FB;
        --bs-body-color: #0F172A;
        --bs-card-bg: #FFFFFF;
        --bs-border-color: #E2E8F0;
        --bs-secondary-color: #64748B;
      }
      /* Ensure Navbar is permanently visible, never collapsed, and tabs stand out */
      .navbar {
        background-color: var(--bs-card-bg) !important;
        border-bottom: 1px solid var(--bs-border-color) !important;
        padding: 0.6rem 1.25rem !important;
        position: sticky !important;
        top: 0 !important;
        z-index: 1030 !important;
      }
      .navbar .container-fluid {
        display: flex !important;
        align-items: center !important;
        justify-content: space-between !important;
        flex-wrap: nowrap !important;
      }
      .navbar-header {
        display: flex !important;
        align-items: center !important;
      }
      .navbar-toggle, .navbar-toggler {
        display: none !important;
      }
      .navbar-collapse, .navbar-collapse.collapse, .navbar-collapse.collapsing {
        display: flex !important;
        visibility: visible !important;
        opacity: 1 !important;
        flex-grow: 1 !important;
        align-items: center !important;
        justify-content: space-between !important;
      }
      .navbar-brand {
        font-size: 1.15rem !important;
        font-weight: 700 !important;
        letter-spacing: -0.02em !important;
        color: var(--bs-body-color) !important;
        margin-right: 1.5rem !important;
        white-space: nowrap !important;
        text-decoration: none !important;
      }
      .navbar-nav {
        display: flex !important;
        flex-direction: row !important;
        align-items: center !important;
        gap: 0.35rem !important;
        margin: 0 !important;
        padding: 0 !important;
        list-style: none !important;
        flex-wrap: wrap !important;
      }
      .navbar-nav > li {
        display: inline-flex !important;
        align-items: center !important;
      }
      .navbar-nav > li > a,
      .navbar-nav .nav-link {
        display: inline-block !important;
        padding: 0.45rem 0.85rem !important;
        font-size: 0.92rem !important;
        font-weight: 500 !important;
        border-radius: 8px !important;
        text-decoration: none !important;
        transition: all 0.15s ease-in-out !important;
        white-space: nowrap !important;
        line-height: 1.4 !important;
      }
      /* Dark mode tab links */
      [data-bs-theme='dark'] .navbar {
        background-color: #121B2E !important;
        border-bottom: 1px solid #1E2A44 !important;
      }
      [data-bs-theme='dark'] .navbar-brand {
        color: #E6EDF7 !important;
      }
      [data-bs-theme='dark'] .navbar-nav > li > a,
      [data-bs-theme='dark'] .navbar-nav .nav-link {
        color: #CBD5E1 !important;
        background-color: transparent !important;
      }
      [data-bs-theme='dark'] .navbar-nav > li > a:hover,
      [data-bs-theme='dark'] .navbar-nav .nav-link:hover {
        color: #38BDF8 !important;
        background-color: rgba(56, 189, 248, 0.12) !important;
      }
      [data-bs-theme='dark'] .navbar-nav > li.active > a,
      [data-bs-theme='dark'] .navbar-nav .nav-link.active {
        color: #0B1220 !important;
        background-color: #38BDF8 !important;
        font-weight: 700 !important;
      }
      /* Light mode tab links */
      [data-bs-theme='light'] .navbar {
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #E2E8F0 !important;
      }
      [data-bs-theme='light'] .navbar-brand {
        color: #0F172A !important;
      }
      [data-bs-theme='light'] .navbar-nav > li > a,
      [data-bs-theme='light'] .navbar-nav .nav-link {
        color: #334155 !important;
        background-color: transparent !important;
      }
      [data-bs-theme='light'] .navbar-nav > li > a:hover,
      [data-bs-theme='light'] .navbar-nav .nav-link:hover {
        color: #0284C7 !important;
        background-color: rgba(2, 132, 199, 0.1) !important;
      }
      [data-bs-theme='light'] .navbar-nav > li.active > a,
      [data-bs-theme='light'] .navbar-nav .nav-link.active {
        color: #FFFFFF !important;
        background-color: #0284C7 !important;
        font-weight: 700 !important;
      }
      /* Sub-tabs / Pills styling */
      [data-bs-theme='dark'] .nav-pills .nav-link {
        color: #CBD5E1 !important;
        background-color: rgba(30, 42, 68, 0.6) !important;
        border: 1px solid #1E2A44 !important;
        margin-right: 6px !important;
        border-radius: 8px !important;
      }
      [data-bs-theme='dark'] .nav-pills .nav-link:hover {
        color: #38BDF8 !important;
        border-color: #38BDF8 !important;
      }
      [data-bs-theme='dark'] .nav-pills .nav-link.active {
        color: #0B1220 !important;
        background-color: #38BDF8 !important;
        border-color: #38BDF8 !important;
        font-weight: 700 !important;
      }
      [data-bs-theme='light'] .nav-pills .nav-link {
        color: #475569 !important;
        background-color: #F1F5F9 !important;
        border: 1px solid #CBD5E1 !important;
        margin-right: 6px !important;
        border-radius: 8px !important;
      }
      [data-bs-theme='light'] .nav-pills .nav-link:hover {
        color: #0284C7 !important;
        border-color: #0284C7 !important;
      }
      [data-bs-theme='light'] .nav-pills .nav-link.active {
        color: #FFFFFF !important;
        background-color: #0284C7 !important;
        border-color: #0284C7 !important;
        font-weight: 700 !important;
      }
      /* Cards & typography */
      .card {
        border-radius: 12px !important;
        border: 1px solid var(--bs-border-color) !important;
        background-color: var(--bs-card-bg) !important;
        box-shadow: none !important;
      }
      .card-header {
        background-color: transparent !important;
        border-bottom: 1px solid var(--bs-border-color) !important;
        font-weight: 600;
        letter-spacing: -0.01em;
      }
      .pipeline-step {
        display: inline-block;
        padding: 6px 14px;
        margin: 4px;
        border-radius: 20px;
        background: rgba(56, 189, 248, 0.12);
        color: #38BDF8;
        font-size: 14px;
        font-weight: 600;
        border: 1px solid rgba(56, 189, 248, 0.3);
      }
      .pipeline-arrow {
        color: var(--bs-secondary-color);
        font-weight: bold;
        margin: 0 4px;
      }
      .badge-drainer {
        background-color: #EF4444 !important;
        color: #FFFFFF !important;
        font-weight: 600;
      }
      .badge-collector {
        background-color: #3B82F6 !important;
        color: #FFFFFF !important;
        font-weight: 600;
      }
      .badge-balanced {
        background-color: #94A3B8 !important;
        color: #0F172A !important;
        font-weight: 600;
      }
      .control-help {
        font-size: 13px;
        color: var(--bs-secondary-color);
        margin-top: 2px;
        margin-bottom: 8px;
      }
      /* Value boxes */
      .bslib-value-box {
        border-radius: 12px !important;
        border: 1px solid var(--bs-border-color) !important;
      }
      [data-bs-theme='dark'] .bslib-value-box {
        background-color: #121B2E !important;
        color: #E6EDF7 !important;
      }
      /* DataTables dark theme styling */
      [data-bs-theme='dark'] table.dataTable {
        color: #E6EDF7 !important;
      }
      [data-bs-theme='dark'] table.dataTable th {
        background-color: #162036 !important;
        color: #F1F5F9 !important;
        border-bottom: 1px solid #1E2A44 !important;
      }
      [data-bs-theme='dark'] table.dataTable td {
        background-color: #121B2E !important;
        color: #CBD5E1 !important;
        border-top: 1px solid #1E2A44 !important;
      }
      [data-bs-theme='dark'] .dataTables_info,
      [data-bs-theme='dark'] .dataTables_paginate,
      [data-bs-theme='dark'] .dataTables_length,
      [data-bs-theme='dark'] .dataTables_filter {
        color: #94A3B8 !important;
      }
      [data-bs-theme='dark'] .page-link {
        background-color: #121B2E !important;
        border-color: #1E2A44 !important;
        color: #CBD5E1 !important;
      }
      /* Form inputs & selects */
      [data-bs-theme='dark'] .form-control,
      [data-bs-theme='dark'] .form-select {
        background-color: #162036 !important;
        border-color: #1E2A44 !important;
        color: #E6EDF7 !important;
      }
      [data-bs-theme='dark'] .form-label,
      [data-bs-theme='dark'] .control-label {
        color: #CBD5E1 !important;
        font-weight: 500 !important;
      }
    "))
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 1: Overview
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Overview",
    bslib::layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      bslib::value_box(
        title = "Trips Analysed",
        value = "4,708,761",
        showcase = bsicons::bs_icon("bicycle"),
        p("Retained from 4,997,898 raw records (94.21% clean retention rate)", class = "control-help")
      ),
      bslib::value_box(
        title = "Active Stations",
        value = "2,266",
        showcase = bsicons::bs_icon("geo-alt"),
        p("Spanning Manhattan, Brooklyn, Queens, Bronx and Jersey City", class = "control-help")
      ),
      bslib::value_box(
        title = "Test Macro ROC-AUC",
        value = "0.9005",
        showcase = bsicons::bs_icon("graph-up-arrow"),
        p("Final Random Forest across 9 held-out days (Sept 22-30)", class = "control-help")
      ),
      bslib::value_box(
        title = "Windows Scored",
        value = "239,112",
        showcase = bsicons::bs_icon("clock"),
        p("108 two-hour time windows evaluated out-of-sample", class = "control-help")
      )
    ),
    
    htmltools::tags$div(style = "height: 16px;"),
    
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Problem Statement & Operational Context"),
        htmltools::tags$p(
          "Urban bike-sharing systems experience severe ",
          htmltools::tags$strong("tidal spatio-temporal demand asymmetry"),
          ". During morning peak hours, thousands of commuters unlock bikes in residential outer-borough ",
          "neighborhoods and ride into commercial centers in Midtown and Lower Manhattan. By evening, the flow reverses.",
          style = "line-height: 1.6;"
        ),
        htmltools::tags$p(
          "This dynamic creates acute dock starvation (",
          htmltools::tags$span(class = "badge badge-drainer", "Drainer stations"),
          " with net deficit \u2264 -5 bikes) and dock overflow (",
          htmltools::tags$span(class = "badge badge-collector", "Collector stations"),
          " with net accumulation \u2265 +5 bikes), leaving arriving riders unable to dock.",
          style = "line-height: 1.6;"
        ),
        htmltools::tags$p(
          "Because Citi Bike does not publish live historical dock inventories, this system operationalizes rebalancing urgency ",
          "through ", htmltools::tags$strong("net-flow imbalance (\u0394 = arrivals \u2212 departures)"),
          " across discrete 2-hour operational horizons, forecasting bottlenecks before they occur.",
          style = "line-height: 1.6;"
        )
      ),
      
      bslib::card(
        bslib::card_header("End-to-End Pipeline Architecture"),
        htmltools::tags$div(
          style = "padding: 10px 0; text-align: center;",
          htmltools::tags$span(class = "pipeline-step", "1. Ingest Raw Data"),
          htmltools::tags$span(class = "pipeline-arrow", "\u2192"),
          htmltools::tags$span(class = "pipeline-step", "2. Clean & Audit"),
          htmltools::tags$span(class = "pipeline-arrow", "\u2192"),
          htmltools::tags$span(class = "pipeline-step", "3. Feature Store"),
          htmltools::tags$br(), htmltools::tags$br(),
          htmltools::tags$span(class = "pipeline-step", "4. Spatial & Stats"),
          htmltools::tags$span(class = "pipeline-arrow", "\u2192"),
          htmltools::tags$span(class = "pipeline-step", "5. 9 ML Classifiers"),
          htmltools::tags$span(class = "pipeline-arrow", "\u2192"),
          htmltools::tags$span(class = "pipeline-step", "6. Live Dispatch Core")
        ),
        htmltools::tags$p(
          "Strict data hygiene: R 4.6.1 on Windows, zero leakage, leak-free target encodings computed strictly on ",
          "train partition (Sept 1-21), and out-of-sample evaluation strictly on Sept 22-30.",
          class = "control-help", style = "margin-top: 10px;"
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 2: Dispatch Map (Core Interactive Command Center)
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Dispatch map",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        title = "Dispatch Controls",
        selectInput("dispatch_date", "Evaluation date", choices = avail_dates, selected = "2024-09-23"),
        p("Held-out test period (22 to 30 September 2024)", class = "control-help"),
        
        selectInput("dispatch_time", "2-hour operational window", choices = avail_windows, selected = "08:00"),
        p("Commuter morning rush (08:00) vs. evening return (18:00)", class = "control-help"),
        
        sliderInput("thresh_drainer", "Drainer alert threshold (P)", min = 0.05, max = 0.95, value = 0.50, step = 0.05),
        p("Flag Drainer if predicted P(Drainer) \u2265 threshold", class = "control-help"),
        
        sliderInput("thresh_collector", "Collector alert threshold (P)", min = 0.05, max = 0.95, value = 0.50, step = 0.05),
        p("Flag Collector if predicted P(Collector) \u2265 threshold", class = "control-help"),
        
        radioButtons("dispatch_scope", "KPI evaluation horizon",
                     choices = c("Selected window" = "window", "Selected date" = "day", "Whole 9-day test period" = "whole"),
                     selected = "day"),
        p("Compute live precision/recall over selected scope", class = "control-help"),
        
        checkboxInput("show_actual", "Show ground truth instead of prediction", value = FALSE),
        p("Toggle to compare predicted alerts against realized net-flow", class = "control-help")
      ),
      
      # Live KPI metrics row
      bslib::layout_column_wrap(
        width = 1/6,
        fill = FALSE,
        bslib::value_box(title = "Alerts Raised", value = textOutput("kpi_alerts"), showcase = bsicons::bs_icon("bell")),
        bslib::value_box(title = "Correct Alerts", value = textOutput("kpi_correct"), showcase = bsicons::bs_icon("check-circle")),
        bslib::value_box(title = "False Alarms", value = textOutput("kpi_false_alarms"), showcase = bsicons::bs_icon("exclamation-triangle")),
        bslib::value_box(title = "Missed Events", value = textOutput("kpi_missed"), showcase = bsicons::bs_icon("x-circle")),
        bslib::value_box(title = "Precision", value = textOutput("kpi_precision"), showcase = bsicons::bs_icon("bullseye")),
        bslib::value_box(title = "Recall", value = textOutput("kpi_recall"), showcase = bsicons::bs_icon("reception-4"))
      ),
      
      htmltools::tags$div(style = "height: 8px;"),
      
      # Interactive Map
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("NYC Rebalancing Spatial Distribution Map"),
        leaflet::leafletOutput("dispatch_leaflet_map", height = "68vh")
      ),
      
      # Greedy Rebalancing Suggestions Table
      bslib::card(
        bslib::card_header("Suggested Rebalancing Moves (Greedy Nearest-Neighbour Heuristic)"),
        p("Pairs each flagged Collector (\u2265 +5 bikes surplus) with the nearest flagged Drainer (\u2264 -5 bikes deficit) within 1.5 km walking/van distance.", class = "control-help"),
        DT::dataTableOutput("dispatch_moves_table")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 3: Heatmaps
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Heatmaps",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        title = "Heatmap Controls",
        selectInput("heatmap_metric", "Activity metric",
                    choices = c("Departures" = "departures",
                                "Arrivals" = "arrivals",
                                "Drainer pressure (net outflow)" = "net_outflow",
                                "Collector pressure (net inflow)" = "net_inflow"),
                    selected = "departures"),
        p("Spatial density weighted by selected metric", class = "control-help"),
        
        sliderInput("heatmap_hours", "Hour of day (EST)", min = 0, max = 23, value = c(7, 9), step = 1),
        p("Filter temporal window across diurnal cycle", class = "control-help"),
        
        radioButtons("heatmap_daytype", "Day type",
                     choices = c("All Days" = "all", "Weekday Only" = "Weekday", "Weekend Only" = "Weekend"),
                     selected = "Weekday"),
        p("Commute rush occurs Monday-Friday; leisure spreads across weekends", class = "control-help")
      ),
      
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Density Heatmap: Spatial Flow Concentrations"),
          p("Dynamic Gaussian blur kernel weighted by station hourly flow volume.", class = "control-help"),
          leaflet::leafletOutput("leaflet_density_heatmap", height = "68vh")
        ),
        bslib::card(
          bslib::card_header("Top 25 Busiest Stations Diurnal Profile"),
          p("Station (y-axis) vs. Hour (x-axis) activity matrix for the selected filter.", class = "control-help"),
          plotly::plotlyOutput("plotly_top25_heatmap", height = "68vh")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 4: Station Explorer
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Station explorer",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        title = "Station Lookup",
        selectizeInput("explorer_station", "Select station", choices = NULL),
        p("Search any of 2,266 stations by name", class = "control-help")
      ),
      
      # Station Metadata Cards
      bslib::layout_column_wrap(
        width = 1/4,
        fill = FALSE,
        bslib::value_box(title = "Physical Capacity", value = textOutput("stn_kpi_capacity"), showcase = bsicons::bs_icon("safe")),
        bslib::value_box(title = "Subway (400m)", value = textOutput("stn_kpi_subway"), showcase = bsicons::bs_icon("train-front")),
        bslib::value_box(title = "Diurnal Personality", value = textOutput("stn_kpi_personality"), showcase = bsicons::bs_icon("person-workspace")),
        bslib::value_box(title = "DBSCAN Cluster", value = textOutput("stn_kpi_cluster"), showcase = bsicons::bs_icon("diagram-3"))
      ),
      
      htmltools::tags$div(style = "height: 8px;"),
      
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Average 24-Hour Diurnal Profile"),
          p("Mean departures per hour: Weekday commute peaks vs. Weekend leisure curve.", class = "control-help"),
          plotly::plotlyOutput("plotly_stn_profile", height = "50vh")
        ),
        bslib::card(
          bslib::card_header("Out-of-Sample Test Timeline (Sept 22-30)"),
          p("Realized net flow (\u0394) overlaid with predicted class probabilities.", class = "control-help"),
          plotly::plotlyOutput("plotly_stn_timeline", height = "50vh")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 5: Statistics and Spatial
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Statistics and spatial",
    bslib::navset_card_pill(
      id = "stats_subtabs",
      
      # Sub-tab: Hypothesis Tests
      bslib::nav_panel(
        title = "Statistical Tests",
        p("Rigorous non-parametric hypothesis testing on 300,000-trip samples to avoid sample-size inflation.", class = "control-help"),
        DT::dataTableOutput("table_stats_summary")
      ),
      
      # Sub-tab: Weather Sensitivity
      bslib::nav_panel(
        title = "Weather Effects (IRR)",
        p("Incidence Rate Ratios (IRR) from Negative Binomial count regression (\u03b8 = 7.191). Only temperature exhibits statistically significant elasticity in September 2024.", class = "control-help"),
        plotly::plotlyOutput("plotly_weather_irr", height = "55vh")
      ),
      
      # Sub-tab: Spatial & Clusters
      bslib::nav_panel(
        title = "Spatial Autocorrelation & Clusters",
        bslib::layout_column_wrap(
          width = 1/3,
          fill = FALSE,
          bslib::value_box(title = "Global Moran's I", value = "0.2780", showcase = bsicons::bs_icon("globe-americas")),
          bslib::value_box(title = "Permutation p-value", value = "< 0.001", showcase = bsicons::bs_icon("check-all")),
          bslib::value_box(title = "DBSCAN Corridors", value = "18 Corridors", showcase = bsicons::bs_icon("layers"))
        ),
        htmltools::tags$div(style = "height: 8px;"),
        bslib::card(
          bslib::card_header("Cluster Silhouette Width: DBSCAN vs. K-Means"),
          htmltools::tags$p(
            "The silhouette score favours round clusters and counts noise stations as a cluster, so it cannot judge DBSCAN fairly. ",
            "DBSCAN was chosen because station corridors are not round and it can flag isolated stations as noise.",
            style = "font-style: italic; color: var(--bs-secondary-color);"
          ),
          DT::dataTableOutput("table_silhouette_comp")
        ),
        htmltools::tags$div(style = "height: 8px;"),
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            bslib::card_header("Local Moran (LISA) Hotspots"),
            htmltools::tags$iframe(src = "maps/local_moran.html", width = "100%", height = "420px", style = "border:none;")
          ),
          bslib::card(
            bslib::card_header("DBSCAN Linear Corridors"),
            htmltools::tags$iframe(src = "maps/dbscan_clusters.html", width = "100%", height = "420px", style = "border:none;")
          ),
          bslib::card(
            bslib::card_header("Diurnal Personalities"),
            htmltools::tags$iframe(src = "maps/station_personalities.html", width = "100%", height = "420px", style = "border:none;")
          )
        )
      ),
      
      # Sub-tab: Time Series Holdout
      bslib::nav_panel(
        title = "Time Series Forecasting",
        htmltools::tags$p(
          "A forecast from the daily pattern alone is about as good as repeating last week; ",
          "the extra signal must come from weather and station features, which is why the classification models use them.",
          style = "font-weight: 500; font-size: 15px; margin-bottom: 12px;"
        ),
        DT::dataTableOutput("table_ts_accuracy"),
        htmltools::tags$div(style = "height: 12px;"),
        bslib::card(
          bslib::card_header("72-Hour Autocorrelation & STL Decomposition"),
          htmltools::tags$img(src = "figures/stl_decomposition.png", style = "max-width:100%; height:auto; border-radius:8px;")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 6: Model Lab
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Model lab",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        title = "Model Benchmark",
        selectInput("model_metric_select", "Evaluation Metric",
                    choices = c("Macro ROC-AUC" = "macro_roc_auc",
                                "Balanced Accuracy" = "balanced_accuracy",
                                "Macro PR-AUC" = "macro_pr_auc",
                                "Macro F1" = "macro_f1",
                                "Drainer Recall" = "recall_drainer",
                                "Collector Recall" = "recall_collector",
                                "Training Time (s)" = "training_time_sec"),
                    selected = "macro_roc_auc"),
        p("Sort architectures by operational criteria", class = "control-help"),
        
        numericInput("cost_wasted", "Cost of wasted van dispatch (FA)", value = 1, min = 0.1, max = 50, step = 0.5),
        p("Operator cost incurred for a false alarm", class = "control-help"),
        
        numericInput("cost_missed", "Cost of missed dock emergency (FN)", value = 3, min = 0.5, max = 50, step = 0.5),
        p("Customer penalty for unmitigated starvation/overflow", class = "control-help")
      ),
      
      # Benchmark Bar Chart
      bslib::card(
        bslib::card_header("Classifier Performance Comparison (Sorted)"),
        p("Baseline (Null) marked in grey; SVM RBF failed probability calibration and is marked as Failed.", class = "control-help"),
        plotly::plotlyOutput("plotly_model_benchmark", height = "42vh")
      ),
      
      htmltools::tags$div(style = "height: 8px;"),
      
      # Optimal Threshold Economic Cost Curve
      bslib::card(
        bslib::card_header("Alert Threshold Trade-off & Cost Curve"),
        p("Trade-off between precision, recall, and total dispatch penalty across thresholds.", class = "control-help"),
        plotly::plotlyOutput("plotly_threshold_tradeoff", height = "45vh")
      ),
      
      htmltools::tags$div(style = "height: 8px;"),
      
      # Model Diagnostic Artifacts
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Multiclass ROC Curves"),
          htmltools::tags$img(src = "figures/roc_curves_by_class.png", style = "max-width:100%; height:auto; border-radius:8px;")
        ),
        bslib::card(
          bslib::card_header("Multiclass PR Curves"),
          htmltools::tags$img(src = "figures/pr_curves_by_class.png", style = "max-width:100%; height:auto; border-radius:8px;")
        )
      ),
      
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header("Confusion Matrices (10 Models)"),
          htmltools::tags$img(src = "figures/confusion_matrices_all_models.png", style = "max-width:100%; height:auto; border-radius:8px;")
        ),
        bslib::card(
          bslib::card_header("Feature Importance (Random Forest vs. XGBoost)"),
          htmltools::tags$img(src = "figures/feature_importance_rf_xgb.png", style = "max-width:100%; height:auto; border-radius:8px;")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 7: What-If Scenario Simulation
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "What-if",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        title = "What-If Parameters",
        selectInput("whatif_station_select", "Station scenario", choices = whatif_stns, selected = whatif_stns[1]),
        p("High-activity commuter stations", class = "control-help"),
        
        selectInput("whatif_window_select", "Operational window",
                    choices = c("Weekday Morning Rush (08:00)" = "2024-09-23 08:00:00",
                                "Weekday Evening Rush (18:00)" = "2024-09-23 18:00:00",
                                "Weekend Midday Leisure (14:00)" = "2024-09-28 14:00:00",
                                "Late Night Low Volume (02:00)" = "2024-09-25 02:00:00"),
                    selected = "2024-09-23 08:00:00"),
        p("Diurnal demand scenarios", class = "control-help"),
        
        sliderInput("whatif_temp", "Counterfactual Temperature (\u00b0C)", min = 5, max = 35, value = 20, step = 1),
        p("Shift ambient temperature", class = "control-help"),
        
        sliderInput("whatif_precip", "Counterfactual Precipitation (mm)", min = 0, max = 15, value = 0, step = 0.5),
        p("Simulate heavy rainfall event", class = "control-help"),
        
        sliderInput("whatif_wind", "Counterfactual Wind Speed (km/h)", min = 0, max = 35, value = 10, step = 1),
        p("Simulate coastal gusting", class = "control-help")
      ),
      
      bslib::card(
        bslib::card_header("Counterfactual Model Sensitivity Simulation"),
        htmltools::tags$div(
          style = "padding: 6px 12px; background: rgba(56, 189, 248, 0.1); border-left: 4px solid #38BDF8; margin-bottom: 15px; border-radius: 4px;",
          htmltools::tags$strong("Note on Operational Interpretation: "),
          "This interactive simulation demonstrates the trained Random Forest model's marginal probability sensitivity to weather shocks. ",
          "It represents model sensitivity, not an operational forecast."
        ),
        plotly::plotlyOutput("plotly_whatif_comparison", height = "52vh")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # Tab 8: Method and Limits
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Method and limits",
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Data Sources & Attributions"),
        htmltools::tags$ul(
          htmltools::tags$li(htmltools::tags$strong("Citi Bike Trip Transactions: "), "Lyft Urban Solutions / S3 Public Bucket (4,997,898 raw transactions ingested across 5 NYC partition files; 4,708,761 cleaned)."),
          htmltools::tags$li(htmltools::tags$strong("GBFS Live Feed Metadata: "), "Station coordinates, physical capacities, and region codes (2,518 stations)."),
          htmltools::tags$li(htmltools::tags$strong("Historical Weather Archive: "), "Open-Meteo Historical Weather API for Central Park Lat 40.7128, Lon -74.006 (Licensed under ", htmltools::tags$strong("CC BY 4.0"), ")."),
          htmltools::tags$li(htmltools::tags$strong("MTA Rapid Transit Entrances: "), "New York State Open Data Portal (2,120 subway entrances).")
        )
      ),
      bslib::card(
        bslib::card_header("Target Formulation & Leakage Control"),
        htmltools::tags$ul(
          htmltools::tags$li(htmltools::tags$strong("Net-Flow Metric: "), "\u0394 = arrivals \u2212 departures over 2-hour windows."),
          htmltools::tags$li(htmltools::tags$strong("Rebalancing Classes: "), "Drainer (\u2264 -5), Collector (\u2265 +5), Balanced (-5 < \u0394 < 5)."),
          htmltools::tags$li(htmltools::tags$strong("Strict Chronological Split: "), "Training on Sept 1-21 (557,928 rows); Testing on Sept 22-30 (239,112 rows). Zero random splitting."),
          htmltools::tags$li(htmltools::tags$strong("Target Encoding Hygiene: "), "Station \u00d7 hour historical mean and SD computed strictly on training data and joined forward. Contemporaneous arrivals/departures strictly excluded.")
        )
      )
    ),
    
    htmltools::tags$div(style = "height: 12px;"),
    
    bslib::card(
      bslib::card_header("Honest Limitations of the Study"),
      htmltools::tags$ol(
        htmltools::tags$li(htmltools::tags$strong("Net-flow imbalance is not physical dock occupancy: "), "Citi Bike does not publish historical per-station bike inventories over time, so true empty/full stock-outs cannot be directly observed from trip logs."),
        htmltools::tags$li(htmltools::tags$strong("One-Month Temporal Snapshot: "), "September 2024 captures peak commuter volume but lacks winter freezing and seasonal snow disruptions."),
        htmltools::tags$li(htmltools::tags$strong("Censored Latent Demand: "), "When a station sits empty, aspiring riders walk away, generating unobserved latent demand."),
        htmltools::tags$li(htmltools::tags$strong("Unrecorded Van Transfers: "), "Operator rebalancing moves are not logged in raw customer trip transaction files."),
        htmltools::tags$li(htmltools::tags$strong("SVM Line Search Failure: "), "Support Vector Machines (SVM RBF) failed Platt scaling calibration during multinomial optimization in kernlab and is recorded as Failed.")
      ),
      htmltools::tags$div(
        style = "margin-top: 15px; text-align: right;",
        htmltools::tags$a(
          href = "../docs/final_report.html", target = "_blank", class = "btn btn-outline-info",
          "Open Full Technical Research Report (\u2197)"
        )
      )
    )
  ),
  
  # Right-side Dark / Light Theme Toggle
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::input_dark_mode(id = "dark_mode", mode = "dark")
  )
)

# ------------------------------------------------------------------------------
# 4. Application Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Track current dark theme status
  is_dark <- reactive({
    is.null(input$dark_mode) || input$dark_mode == "dark"
  })
  
  # Server-side selectize for station explorer
  updateSelectizeInput(session, "explorer_station", choices = avail_stns, selected = "W 21 St & 6 Ave", server = TRUE)
  
  # ----------------------------------------------------------------------------
  # Tab 2: Dispatch Map Reactives & Rendering
  # ----------------------------------------------------------------------------
  # Filter predictions for selected window
  dispatch_window_data <- reactive({
    req(input$dispatch_date, input$dispatch_time)
    test_preds |>
      dplyr::filter(date == input$dispatch_date, time_window == input$dispatch_time)
  })
  
  # Filter predictions for selected day
  dispatch_day_data <- reactive({
    req(input$dispatch_date)
    test_preds |>
      dplyr::filter(date == input$dispatch_date)
  })
  
  # Classify records based on dynamic thresholds
  classified_preds <- reactive({
    th_d <- req(input$thresh_drainer)
    th_c <- req(input$thresh_collector)
    
    scope <- req(input$dispatch_scope)
    df <- if (scope == "window") {
      dispatch_window_data()
    } else if (scope == "day") {
      dispatch_day_data()
    } else {
      test_preds
    }
    
    # Classification logic
    flag_d <- (df$prob_drainer >= th_d) & (df$prob_drainer >= df$prob_collector | df$prob_collector < th_c)
    flag_c <- (df$prob_collector >= th_c) & (df$prob_collector > df$prob_drainer | df$prob_drainer < th_d)
    
    assigned <- rep("Balanced", nrow(df))
    assigned[flag_d] <- "Drainer"
    assigned[flag_c] <- "Collector"
    
    df$flagged_class <- assigned
    df
  })
  
  # Live KPI Calculations
  output$kpi_alerts <- renderText({
    df <- classified_preds()
    alerts <- sum(df$flagged_class %in% c("Drainer", "Collector"))
    format(alerts, big.mark = ",")
  })
  
  output$kpi_correct <- renderText({
    df <- classified_preds()
    correct <- sum(df$flagged_class %in% c("Drainer", "Collector") & df$flagged_class == df$actual_class)
    format(correct, big.mark = ",")
  })
  
  output$kpi_false_alarms <- renderText({
    df <- classified_preds()
    fa <- sum(df$flagged_class %in% c("Drainer", "Collector") & df$flagged_class != df$actual_class)
    format(fa, big.mark = ",")
  })
  
  output$kpi_missed <- renderText({
    df <- classified_preds()
    missed <- sum(df$actual_class %in% c("Drainer", "Collector") & df$flagged_class != df$actual_class)
    format(missed, big.mark = ",")
  })
  
  output$kpi_precision <- renderText({
    df <- classified_preds()
    alerts <- sum(df$flagged_class %in% c("Drainer", "Collector"))
    correct <- sum(df$flagged_class %in% c("Drainer", "Collector") & df$flagged_class == df$actual_class)
    if (alerts == 0) return("N/A")
    sprintf("%.1f%%", (correct / alerts) * 100)
  })
  
  output$kpi_recall <- renderText({
    df <- classified_preds()
    actual_events <- sum(df$actual_class %in% c("Drainer", "Collector"))
    correct <- sum(df$flagged_class %in% c("Drainer", "Collector") & df$flagged_class == df$actual_class)
    if (actual_events == 0) return("N/A")
    sprintf("%.1f%%", (correct / actual_events) * 100)
  })
  
  # Leaflet Dispatch Map
  output$dispatch_leaflet_map <- leaflet::renderLeaflet({
    # Tile selection matching dark/light mode
    tile_provider <- if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
    
    leaflet::leaflet() |>
      leaflet::addProviderTiles(tile_provider) |>
      leaflet::setView(lng = -73.985, lat = 40.738, zoom = 12)
  })
  
  # Observe inputs and update markers without redrawing base map
  observe({
    df <- dispatch_window_data()
    th_d <- req(input$thresh_drainer)
    th_c <- req(input$thresh_collector)
    show_act <- isTRUE(input$show_actual)
    
    # Classify current window
    flag_d <- (df$prob_drainer >= th_d) & (df$prob_drainer >= df$prob_collector | df$prob_collector < th_c)
    flag_c <- (df$prob_collector >= th_c) & (df$prob_collector > df$prob_drainer | df$prob_drainer < th_d)
    
    assigned <- rep("Balanced", nrow(df))
    assigned[flag_d] <- "Drainer"
    assigned[flag_c] <- "Collector"
    df$display_class <- if (show_act) df$actual_class else assigned
    
    # Marker colors
    pal <- c("Drainer" = COLOR_DRAINER, "Collector" = COLOR_COLLECTOR, "Balanced" = COLOR_BALANCED)
    marker_colors <- pal[df$display_class]
    
    # Marker radii sized by probability
    max_prob <- pmax(df$prob_drainer, df$prob_collector, df$prob_balanced)
    marker_radii <- 4 + (max_prob * 7)
    
    # Detailed popups
    popups <- sprintf(
      "<strong>%s</strong><br/>
       Capacity: %d docks | Subway \u2264400m: %d<br/>
       Diurnal Profile: %s<br/>
       -----------------------------<br/>
       <strong>P(Drainer):</strong> %.1f%%<br/>
       <strong>P(Collector):</strong> %.1f%%<br/>
       <strong>P(Balanced):</strong> %.1f%%<br/>
       <strong>Flagged Class:</strong> %s<br/>
       <strong>Actual State:</strong> %s (\u0394 = %d)",
      df$station_name, df$capacity, df$subway_400m, df$personality,
      df$prob_drainer * 100, df$prob_collector * 100, df$prob_balanced * 100,
      assigned, df$actual_class, df$delta
    )
    
    leaflet::leafletProxy("dispatch_leaflet_map") |>
      leaflet::clearMarkers() |>
      leaflet::addCircleMarkers(
        lng = df$lng,
        lat = df$lat,
        radius = marker_radii,
        color = marker_colors,
        fillColor = marker_colors,
        fillOpacity = 0.75,
        weight = 1,
        popup = popups
      )
  })
  
  # Greedy Moves Suggestions Table
  output$dispatch_moves_table <- DT::renderDataTable({
    df <- dispatch_window_data()
    th_d <- req(input$thresh_drainer)
    th_c <- req(input$thresh_collector)
    
    collectors <- df |> dplyr::filter(prob_collector >= th_c)
    drainers   <- df |> dplyr::filter(prob_drainer >= th_d)
    
    moves <- match_greedy_moves(collectors, drainers, max_dist_km = 1.5)
    
    DT::datatable(
      moves,
      options = list(pageLength = 6, dom = 'tip', scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ----------------------------------------------------------------------------
  # Tab 3: Heatmaps Reactives & Rendering
  # ----------------------------------------------------------------------------
  output$leaflet_density_heatmap <- leaflet::renderLeaflet({
    tile_provider <- if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
    metric_col <- req(input$heatmap_metric)
    hr_range <- req(input$heatmap_hours)
    dtype <- req(input$heatmap_daytype)
    
    sub_df <- flow_hourly |>
      dplyr::filter(hour >= hr_range[1], hour <= hr_range[2])
    
    if (dtype != "all") {
      sub_df <- sub_df |> dplyr::filter(day_type == dtype)
    }
    
    agg_df <- sub_df |>
      dplyr::group_by(station_id, station_name, lat, lon) |>
      dplyr::summarise(weight = sum(.data[[metric_col]]), .groups = "drop") |>
      dplyr::filter(weight > 0)
    
    leaflet::leaflet(agg_df) |>
      leaflet::addProviderTiles(tile_provider) |>
      leaflet::setView(lng = -73.985, lat = 40.738, zoom = 12) |>
      leaflet.extras::addHeatmap(
        lng = ~lon,
        lat = ~lat,
        intensity = ~weight,
        blur = 18,
        max = max(agg_df$weight, na.rm = TRUE),
        radius = 14
      )
  })
  
  output$plotly_top25_heatmap <- plotly::renderPlotly({
    metric_col <- req(input$heatmap_metric)
    dtype <- req(input$heatmap_daytype)
    
    sub_df <- flow_hourly |>
      dplyr::filter(station_id %in% top25_stns$station_id)
    
    if (dtype != "all") {
      sub_df <- sub_df |> dplyr::filter(day_type == dtype)
    }
    
    agg_matrix <- sub_df |>
      dplyr::group_by(station_name, hour) |>
      dplyr::summarise(val = mean(.data[[metric_col]]), .groups = "drop")
    
    # Heatmap color scale
    p <- plotly::plot_ly(
      data = agg_matrix,
      x = ~hour,
      y = ~station_name,
      z = ~val,
      type = "heatmap",
      colorscale = if (metric_col == "net_outflow") "Reds" else if (metric_col == "net_inflow") "Blues" else "Viridis"
    )
    
    apply_plotly_theme(p, is_dark(),
                       title = paste("Diurnal", metric_col, "Across Top 25 Stations"),
                       x_title = "Hour of Day (0-23)", y_title = "")
  })
  
  # ----------------------------------------------------------------------------
  # Tab 4: Station Explorer Reactives & Rendering
  # ----------------------------------------------------------------------------
  selected_stn_meta <- reactive({
    req(input$explorer_station)
    flow_hourly |>
      dplyr::filter(station_name == input$explorer_station) |>
      dplyr::slice_head(n = 1)
  })
  
  output$stn_kpi_capacity <- renderText({
    meta <- selected_stn_meta()
    if (nrow(meta) == 0) return("N/A")
    paste(meta$capacity, "docks")
  })
  
  output$stn_kpi_subway <- renderText({
    meta <- selected_stn_meta()
    if (nrow(meta) == 0) return("N/A")
    paste(meta$subway_400m, "entrances")
  })
  
  output$stn_kpi_personality <- renderText({
    meta <- selected_stn_meta()
    if (nrow(meta) == 0) return("N/A")
    meta$personality
  })
  
  output$stn_kpi_cluster <- renderText({
    meta <- selected_stn_meta()
    if (nrow(meta) == 0) return("N/A")
    paste("Corridor #", meta$dbscan_cluster)
  })
  
  output$plotly_stn_profile <- plotly::renderPlotly({
    req(input$explorer_station)
    prof <- station_prof |>
      dplyr::filter(station_name == input$explorer_station)
    
    p <- plotly::plot_ly(prof, x = ~hour, y = ~avg_departures, color = ~day_type,
                         colors = c("Weekday" = "#38BDF8", "Weekend" = "#F59E0B"),
                         type = "scatter", mode = "lines+markers")
    
    apply_plotly_theme(p, is_dark(),
                       title = "Average Hourly Departures",
                       x_title = "Hour of Day", y_title = "Average Departures")
  })
  
  output$plotly_stn_timeline <- plotly::renderPlotly({
    req(input$explorer_station)
    th_d <- req(input$thresh_drainer)
    th_c <- req(input$thresh_collector)
    
    stn_preds <- test_preds |>
      dplyr::filter(station_name == input$explorer_station) |>
      dplyr::arrange(bin_start)
    
    p <- plotly::plot_ly(stn_preds, x = ~bin_start) |>
      plotly::add_trace(y = ~prob_drainer, name = "P(Drainer)", type = "scatter", mode = "lines",
                        line = list(color = COLOR_DRAINER, width = 2)) |>
      plotly::add_trace(y = ~prob_collector, name = "P(Collector)", type = "scatter", mode = "lines",
                        line = list(color = COLOR_COLLECTOR, width = 2)) |>
      plotly::add_trace(y = ~prob_balanced, name = "P(Balanced)", type = "scatter", mode = "lines",
                        line = list(color = COLOR_BALANCED, width = 1.5, dash = "dot"))
    
    # Add horizontal threshold reference lines
    p <- p |>
      plotly::add_segments(x = min(stn_preds$bin_start), xend = max(stn_preds$bin_start),
                           y = th_d, yend = th_d, name = "Drainer Threshold",
                           line = list(color = COLOR_DRAINER, dash = "dash", width = 1.2)) |>
      plotly::add_segments(x = min(stn_preds$bin_start), xend = max(stn_preds$bin_start),
                           y = th_c, yend = th_c, name = "Collector Threshold",
                           line = list(color = COLOR_COLLECTOR, dash = "dash", width = 1.2))
    
    apply_plotly_theme(p, is_dark(),
                       title = "Predicted Probabilities Across Test Period",
                       x_title = "Date & Window", y_title = "Probability [0, 1]")
  })
  
  # ----------------------------------------------------------------------------
  # Tab 5: Statistics and Spatial Rendering
  # ----------------------------------------------------------------------------
  output$table_stats_summary <- DT::renderDataTable({
    tbl <- tbl_bundle$stats_summary
    DT::datatable(
      tbl,
      options = list(pageLength = 5, dom = 't', scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$plotly_weather_irr <- plotly::renderPlotly({
    irr_df <- tbl_bundle$weather_nb_irr |>
      dplyr::filter(term %in% c("temperature_2m", "precipitation", "wind_speed_10m", "relative_humidity_2m")) |>
      dplyr::mutate(
        term_clean = c("Temperature (+1\u00b0C)", "Precipitation (+1mm)", "Wind Speed (+1km/h)", "Relative Humidity (+1%)"),
        significant = ifelse(p_value < 0.05, "Significant (p < 0.001)", "Insignificant (95% CI includes 1)")
      )
    
    p <- plotly::plot_ly(irr_df, x = ~irr, y = ~term_clean, type = "scatter", mode = "markers",
                         color = ~significant, colors = c("#94A3B8", "#38BDF8"),
                         error_x = list(type = "data",
                                        symmetric = FALSE,
                                        array = irr_df$irr_ci_high - irr_df$irr,
                                        arrayminus = irr_df$irr - irr_df$irr_ci_low))
    
    p <- apply_plotly_theme(p, is_dark(),
                            title = "Weather Incidence Rate Ratios (IRR) with 95% Confidence Intervals",
                            x_title = "Incidence Rate Ratio (IRR)", y_title = "")
    
    p |> plotly::layout(
      shapes = list(
        list(
          type = "line",
          x0 = 1, x1 = 1,
          y0 = 0, y1 = 1,
          yref = "paper",
          line = list(color = "#EF4444", dash = "dash", width = 1.5)
        )
      )
    )
  })
  
  output$table_silhouette_comp <- DT::renderDataTable({
    tbl <- tbl_bundle$cluster_silhouette_comparison
    DT::datatable(
      tbl,
      options = list(pageLength = 4, dom = 't'),
      rownames = FALSE
    )
  })
  
  output$table_ts_accuracy <- DT::renderDataTable({
    tbl <- tbl_bundle$ts_accuracy
    DT::datatable(
      tbl,
      options = list(pageLength = 4, dom = 't'),
      rownames = FALSE
    )
  })
  
  # ----------------------------------------------------------------------------
  # Tab 6: Model Lab Reactives & Rendering
  # ----------------------------------------------------------------------------
  output$plotly_model_benchmark <- plotly::renderPlotly({
    metric <- req(input$model_metric_select)
    mcomp <- tbl_bundle$model_comparison |>
      dplyr::mutate(
        val = as.numeric(.data[[metric]]),
        is_baseline = (model == "Baseline (Null)")
      ) |>
      dplyr::arrange(dplyr::desc(val))
    
    bar_colors <- ifelse(mcomp$is_baseline, "#94A3B8",
                         ifelse(is.na(mcomp$val), "#EF4444", "#38BDF8"))
    
    p <- plotly::plot_ly(
      mcomp,
      x = ~val,
      y = ~reorder(model, val),
      type = "bar",
      orientation = "h",
      marker = list(color = bar_colors)
    )
    
    apply_plotly_theme(p, is_dark(),
                       title = paste("Model Benchmark Sorted by", metric),
                       x_title = metric, y_title = "")
  })
  
  output$plotly_threshold_tradeoff <- plotly::renderPlotly({
    c_wasted <- req(input$cost_wasted)
    c_missed <- req(input$cost_missed)
    cur_th_d <- req(input$thresh_drainer)
    
    df <- thresh_curve |>
      dplyr::mutate(
        total_cost = (c_wasted * false_alarms) + (c_missed * missed_events)
      )
    
    best_th <- df$threshold[which.min(df$total_cost)]
    
    p <- plotly::plot_ly(df, x = ~threshold) |>
      plotly::add_trace(y = ~macro_prec, name = "Macro Precision", type = "scatter", mode = "lines+markers",
                        line = list(color = "#38BDF8")) |>
      plotly::add_trace(y = ~macro_rec, name = "Macro Recall", type = "scatter", mode = "lines+markers",
                        line = list(color = "#10B981")) |>
      plotly::add_trace(y = ~macro_f1, name = "Macro F1", type = "scatter", mode = "lines+markers",
                        line = list(color = "#F59E0B")) |>
      plotly::add_trace(y = ~ (total_cost / max(total_cost)), name = "Normalized Total Cost",
                        type = "scatter", mode = "lines", line = list(color = "#EF4444", dash = "dot")) |>
      plotly::add_segments(x = cur_th_d, xend = cur_th_d, y = 0, yend = 1,
                           name = "Current Slider Threshold",
                           line = list(color = "#E6EDF7", dash = "dash"))
    
    apply_plotly_theme(p, is_dark(),
                       title = sprintf("Precision / Recall / Economic Cost Trade-off (Optimal Threshold: %.2f)", best_th),
                       x_title = "Decision Threshold [0.05 - 0.95]", y_title = "Score / Scaled Cost")
  })
  
  # ----------------------------------------------------------------------------
  # Tab 7: What-If Scenario Sensitivity Simulation
  # ----------------------------------------------------------------------------
  output$plotly_whatif_comparison <- plotly::renderPlotly({
    req(input$whatif_station_select, input$whatif_window_select)
    stn_name <- input$whatif_station_select
    win_time <- as.POSIXct(input$whatif_window_select, tz = "America/New_York")
    
    row <- whatif_df |>
      dplyr::filter(station_name == stn_name, window_start == win_time)
    
    if (nrow(row) == 0) {
      row <- whatif_df |> dplyr::slice_head(n = 1)
    }
    
    # Baseline predictions
    model_path <- get_data_path("best_model.rds")
    best_wf <- readRDS(model_path)
    
    base_pred <- workflows:::predict.workflow(best_wf, new_data = row, type = "prob")
    
    # Counterfactual row
    cf_row <- row
    cf_row$temperature   <- req(input$whatif_temp)
    cf_row$precipitation <- req(input$whatif_precip)
    cf_row$wind          <- req(input$whatif_wind)
    
    cf_pred <- workflows:::predict.workflow(best_wf, new_data = cf_row, type = "prob")
    
    plot_df <- data.frame(
      Class = factor(c("Drainer", "Collector", "Balanced"), levels = c("Drainer", "Collector", "Balanced")),
      Baseline = c(base_pred$.pred_Drainer, base_pred$.pred_Collector, base_pred$.pred_Balanced),
      Counterfactual = c(cf_pred$.pred_Drainer, cf_pred$.pred_Collector, cf_pred$.pred_Balanced)
    )
    
    p <- plotly::plot_ly(plot_df, x = ~Class) |>
      plotly::add_bars(y = ~Baseline, name = "Baseline Weather Scenario", marker = list(color = "#94A3B8")) |>
      plotly::add_bars(y = ~Counterfactual, name = "Counterfactual Weather Scenario", marker = list(color = "#38BDF8"))
    
    apply_plotly_theme(p, is_dark(),
                       title = paste("Probability Shift for", stn_name, "at", format(win_time, "%Y-%m-%d %H:%M")),
                       x_title = "Rebalancing State", y_title = "Predicted Probability")
  })
}

# ------------------------------------------------------------------------------
# 5. App Runner Entry Point
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
