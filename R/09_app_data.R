# R/09_app_data.R
# Project: citibike-rebalancing
# Purpose: Precompute and bundle small, optimized RDS artifacts and static assets
#          for the interactive Shiny dashboard in app/
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Helper for formatted timestamped logging
log_msg <- function(stage, message) {
  cat(sprintf("[%s] [%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stage, message))
}

#' 1. Prepare and Join Test Predictions
#'
#' Joins 239,112 test predictions with station metadata (capacity, subway, clusters)
#' and formats timestamps for instant UI filtering.
prepare_test_predictions <- function(pred_path = "data/processed/test_predictions.rds",
                                     stn_path = "results/tables/station_features_final.csv",
                                     out_path = "app/data/test_predictions.rds") {
  log_msg("START", "Preparing test_predictions with station metadata...")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  
  preds <- readRDS(pred_path)
  stns  <- readr::read_csv(stn_path, show_col_types = FALSE)
  
  # Select metadata columns to merge
  meta <- stns |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::select(
      station_id,
      capacity,
      subway_400m,
      personality,
      dbscan_cluster
    )
  
  preds <- preds |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::left_join(meta, by = "station_id") |>
    dplyr::mutate(
      capacity = ifelse(is.na(capacity), 25, capacity),
      subway_400m = ifelse(is.na(subway_400m), 0, subway_400m),
      personality = ifelse(is.na(personality), "Balanced Diurnal Plateau", personality),
      dbscan_cluster = ifelse(is.na(dbscan_cluster), 0, dbscan_cluster),
      bin_start = as.POSIXct(bin_start, tz = "America/New_York"),
      date = format(bin_start, "%Y-%m-%d"),
      time_window = format(bin_start, "%H:%M")
    )
  
  saveRDS(preds, out_path, compress = "xz")
  log_msg("END", sprintf("Saved %d test predictions to '%s' (Size: %.2f MB)",
                         nrow(preds), out_path, file.info(out_path)$size / (1024 * 1024)))
  preds
}

#' 2. Aggregate Station Hourly Flow Tables from SQLite
#'
#' Aggregates departures and arrivals by station x hour x day_type (weekday/weekend)
#' directly via SQL to avoid loading 4.7M raw trips in the Shiny server.
prepare_station_flow_tables <- function(db_path = "data/citibike.sqlite",
                                        stn_path = "results/tables/station_features_final.csv",
                                        out_flow_path = "app/data/station_hour_daytype.rds",
                                        out_prof_path = "app/data/station_departure_profiles.rds",
                                        out_top25_path = "app/data/top25_stations.rds") {
  log_msg("START", "Aggregating station hourly flows from SQLite database...")
  dir.create(dirname(out_flow_path), recursive = TRUE, showWarnings = FALSE)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  # Aggregated departures
  q_dep <- "
    SELECT start_station_id AS station_id,
           CAST(strftime('%H', started_at) AS INTEGER) AS hour,
           CASE WHEN strftime('%w', started_at) IN ('0', '6') THEN 'Weekend' ELSE 'Weekday' END AS day_type,
           COUNT(*) AS departures
    FROM trips_clean
    GROUP BY station_id, hour, day_type
  "
  dep_df <- DBI::dbGetQuery(con, q_dep)
  
  # Aggregated arrivals
  q_arr <- "
    SELECT end_station_id AS station_id,
           CAST(strftime('%H', ended_at) AS INTEGER) AS hour,
           CASE WHEN strftime('%w', ended_at) IN ('0', '6') THEN 'Weekend' ELSE 'Weekday' END AS day_type,
           COUNT(*) AS arrivals
    FROM trips_clean
    GROUP BY station_id, hour, day_type
  "
  arr_df <- DBI::dbGetQuery(con, q_arr)
  DBI::dbDisconnect(con)
  
  # Join departures and arrivals
  flow_df <- dplyr::full_join(
    dep_df, arr_df,
    by = c("station_id", "hour", "day_type")
  ) |>
    dplyr::mutate(
      departures = ifelse(is.na(departures), 0, departures),
      arrivals   = ifelse(is.na(arrivals), 0, arrivals),
      net_flow   = arrivals - departures,
      net_outflow = pmax(0, departures - arrivals), # drainer pressure
      net_inflow  = pmax(0, arrivals - departures)  # collector pressure
    )
  
  # Join station metadata (name, coordinates, capacity, subway, clusters)
  stns <- readr::read_csv(stn_path, show_col_types = FALSE) |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::select(
      station_id,
      station_name,
      lat,
      lon,
      capacity,
      subway_400m,
      dist_nearest_subway_m,
      dbscan_cluster,
      personality
    )
  
  flow_df <- flow_df |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::inner_join(stns, by = "station_id")
  
  saveRDS(flow_df, out_flow_path, compress = "xz")
  log_msg("INFO", sprintf("Saved station hourly flow table (%d rows) to '%s'", nrow(flow_df), out_flow_path))
  
  # Compute 24-hour departure profile per station
  prof_df <- flow_df |>
    dplyr::group_by(station_id, station_name, hour, day_type) |>
    dplyr::summarise(
      avg_departures = mean(departures),
      avg_arrivals   = mean(arrivals),
      avg_net_flow   = mean(net_flow),
      .groups = "drop"
    )
  saveRDS(prof_df, out_prof_path, compress = "xz")
  log_msg("INFO", sprintf("Saved station 24h departure profiles (%d rows) to '%s'", nrow(prof_df), out_prof_path))
  
  # Identify Top 25 busiest stations citywide
  top25 <- flow_df |>
    dplyr::group_by(station_id, station_name, lat, lon) |>
    dplyr::summarise(
      total_activity = sum(departures + arrivals),
      total_departures = sum(departures),
      total_arrivals = sum(arrivals),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_activity)) |>
    dplyr::slice_head(n = 25)
  
  saveRDS(top25, out_top25_path, compress = "xz")
  log_msg("END", sprintf("Saved top 25 busiest stations to '%s'", out_top25_path))
  
  list(flow = flow_df, profile = prof_df, top25 = top25)
}

#' 3. Bundle Analysis Tables
#'
#' Reads all precomputed results CSV tables and bundles them into an RDS list
#' for instant retrieval by the Shiny application.
bundle_analysis_tables <- function(tables_dir = "results/tables",
                                   out_bundle_path = "app/data/tables_bundle.rds") {
  log_msg("START", "Bundling precomputed results tables...")
  dir.create(dirname(out_bundle_path), recursive = TRUE, showWarnings = FALSE)
  
  tbl_names <- c(
    "model_comparison",
    "feature_selection",
    "tuning_before_after",
    "best_params",
    "model_ci",
    "stats_summary",
    "weather_nb_irr",
    "cluster_silhouette_comparison",
    "ts_accuracy",
    "cleaning_audit",
    "eda_summary",
    "station_asymmetry_regression"
  )
  
  bundle <- list()
  for (nm in tbl_names) {
    csv_file <- file.path(tables_dir, paste0(nm, ".csv"))
    if (file.exists(csv_file)) {
      bundle[[nm]] <- readr::read_csv(csv_file, show_col_types = FALSE)
      # Also save individual RDS file for direct loading if needed
      saveRDS(bundle[[nm]], file.path("app/data", paste0(nm, ".rds")))
    } else {
      warning(sprintf("Table file '%s' not found.", csv_file))
    }
  }
  
  saveRDS(bundle, out_bundle_path)
  log_msg("END", sprintf("Saved %d bundled tables to '%s'", length(bundle), out_bundle_path))
  bundle
}

#' 4. Precompute Alert Threshold Sensitivity Curve
#'
#' Evaluates alerts, precision, recall, false alarms, and missed events
#' across a 0.05 to 0.95 threshold grid to power the interactive trade-off chart.
precompute_threshold_curve <- function(preds,
                                       out_curve_path = "app/data/threshold_curve.rds") {
  log_msg("START", "Precomputing threshold trade-off curve across full test period...")
  dir.create(dirname(out_curve_path), recursive = TRUE, showWarnings = FALSE)
  
  thresholds <- seq(0.05, 0.95, by = 0.05)
  n_total <- nrow(preds)
  
  act_drainer   <- preds$actual_class == "Drainer"
  act_collector <- preds$actual_class == "Collector"
  act_balanced  <- preds$actual_class == "Balanced"
  
  curve_list <- lapply(thresholds, function(th) {
    # Decision rule:
    # Flag Drainer if P(Drainer) >= th
    # Flag Collector if P(Collector) >= th
    # If both, choose higher probability
    flag_drainer   <- (preds$prob_drainer >= th) & (preds$prob_drainer >= preds$prob_collector | preds$prob_collector < th)
    flag_collector <- (preds$prob_collector >= th) & (preds$prob_collector > preds$prob_drainer | preds$prob_drainer < th)
    
    # Drainer metrics
    tp_d <- sum(flag_drainer & act_drainer)
    fp_d <- sum(flag_drainer & !act_drainer)
    fn_d <- sum(!flag_drainer & act_drainer)
    
    prec_d <- ifelse(tp_d + fp_d > 0, tp_d / (tp_d + fp_d), 0)
    rec_d  <- ifelse(tp_d + fn_d > 0, tp_d / (tp_d + fn_d), 0)
    f1_d   <- ifelse(prec_d + rec_d > 0, 2 * prec_d * rec_d / (prec_d + rec_d), 0)
    
    # Collector metrics
    tp_c <- sum(flag_collector & act_collector)
    fp_c <- sum(flag_collector & !act_collector)
    fn_c <- sum(!flag_collector & act_collector)
    
    prec_c <- ifelse(tp_c + fp_c > 0, tp_c / (tp_c + fp_c), 0)
    rec_c  <- ifelse(tp_c + fn_c > 0, tp_c / (tp_c + fn_c), 0)
    f1_c   <- ifelse(prec_c + rec_c > 0, 2 * prec_c * rec_c / (prec_c + rec_c), 0)
    
    # Aggregates
    total_alerts  <- (tp_d + fp_d) + (tp_c + fp_c)
    false_alarms  <- fp_d + fp_c
    missed_events <- fn_d + fn_c
    macro_prec    <- (prec_d + prec_c) / 2
    macro_rec     <- (rec_d + rec_c) / 2
    macro_f1      <- (f1_d + f1_c) / 2
    
    dplyr::tibble(
      threshold     = th,
      prec_drainer  = round(prec_d, 4),
      rec_drainer   = round(rec_d, 4),
      f1_drainer    = round(f1_d, 4),
      prec_collector = round(prec_c, 4),
      rec_collector = round(rec_c, 4),
      f1_collector  = round(f1_c, 4),
      macro_prec    = round(macro_prec, 4),
      macro_rec     = round(macro_rec, 4),
      macro_f1      = round(macro_f1, 4),
      total_alerts  = total_alerts,
      false_alarms  = false_alarms,
      missed_events = missed_events
    )
  })
  
  curve_df <- dplyr::bind_rows(curve_list)
  saveRDS(curve_df, out_curve_path)
  log_msg("END", sprintf("Saved threshold sensitivity curve (%d rows) to '%s'", nrow(curve_df), out_curve_path))
  curve_df
}

#' 5. Prepare What-If Scenario Dataset & Best Model Copy
#'
#' Selects representative station test rows formatted exactly for workflows:::predict.workflow
#' and copies the fitted Ranger workflow to app/data/best_model.rds.
prepare_whatif_scenario_data <- function(panel_path = "data/processed/panel.rds",
                                         model_path = "data/processed/best_model.rds",
                                         stn_path = "results/tables/station_features_final.csv",
                                         out_data_path = "app/data/whatif_data.rds",
                                         out_model_path = "app/data/best_model.rds") {
  log_msg("START", "Preparing What-If sensitivity data and copying model...")
  dir.create(dirname(out_data_path), recursive = TRUE, showWarnings = FALSE)
  
  panel <- readRDS(panel_path)
  stns  <- readr::read_csv(stn_path, show_col_types = FALSE)
  
  # Filter test period
  split_time <- as.POSIXct("2024-09-22 00:00:00", tz = "America/New_York")
  train_full <- panel |> dplyr::filter(window_start < split_time)
  test_full  <- panel |> dplyr::filter(window_start >= split_time)
  
  # Leakage-safe target encoding computed strictly on train_full
  te_train <- train_full |>
    dplyr::group_by(station_id, window_start_hour) |>
    dplyr::summarise(
      station_hour_mean_delta = mean(delta, na.rm = TRUE),
      station_hour_sd_delta   = stats::sd(delta, na.rm = TRUE),
      .groups = "drop"
    )
  
  global_mean_delta <- mean(train_full$delta, na.rm = TRUE)
  global_sd_delta   <- stats::sd(train_full$delta, na.rm = TRUE)
  
  test_full <- test_full |>
    dplyr::left_join(te_train, by = c("station_id", "window_start_hour")) |>
    dplyr::mutate(
      station_hour_mean_delta = ifelse(is.na(station_hour_mean_delta), global_mean_delta, station_hour_mean_delta),
      station_hour_sd_delta   = ifelse(is.na(station_hour_sd_delta) | is.nan(station_hour_sd_delta), global_sd_delta, station_hour_sd_delta)
    )
  
  # Add station names
  stn_meta <- stns |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::select(station_id, station_name)
  test_full <- test_full |>
    dplyr::mutate(station_id = as.character(station_id)) |>
    dplyr::inner_join(stn_meta, by = "station_id")
  
  # Select 50 high-volume stations across 4 distinct representative windows
  top_stations <- stns |>
    dplyr::arrange(dplyr::desc(departures + arrivals)) |>
    dplyr::slice_head(n = 60) |>
    dplyr::pull(station_id) |>
    as.character()
  
  target_windows <- as.POSIXct(c(
    "2024-09-23 08:00:00", # Weekday Morning Rush
    "2024-09-23 18:00:00", # Weekday Evening Rush
    "2024-09-28 14:00:00", # Weekend Midday Leisure
    "2024-09-25 02:00:00"  # Late Night Plateau
  ), tz = "America/New_York")
  
  whatif_rows <- test_full |>
    dplyr::filter(station_id %in% top_stations, window_start %in% target_windows) |>
    dplyr::mutate(
      sin_hour = sin(2 * pi * window_start_hour / 24),
      cos_hour = cos(2 * pi * window_start_hour / 24),
      capacity_missing = as.integer(is.na(capacity)),
      capacity = ifelse(is.na(capacity), 25, capacity),
      temperature = mean_temperature,
      precipitation = total_precipitation,
      wind = mean_wind,
      humidity = mean_humidity,
      day_of_week = factor(as.character(day_of_week), ordered = FALSE),
      is_weekend = as.numeric(is_weekend),
      is_holiday = as.numeric(is_holiday),
      dbscan_cluster = factor(as.character(dbscan_cluster)),
      personality = factor(as.character(personality)),
      target = factor(
        ifelse(delta <= -5, "Drainer", ifelse(delta >= 5, "Collector", "Balanced")),
        levels = c("Drainer", "Collector", "Balanced")
      )
    )
  
  saveRDS(whatif_rows, out_data_path)
  log_msg("INFO", sprintf("Saved %d What-If scenario test rows to '%s'", nrow(whatif_rows), out_data_path))
  
  # Copy fitted workflow
  if (file.exists(model_path)) {
    file.copy(model_path, out_model_path, overwrite = TRUE)
    log_msg("INFO", sprintf("Copied fitted best model to '%s'", out_model_path))
  }
  
  whatif_rows
}

#' 6. Copy Static Assets (PNG Figures & HTML Maps)
#'
#' Replicates all figures and interactive HTML maps to app/www/ for Shiny serving.
copy_static_assets <- function(fig_src = "results/figures",
                               map_src = "results/maps",
                               www_dest = "app/www") {
  log_msg("START", "Copying figures and maps to app/www/...")
  fig_dest <- file.path(www_dest, "figures")
  map_dest <- file.path(www_dest, "maps")
  
  dir.create(fig_dest, recursive = TRUE, showWarnings = FALSE)
  dir.create(map_dest, recursive = TRUE, showWarnings = FALSE)
  
  # Copy figures
  figs <- list.files(fig_src, pattern = "\\.png$", full.names = TRUE)
  file.copy(figs, fig_dest, overwrite = TRUE)
  log_msg("INFO", sprintf("Copied %d PNG figures to '%s'", length(figs), fig_dest))
  
  # Copy maps
  maps <- list.files(map_src, pattern = "\\.html$", full.names = TRUE)
  file.copy(maps, map_dest, overwrite = TRUE)
  log_msg("INFO", sprintf("Copied %d HTML maps to '%s'", length(maps), map_dest))
  
  log_msg("END", "Static assets mirrored successfully.")
}

# ==============================================================================
# Runner: Execute All Data Bundling Tasks
# ==============================================================================
main <- function() {
  t0 <- Sys.time()
  log_msg("START", "==== Building App Data Artifacts (R/09_app_data.R) ====")
  
  # 1. Test predictions
  preds <- prepare_test_predictions()
  
  # 2. Station flow aggregations
  prepare_station_flow_tables()
  
  # 3. Bundle CSV tables
  bundle_analysis_tables()
  
  # 4. Threshold trade-off curve
  precompute_threshold_curve(preds)
  
  # 5. What-if sensitivity data
  prepare_whatif_scenario_data()
  
  # 6. Copy static assets
  copy_static_assets()
  
  t1 <- Sys.time()
  log_msg("END", sprintf("==== App data preparation complete in %.2f seconds ====",
                         as.numeric(difftime(t1, t0, units = "secs"))))
}

if (!interactive()) {
  main()
}
