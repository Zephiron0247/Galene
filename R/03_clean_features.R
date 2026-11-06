# ==============================================================================
# R/03_clean_features.R
# Project: citibike-rebalancing
# Purpose: Data cleaning pipeline, feature engineering, spatial subway proximity,
#          station flow asymmetry, panel creation (2-hour bins), and hourly demand.
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Step 0: Load environment setup and shared utilities
source("R/00_setup.R")
source("R/utils.R")

#' Audit and record column missingness of the raw trips table
#'
#' Evaluates missing values (NA, empty strings, "NA" tokens) across all columns
#' of the raw ingested trips dataset and writes summary to CSV.
#'
#' @param raw_trips Raw trips data frame or tibble.
#' @param out_file Destination path for the CSV report.
#' @return A tibble of column missingness metrics.
audit_raw_missingness <- function(raw_trips,
                                  out_file = "results/tables/missingness_raw.csv") {
  message("Auditing column missingness on raw trips dataset...")
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

  n_total <- nrow(raw_trips)
  col_names <- names(raw_trips)

  missing_counts <- vapply(raw_trips, function(col) {
    sum(is.na(col) | col == "" | col == "NA" | col == "N/A")
  }, integer(1))

  missing_tbl <- dplyr::tibble(
    column_name   = col_names,
    missing_count = as.integer(missing_counts),
    missing_pct   = round(100 * (missing_counts / n_total), 4),
    total_rows    = n_total
  )

  readr::write_csv(missing_tbl, out_file)
  message(sprintf("Saved raw missingness audit to '%s'.", out_file))
  return(missing_tbl)
}

#' Sequentially clean trips and record audit steps
#'
#' Applies the exact sequential cleaning pipeline per project specifications:
#' 1. Parse timestamps (America/New_York)
#' 2. Drop missing coordinates or station IDs
#' 3. Drop start >= end (started_at < ended_at)
#' 4. Drop duration <= 0 (trip_duration_mins > 0)
#' 5. Tukey IQR duration filter (iqr_inlier_mask)
#' 6. Haversine distance_km calculation
#' 7. Drop speed > 45 km/h (trip_speed_kmh <= 45)
#'
#' @param raw_trips Raw trips data frame.
#' @param audit_file Destination path for cleaning audit log CSV.
#' @return A cleaned tibble of trips with engineered attributes.
clean_trips_sequential <- function(raw_trips,
                                   audit_file = "results/tables/cleaning_audit.csv") {
  message("Starting sequential trip cleaning and validation pipeline...")
  dir.create(dirname(audit_file), recursive = TRUE, showWarnings = FALSE)

  audit_records <- list()
  record_step <- function(step_num, desc, n_rows) {
    audit_records[[length(audit_records) + 1]] <<- dplyr::tibble(
      step_number     = as.integer(step_num),
      step_description = desc,
      rows_remaining  = as.integer(n_rows)
    )
    message(sprintf(" - Step %d: %s -> %s rows remaining.",
                    step_num, desc, format(n_rows, big.mark = ",")))
  }

  # Step 0: Raw Ingestion Count
  current_df <- raw_trips
  record_step(0, "Raw Ingested Records", nrow(current_df))

  # Step 1: Parse timestamps (America/New_York)
  message("Parsing timestamps (America/New_York)...")
  current_df <- current_df |>
    dplyr::mutate(
      started_at = lubridate::ymd_hms(started_at, tz = "America/New_York", quiet = TRUE),
      ended_at   = lubridate::ymd_hms(ended_at, tz = "America/New_York", quiet = TRUE)
    )
  record_step(1, "Parse Timestamps (America/New_York)", nrow(current_df))

  # Step 2: Drop missing coordinates / station IDs / invalid timestamps
  current_df <- current_df |>
    dplyr::filter(
      !is.na(start_lat),
      !is.na(start_lng),
      !is.na(end_lat),
      !is.na(end_lng),
      !is.na(start_station_id),
      !is.na(end_station_id),
      nzchar(start_station_id),
      nzchar(end_station_id),
      !is.na(started_at),
      !is.na(ended_at)
    )
  record_step(2, "Drop Missing Coordinates and Station IDs", nrow(current_df))

  # Step 3: Drop start >= end
  current_df <- current_df |>
    dplyr::filter(started_at < ended_at)
  record_step(3, "Drop start >= end", nrow(current_df))

  # Step 4: Drop duration <= 0
  current_df <- current_df |>
    dplyr::mutate(
      trip_duration_mins = as.numeric(difftime(ended_at, started_at, units = "mins"))
    ) |>
    dplyr::filter(!is.na(trip_duration_mins), trip_duration_mins > 0)
  record_step(4, "Drop Duration <= 0", nrow(current_df))

  # Step 5: Tukey IQR duration filter
  current_df <- current_df |>
    dplyr::filter(iqr_inlier_mask(trip_duration_mins))
  record_step(5, "Tukey IQR Duration Filter", nrow(current_df))

  # Step 6: Haversine distance_km and trip speed
  current_df <- current_df |>
    dplyr::mutate(
      start_hour  = lubridate::hour(started_at),
      day_of_week = ordered(
        lubridate::wday(started_at, label = TRUE, abbr = FALSE, week_start = 1),
        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      ),
      is_weekend  = lubridate::wday(started_at, week_start = 1) %in% c(6L, 7L),
      distance_km = haversine_km(start_lat, start_lng, end_lat, end_lng),
      trip_speed_kmh = distance_km / (trip_duration_mins / 60)
    )
  record_step(6, "Compute Haversine Distance and Speed", nrow(current_df))

  # Step 7: Drop speed > 45 km/h
  current_df <- current_df |>
    dplyr::filter(is.finite(trip_speed_kmh), trip_speed_kmh <= 45)
  record_step(7, "Drop Speed > 45 km/h", nrow(current_df))

  # Compile audit table with rows dropped & retention percentages
  audit_tbl <- dplyr::bind_rows(audit_records) |>
    dplyr::mutate(
      rows_dropped = dplyr::lag(rows_remaining, default = dplyr::first(rows_remaining)) - rows_remaining,
      pct_retained = round(100 * (rows_remaining / dplyr::first(rows_remaining)), 2)
    )

  readr::write_csv(audit_tbl, audit_file)
  message(sprintf("Saved cleaning audit log to '%s'.", audit_file))

  return(dplyr::as_tibble(current_df))
}

#' Compute station departures, arrivals, net flow, and asymmetry index via SQL
#'
#' Uses DBI::dbGetQuery with a SQL GROUP BY query to compute total departures,
#' arrivals, net flow, and asymmetry index per station from the cleaned trips.
#'
#' @param con Active DBI SQLite connection.
#' @return A tibble with station flow metrics.
compute_station_flows_sql <- function(con) {
  message("Computing station departures and arrivals via SQL GROUP BY (DBI)...")

  sql_query <- "
    SELECT
      s.station_id,
      COALESCE(d.station_name, a.station_name) AS station_name,
      COALESCE(d.lat, a.lat) AS lat,
      COALESCE(d.lon, a.lon) AS lon,
      COALESCE(d.departures, 0) AS departures,
      COALESCE(a.arrivals, 0) AS arrivals,
      (COALESCE(a.arrivals, 0) - COALESCE(d.departures, 0)) AS net_flow,
      CASE
        WHEN (COALESCE(a.arrivals, 0) + COALESCE(d.departures, 0)) > 0
        THEN CAST(COALESCE(a.arrivals, 0) - COALESCE(d.departures, 0) AS REAL) /
             (COALESCE(a.arrivals, 0) + COALESCE(d.departures, 0))
        ELSE 0.0
      END AS asymmetry_index
    FROM (
      SELECT start_station_id AS station_id FROM trips_clean
      UNION
      SELECT end_station_id AS station_id FROM trips_clean
    ) s
    LEFT JOIN (
      SELECT
        start_station_id AS station_id,
        start_station_name AS station_name,
        AVG(start_lat) AS lat,
        AVG(start_lng) AS lon,
        COUNT(*) AS departures
      FROM trips_clean
      GROUP BY start_station_id
    ) d ON s.station_id = d.station_id
    LEFT JOIN (
      SELECT
        end_station_id AS station_id,
        end_station_name AS station_name,
        AVG(end_lat) AS lat,
        AVG(end_lng) AS lon,
        COUNT(*) AS arrivals
      FROM trips_clean
      GROUP BY end_station_id
    ) a ON s.station_id = a.station_id;
  "

  station_flows <- DBI::dbGetQuery(con, sql_query)
  message(sprintf("Extracted flow metrics for %d distinct docking stations.", nrow(station_flows)))
  return(dplyr::as_tibble(station_flows))
}

#' Join station capacity from GBFS stations table
#'
#' Performs left join of station capacity matching short_name = station_id.
#' Reports unmatched stations and retains them with NA capacity.
#'
#' @param station_flows Tibble of station flow metrics.
#' @param con Active DBI SQLite connection.
#' @return A tibble with joined capacity column.
join_station_capacity <- function(station_flows, con) {
  message("Joining station capacity from 'stations' table on short_name = station_id...")

  stations_meta <- DBI::dbGetQuery(con, "SELECT short_name, capacity FROM stations") |>
    dplyr::as_tibble()

  # Deduplicate metadata if any repeated short_names exist
  stations_meta <- stations_meta |>
    dplyr::distinct(short_name, .keep_all = TRUE)

  joined <- station_flows |>
    dplyr::left_join(stations_meta, by = c("station_id" = "short_name"))

  unmatched_count <- sum(is.na(joined$capacity))
  message(sprintf("Station capacity join: %d stations did not match station_information.csv and were retained with NA capacity.",
                  unmatched_count))

  return(joined)
}

#' Compute subway entrance proximity metrics using sf
#'
#' Converts subway entrances and stations to sf objects (CRS 4326), projects
#' to UTM Zone 18N (EPSG:32618), and computes:
#' 1. subway_400m: count of subway entrances within 400 meters.
#' 2. dist_nearest_subway_m: distance in meters to the nearest subway entrance.
#'
#' @param station_features Tibble of station features.
#' @param con Active DBI SQLite connection.
#' @return Updated station_features tibble with subway proximity columns.
compute_subway_proximity_sf <- function(station_features, con) {
  message("Calculating subway entrance spatial proximity metrics via sf (EPSG:32618)...")

  # Step 1: Ingest subway entrance coordinates from database
  subway_df <- DBI::dbGetQuery(
    con,
    "SELECT entrance_longitude, entrance_latitude FROM subway WHERE entrance_latitude IS NOT NULL AND entrance_longitude IS NOT NULL"
  )

  # Step 2: Build projected subway entrances layer
  subway_sf <- sf::st_as_sf(
    subway_df,
    coords = c("entrance_longitude", "entrance_latitude"),
    crs = 4326
  )
  subway_proj <- sf::st_transform(subway_sf, 32618)

  # Step 3: Identify stations with valid coordinates
  valid_idx <- which(!is.na(station_features$lat) & !is.na(station_features$lon))
  stn_valid <- station_features[valid_idx, ]

  stn_sf <- sf::st_as_sf(stn_valid, coords = c("lon", "lat"), crs = 4326)
  stn_proj <- sf::st_transform(stn_sf, 32618)

  # Step 4: Compute distance matrix (stations x subway entrances)
  dist_mat <- sf::st_distance(stn_proj, subway_proj)

  # Step 5: Extract minimum distance and count within 400 m
  nearest_dist <- as.numeric(apply(dist_mat, 1, min))
  count_400m   <- as.integer(apply(dist_mat, 1, function(d) sum(d <= 400)))

  # Initialize columns
  station_features$subway_400m <- NA_integer_
  station_features$dist_nearest_subway_m <- NA_real_

  station_features$subway_400m[valid_idx] <- count_400m
  station_features$dist_nearest_subway_m[valid_idx] <- round(nearest_dist, 1)

  message("Subway proximity calculation complete.")
  return(station_features)
}

#' Construct 2-hour station time-series panel
#'
#' Builds a balanced station x 2-hour panel for the full month of September 2024
#' for every station with at least 50 total trips. Completes all missing windows
#' with 0 trips, computes temporal features (lags, pressure, calendar variables),
#' and merges 2-hour aggregated weather.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param station_features Station-level features tibble.
#' @param con Active DBI SQLite connection.
#' @return A balanced panel tibble.
build_station_panel <- function(trips_clean, station_features, con) {
  message("Building station x 2-hour time-series panel for September 2024...")

  # Step 1: Filter stations with at least 50 total trips
  active_stns <- station_features |>
    dplyr::filter((departures + arrivals) >= 50) |>
    dplyr::pull(station_id)

  message(sprintf("Identified %d active stations with >= 50 trips.", length(active_stns)))

  # Step 2: Define full month 2-hour bin sequence (360 bins for Sept 1 - Sept 30)
  start_bound <- as.POSIXct("2024-09-01 00:00:00", tz = "America/New_York")
  end_bound   <- as.POSIXct("2024-09-30 22:00:00", tz = "America/New_York")
  month_end   <- as.POSIXct("2024-10-01 00:00:00", tz = "America/New_York")

  all_windows <- seq(from = start_bound, to = end_bound, by = "2 hours")

  # Step 3: Aggregate departures by 2-hour window
  dep_binned <- trips_clean |>
    dplyr::filter(
      start_station_id %in% active_stns,
      started_at >= start_bound,
      started_at < month_end
    ) |>
    dplyr::mutate(window_start = lubridate::floor_date(started_at, "2 hours")) |>
    dplyr::count(station_id = start_station_id, window_start, name = "departures")

  # Step 4: Aggregate arrivals by 2-hour window
  arr_binned <- trips_clean |>
    dplyr::filter(
      end_station_id %in% active_stns,
      ended_at >= start_bound,
      ended_at < month_end
    ) |>
    dplyr::mutate(window_start = lubridate::floor_date(ended_at, "2 hours")) |>
    dplyr::count(station_id = end_station_id, window_start, name = "arrivals")

  # Step 5: Merge flows and balance with tidyr::complete
  panel <- dplyr::full_join(dep_binned, arr_binned, by = c("station_id", "window_start")) |>
    dplyr::mutate(
      departures = tidyr::replace_na(departures, 0L),
      arrivals   = tidyr::replace_na(arrivals, 0L)
    ) |>
    tidyr::complete(
      station_id   = active_stns,
      window_start = all_windows,
      fill = list(departures = 0L, arrivals = 0L)
    ) |>
    dplyr::arrange(station_id, window_start)

  # Step 6: Join static station features and compute deltas, pressure, and lags
  panel <- panel |>
    dplyr::left_join(
      station_features |>
        dplyr::select(station_id, capacity, subway_400m, dist_nearest_subway_m),
      by = "station_id"
    ) |>
    dplyr::mutate(
      delta             = arrivals - departures,
      pressure          = dplyr::if_else(!is.na(capacity) & capacity > 0, delta / capacity, NA_real_),
      window_start_hour = lubridate::hour(window_start),
      day_of_week       = lubridate::wday(window_start, label = TRUE, abbr = FALSE, week_start = 1),
      is_weekend        = lubridate::wday(window_start, week_start = 1) %in% c(6L, 7L),
      is_holiday        = as.Date(window_start) == as.Date("2024-09-02")
    ) |>
    dplyr::group_by(station_id) |>
    dplyr::mutate(
      lag1_delta    = dplyr::lag(delta, 1),
      lag2_delta    = dplyr::lag(delta, 2),
      lag1_pressure = dplyr::lag(pressure, 1)
    ) |>
    dplyr::ungroup()

  # Step 7: Aggregate weather to 2-hour intervals and join
  weather_raw <- DBI::dbGetQuery(con, "SELECT * FROM weather") |>
    dplyr::as_tibble()

  weather_2h <- weather_raw |>
    dplyr::mutate(
      time_dt = lubridate::as_datetime(time, tz = "America/New_York"),
      window_start = lubridate::floor_date(time_dt, "2 hours")
    ) |>
    dplyr::group_by(window_start) |>
    dplyr::summarize(
      mean_temperature    = round(mean(temperature_2m, na.rm = TRUE), 2),
      total_precipitation = round(sum(precipitation, na.rm = TRUE), 2),
      mean_wind           = round(mean(wind_speed_10m, na.rm = TRUE), 2),
      mean_humidity       = round(mean(relative_humidity_2m, na.rm = TRUE), 2),
      .groups = "drop"
    )

  panel <- panel |>
    dplyr::left_join(weather_2h, by = "window_start")

  message(sprintf("Constructed panel with %s rows across %d active stations.",
                  format(nrow(panel), big.mark = ","), length(active_stns)))

  return(panel)
}

#' Build hourly citywide demand dataset
#'
#' Aggregates clean trips into hourly citywide totals for member and casual riders,
#' and joins contemporaneous hourly weather observations.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param con Active DBI SQLite connection.
#' @return A tibble of hourly demand and weather metrics.
build_hourly_demand <- function(trips_clean, con) {
  message("Building hourly citywide demand dataset...")

  start_bound <- as.POSIXct("2024-09-01 00:00:00", tz = "America/New_York")
  month_end   <- as.POSIXct("2024-10-01 00:00:00", tz = "America/New_York")

  # Full sequence of 720 hours in September 2024
  all_hours <- seq(
    from = start_bound,
    to   = as.POSIXct("2024-09-30 23:00:00", tz = "America/New_York"),
    by   = "1 hour"
  )

  hourly_trips <- trips_clean |>
    dplyr::filter(started_at >= start_bound, started_at < month_end) |>
    dplyr::mutate(hour_start = lubridate::floor_date(started_at, "1 hour")) |>
    dplyr::group_by(hour_start) |>
    dplyr::summarize(
      trips        = dplyr::n(),
      member_trips = sum(member_casual == "member", na.rm = TRUE),
      casual_trips = sum(member_casual == "casual", na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      hour_start = all_hours,
      fill = list(trips = 0L, member_trips = 0L, casual_trips = 0L)
    )

  # Fetch and join hourly weather
  weather_hourly <- DBI::dbGetQuery(con, "SELECT * FROM weather") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      hour_start = lubridate::as_datetime(time, tz = "America/New_York")
    ) |>
    dplyr::select(hour_start, temperature_2m, precipitation, wind_speed_10m, relative_humidity_2m)

  hourly_demand <- hourly_trips |>
    dplyr::left_join(weather_hourly, by = "hour_start")

  message(sprintf("Constructed hourly demand table (%d hourly observations).", nrow(hourly_demand)))
  return(hourly_demand)
}

#' Runner function for data cleaning and feature engineering
#'
#' Orchestrates reading raw trips from SQLite, running the sequential cleaning
#' pipeline, auditing missingness, computing station flow metrics via SQL,
#' calculating spatial proximity with sf, building the 2-hour panel, creating
#' the hourly demand table, and persisting RDS and database outputs.
#'
#' @param db_path Path to SQLite database.
#' @return Invisibly returns a summary list of artifacts.
run_clean_features <- function(db_path = "data/citibike.sqlite") {
  message("=== Citi Bike Rebalancing: Clean Features & Build Panel ===\n")

  # Step 1: Connect to SQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Step 2: Read raw trips from SQLite
  message("Loading raw trips from SQLite...")
  raw_trips <- DBI::dbGetQuery(con, "SELECT * FROM trips") |>
    dplyr::as_tibble()
  message(sprintf("Loaded %s raw trips.", format(nrow(raw_trips), big.mark = ",")))

  # Step 3: Raw missingness audit
  audit_raw_missingness(raw_trips, "results/tables/missingness_raw.csv")

  # Step 4: Clean trips sequentially
  trips_clean <- clean_trips_sequential(raw_trips, "results/tables/cleaning_audit.csv")
  rm(raw_trips)
  invisible(gc(verbose = FALSE))

  # Step 5: Write trips_clean table to SQLite for SQL querying
  message("Writing temporary 'trips_clean' table to SQLite for SQL flow aggregation...")
  trips_clean_db <- trips_clean |>
    dplyr::mutate(
      started_at = as.character(started_at),
      ended_at   = as.character(ended_at),
      day_of_week = as.character(day_of_week)
    )
  DBI::dbWriteTable(con, "trips_clean", as.data.frame(trips_clean_db), overwrite = TRUE)
  rm(trips_clean_db)
  invisible(gc(verbose = FALSE))

  # Step 6: Compute station flow metrics via SQL GROUP BY (DBI)
  station_flows <- compute_station_flows_sql(con)

  # Step 7: Join station capacity from GBFS stations table
  station_features <- join_station_capacity(station_flows, con)

  # Step 8: Spatial subway proximity metrics using sf
  station_features <- compute_subway_proximity_sf(station_features, con)

  # Step 9: Build station x 2-hour panel
  panel <- build_station_panel(trips_clean, station_features, con)

  # Step 10: Build citywide hourly demand
  hourly_demand <- build_hourly_demand(trips_clean, con)

  # Step 11: Save cached RDS files in data/processed/
  message("\nSaving processed cache files to 'data/processed/'...")
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

  saveRDS(trips_clean, "data/processed/trips_clean.rds")
  message("Saved: data/processed/trips_clean.rds")

  saveRDS(panel, "data/processed/panel.rds")
  message("Saved: data/processed/panel.rds")

  # Step 12: Save station_features to CSV
  readr::write_csv(station_features, "results/tables/station_features.csv")
  message("Saved: results/tables/station_features.csv")

  # Step 13: Write persistent tables to SQLite database
  message("Writing updated tables to SQLite database...")

  DBI::dbWriteTable(con, "station_features", as.data.frame(station_features), overwrite = TRUE)

  panel_db <- panel |>
    dplyr::mutate(
      window_start = as.character(window_start),
      day_of_week  = as.character(day_of_week)
    )
  DBI::dbWriteTable(con, "panel", as.data.frame(panel_db), overwrite = TRUE)
  rm(panel_db)

  hourly_demand_db <- hourly_demand |>
    dplyr::mutate(hour_start = as.character(hour_start))
  DBI::dbWriteTable(con, "hourly_demand", as.data.frame(hourly_demand_db), overwrite = TRUE)
  rm(hourly_demand_db)

  message("\n=== Data Cleaning and Feature Pipeline Completed Successfully ===")
  message(sprintf("- Cleaned trips: %s rows", format(nrow(trips_clean), big.mark = ",")))
  message(sprintf("- Stations with features: %d rows", nrow(station_features)))
  message(sprintf("- Balanced panel observations: %s rows", format(nrow(panel), big.mark = ",")))
  message(sprintf("- Hourly city demand: %d rows", nrow(hourly_demand)))

  invisible(list(
    trips_clean      = trips_clean,
    station_features = station_features,
    panel            = panel,
    hourly_demand    = hourly_demand
  ))
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_clean_features()
