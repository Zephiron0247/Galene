# ==============================================================================
# R/02_build_database.R
# Project: citibike-rebalancing
# Purpose: Build the central SQLite database (data/citibike.sqlite):
#          1. Load trip CSVs from data/raw/trips with lapply + data.table::fread
#          2. Load stations, weather, and subway data
#          3. Write tables: trips, stations, weather, subway into SQLite
#          4. Execute and print 3 validation SQL queries using DBI::dbGetQuery
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Ensure setup has loaded required packages
source("R/00_setup.R")

# Explicit colClasses definition for Citi Bike trip data files
TRIP_COL_CLASSES <- c(
  ride_id            = "character",
  rideable_type      = "character",
  started_at         = "character",
  ended_at           = "character",
  start_station_name = "character",
  start_station_id   = "character",
  end_station_name   = "character",
  end_station_id     = "character",
  start_lat          = "numeric",
  start_lng          = "numeric",
  end_lat            = "numeric",
  end_lng            = "numeric",
  member_casual      = "character"
)

#' Read all Citi Bike trip CSV files from raw data directory
#'
#' Scans the trip files directory, reads each CSV with `data.table::fread`
#' using explicit `colClasses` inside `lapply`, and binds the results into
#' a unified tibble.
#'
#' @param trips_dir Directory containing raw trip CSV files.
#' @param col_classes Named character vector specifying column classes.
#' @return A tibble containing combined trip records.
read_trips_data <- function(trips_dir = "data/raw/trips",
                            col_classes = TRIP_COL_CLASSES) {
  # Step 1: List all CSV files in directory
  csv_files <- list.files(trips_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    warning(sprintf("No trip CSV files found in '%s'. Creating empty trips dataset.", trips_dir))
    empty_df <- as.data.frame(
      lapply(col_classes, function(cls) vector(cls, 0)),
      stringsAsFactors = FALSE
    )
    return(dplyr::as_tibble(empty_df))
  }

  message(sprintf("Reading %d trip file(s) from '%s'...", length(csv_files), trips_dir))

  # Step 2: Use lapply + data.table::fread with explicit colClasses
  # Note: data.table is NOT attached; called via data.table::fread directly
  trips_list <- lapply(csv_files, function(file_path) {
    message(sprintf(" - Ingesting: %s", basename(file_path)))
    data.table::fread(
      file         = file_path,
      colClasses   = col_classes,
      data.table   = FALSE,
      showProgress = FALSE
    )
  })

  # Step 3: Combine all trip data frames into a single tibble
  combined_trips <- dplyr::as_tibble(dplyr::bind_rows(trips_list))
  message(sprintf("Successfully ingested %d total trip records.", nrow(combined_trips)))

  return(combined_trips)
}

#' Read station information CSV
#'
#' Loads the pre-fetched station information dataset from `data/raw/station_information.csv`.
#'
#' @param file_path Path to station information CSV.
#' @return A tibble with station metadata.
read_stations_data <- function(file_path = "data/raw/station_information.csv") {
  if (!file.exists(file_path)) {
    stop(sprintf("Station information file not found at '%s'. Run R/01_fetch_apis.R first.", file_path))
  }
  message(sprintf("Loading station information from '%s'...", file_path))
  stations_df <- readr::read_csv(file_path, show_col_types = FALSE)
  return(dplyr::as_tibble(stations_df))
}

#' Read weather observations CSV
#'
#' Loads the pre-fetched hourly weather dataset from `data/raw/weather_sept2024.csv`.
#'
#' @param file_path Path to weather CSV.
#' @return A tibble with hourly weather observations.
read_weather_data <- function(file_path = "data/raw/weather_sept2024.csv") {
  if (!file.exists(file_path)) {
    stop(sprintf("Weather data file not found at '%s'. Run R/01_fetch_apis.R first.", file_path))
  }
  message(sprintf("Loading weather data from '%s'...", file_path))
  weather_df <- readr::read_csv(file_path, show_col_types = FALSE)
  return(dplyr::as_tibble(weather_df))
}

#' Read subway entrances dataset CSV
#'
#' Loads the MTA subway entrances CSV and sanitizes column names for SQL compatibility.
#'
#' @param file_path Path to subway entrances CSV.
#' @return A tibble with sanitized subway entrance coordinates and metadata.
read_subway_data <- function(file_path = "data/raw/subway_entrances.csv") {
  if (!file.exists(file_path)) {
    stop(sprintf("Subway entrances file not found at '%s'. Run R/01_fetch_apis.R first.", file_path))
  }
  message(sprintf("Loading subway entrances from '%s'...", file_path))

  # Read via data.table::fread without attaching library
  subway_df <- data.table::fread(file_path, data.table = FALSE, showProgress = FALSE)

  # Sanitize column names for SQL friendly naming (lowercase, underscores)
  clean_names <- tolower(gsub("[^A-Za-z0-9_]+", "_", names(subway_df)))
  clean_names <- gsub("^_|_$", "", clean_names)
  names(subway_df) <- clean_names

  return(dplyr::as_tibble(subway_df))
}

#' Write data tables to SQLite database
#'
#' Connects to the SQLite database file and writes the tables:
#' trips, stations, weather, and subway. Overwrites existing tables.
#'
#' @param db_path File path for SQLite database.
#' @param trips_df Tibble/data frame of trips.
#' @param stations_df Tibble/data frame of stations.
#' @param weather_df Tibble/data frame of weather observations.
#' @param subway_df Tibble/data frame of subway entrances.
#' @return An active DBI database connection.
write_database_tables <- function(db_path = "data/citibike.sqlite",
                                  trips_df,
                                  stations_df,
                                  weather_df,
                                  subway_df) {
  # Step 1: Ensure output directory exists
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  message(sprintf("Connecting to SQLite database: '%s'...", db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)

  # Step 2: Write tables with overwrite = TRUE
  message("Writing table 'trips'...")
  DBI::dbWriteTable(con, name = "trips", value = as.data.frame(trips_df), overwrite = TRUE)

  message("Writing table 'stations'...")
  DBI::dbWriteTable(con, name = "stations", value = as.data.frame(stations_df), overwrite = TRUE)

  message("Writing table 'weather'...")
  DBI::dbWriteTable(con, name = "weather", value = as.data.frame(weather_df), overwrite = TRUE)

  message("Writing table 'subway'...")
  DBI::dbWriteTable(con, name = "subway", value = as.data.frame(subway_df), overwrite = TRUE)

  message("All tables successfully written to SQLite database.")
  return(con)
}

#' Run 3 example SQL queries and print formatted results
#'
#' Executes and prints three benchmark queries on the SQLite database:
#' 1. Total row count across tables (focused on total trips).
#' 2. Daily trip distribution (trips per day).
#' 3. Top 10 most popular start stations by trip volume.
#'
#' @param con Active DBI SQLite connection.
#' @return A named list of query result data frames.
run_example_queries <- function(con) {
  message("\n============================================================")
  message("             RUNNING EXAMPLE SQL QUERIES (DBI)              ")
  message("============================================================\n")

  # ----------------------------------------------------------------------------
  # Query 1: Row count
  # ----------------------------------------------------------------------------
  message("--- Query 1: Table Row Counts ---")
  q1_sql <- "
    SELECT
      'trips' AS table_name, COUNT(*) AS row_count FROM trips
    UNION ALL
    SELECT
      'stations' AS table_name, COUNT(*) AS row_count FROM stations
    UNION ALL
    SELECT
      'weather' AS table_name, COUNT(*) AS row_count FROM weather
    UNION ALL
    SELECT
      'subway' AS table_name, COUNT(*) AS row_count FROM subway;
  "
  res_q1 <- DBI::dbGetQuery(con, q1_sql)
  print(res_q1)

  # ----------------------------------------------------------------------------
  # Query 2: Trips per day
  # ----------------------------------------------------------------------------
  message("\n--- Query 2: Trips Per Day (September 2024) ---")
  q2_sql <- "
    SELECT
      substr(started_at, 1, 10) AS trip_date,
      COUNT(*)                  AS trip_count
    FROM trips
    WHERE started_at IS NOT NULL AND started_at != ''
    GROUP BY substr(started_at, 1, 10)
    ORDER BY trip_date ASC;
  "
  res_q2 <- DBI::dbGetQuery(con, q2_sql)
  print(utils::head(res_q2, 10))
  if (nrow(res_q2) > 10) {
    message(sprintf("... showing first 10 of %d days total.", nrow(res_q2)))
  }

  # ----------------------------------------------------------------------------
  # Query 3: Top 10 start stations
  # ----------------------------------------------------------------------------
  message("\n--- Query 3: Top 10 Start Stations ---")
  q3_sql <- "
    SELECT
      start_station_name,
      COUNT(*) AS trip_count
    FROM trips
    WHERE start_station_name IS NOT NULL AND start_station_name != ''
    GROUP BY start_station_name
    ORDER BY trip_count DESC
    LIMIT 10;
  "
  res_q3 <- DBI::dbGetQuery(con, q3_sql)
  print(res_q3)

  message("\n============================================================\n")

  return(list(
    row_counts        = res_q1,
    trips_per_day     = res_q2,
    top_start_stations = res_q3
  ))
}

#' Runner function to build database and verify queries
#'
#' Orchestrates reading all raw data files, populating the SQLite database,
#' executing validation queries, and closing the connection cleanly.
#'
#' @param db_path File path for SQLite database.
#' @return Invisibly returns the query results list.
run_build_database <- function(db_path = "data/citibike.sqlite") {
  message("=== Citi Bike Rebalancing: Build Database & Run Queries ===\n")

  # 1. Read raw inputs
  trips_df    <- read_trips_data("data/raw/trips")
  stations_df <- read_stations_data("data/raw/station_information.csv")
  weather_df  <- read_weather_data("data/raw/weather_sept2024.csv")
  subway_df   <- read_subway_data("data/raw/subway_entrances.csv")

  # 2. Connect and write to SQLite
  con <- write_database_tables(
    db_path     = db_path,
    trips_df    = trips_df,
    stations_df = stations_df,
    weather_df  = weather_df,
    subway_df   = subway_df
  )

  # 3. Run validation queries
  queries_output <- run_example_queries(con)

  # 4. Disconnect cleanly
  DBI::dbDisconnect(con)
  message(sprintf("Disconnected from '%s'. Database build complete.\n", db_path))

  invisible(queries_output)
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_build_database()
