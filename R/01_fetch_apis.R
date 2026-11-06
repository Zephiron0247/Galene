# ==============================================================================
# R/01_fetch_apis.R
# Project: citibike-rebalancing
# Purpose: Fetch external datasets via APIs and HTTP downloads:
#          1. Citi Bike GBFS station information
#          2. Open-Meteo historical weather data (September 2024)
#          3. MTA Subway Entrances and Exits dataset
#          4. Sample September 2024 Citi Bike trip data
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Ensure setup has loaded required packages
source("R/00_setup.R")

# Standard HTTP user agent for external requests
HTTP_USER_AGENT <- "citibike-rebalancing-r/1.0 (Windows; R 4.6.1)"

#' Fetch Citi Bike station information from GBFS API
#'
#' Sends an HTTP GET request to the official Citi Bike GBFS station_information
#' endpoint, parses the returned JSON, filters and formats station attributes,
#' and saves the resulting tibble to CSV.
#'
#' @param url Character URL of the GBFS station information endpoint.
#' @param dest_file Destination path for the saved CSV file.
#' @return A tibble with columns: short_name, name, lat, lon, capacity.
fetch_station_info <- function(url = "https://gbfs.citibikenyc.com/gbfs/en/station_information.json",
                               dest_file = "data/raw/station_information.csv") {
  message("Fetching station information from GBFS API...")

  # Step 1: Ensure target directory exists
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)

  # Step 2: Make HTTP GET request with retry and timeout
  response <- httr::RETRY(
    verb = "GET",
    url  = url,
    config = httr::timeout(60),
    httr::user_agent(HTTP_USER_AGENT),
    times = 3,
    pause_base = 2
  )
  httr::stop_for_status(response, task = "fetch GBFS station information")

  # Step 3: Extract and parse JSON text
  content_text <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(content_text, simplifyDataFrame = TRUE)

  # Step 4: Extract the stations data frame
  stations_df <- parsed_data$data$stations

  # Step 5: Select and cast required fields into a clean tibble
  station_tibble <- dplyr::as_tibble(data.frame(
    short_name = as.character(stations_df$short_name),
    name       = as.character(stations_df$name),
    lat        = as.numeric(stations_df$lat),
    lon        = as.numeric(stations_df$lon),
    capacity   = as.integer(stations_df$capacity),
    stringsAsFactors = FALSE
  ))

  # Step 6: Save station data to CSV
  readr::write_csv(station_tibble, dest_file)
  message(sprintf("Saved %d stations to '%s'.", nrow(station_tibble), dest_file))

  return(station_tibble)
}

#' Fetch historical weather observations from Open-Meteo Archive API
#'
#' Queries the Open-Meteo archive API for NYC coordinates for September 2024,
#' retrieving hourly temperature, precipitation, wind speed, and relative humidity.
#' Parses the returned JSON into a tidy tibble and writes it to CSV.
#'
#' @param base_url Base URL of Open-Meteo archive API.
#' @param lat Latitude coordinate (NYC: 40.7128).
#' @param lon Longitude coordinate (NYC: -74.0060).
#' @param start_date Start date in YYYY-MM-DD format (default: 2024-09-01).
#' @param end_date End date in YYYY-MM-DD format (default: 2024-09-30).
#' @param hourly_vars Character vector of hourly weather variables to request.
#' @param timezone Timezone string (default: "America/New_York").
#' @param dest_file Destination path for the saved CSV file.
#' @return A tibble with hourly weather observations.
fetch_weather <- function(base_url = "https://archive-api.open-meteo.com/v1/archive",
                          lat = 40.7128,
                          lon = -74.006,
                          start_date = "2024-09-01",
                          end_date = "2024-09-30",
                          hourly_vars = c("temperature_2m", "precipitation",
                                          "wind_speed_10m", "relative_humidity_2m"),
                          timezone = "America/New_York",
                          dest_file = "data/raw/weather_sept2024.csv") {
  message("Fetching September 2024 weather from Open-Meteo API...")

  # Step 1: Ensure destination directory exists
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)

  # Step 2: Build API query parameters
  query_params <- list(
    latitude   = lat,
    longitude  = lon,
    start_date = start_date,
    end_date   = end_date,
    hourly     = paste(hourly_vars, collapse = ","),
    timezone   = timezone
  )

  # Step 3: Perform HTTP GET request with retry and timeout
  response <- httr::RETRY(
    verb = "GET",
    url  = base_url,
    query = query_params,
    config = httr::timeout(60),
    httr::user_agent(HTTP_USER_AGENT),
    times = 3,
    pause_base = 2
  )
  httr::stop_for_status(response, task = "fetch Open-Meteo weather data")

  # Step 4: Parse JSON response
  content_text <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(content_text)

  # Step 5: Convert hourly block to tibble
  weather_tibble <- dplyr::as_tibble(parsed_data$hourly)

  # Step 6: Write to CSV
  readr::write_csv(weather_tibble, dest_file)
  message(sprintf("Saved %d hourly weather records to '%s'.",
                  nrow(weather_tibble), dest_file))

  return(weather_tibble)
}

#' Download MTA Subway Entrances and Exits dataset
#'
#' Downloads the New York State Open Data subway entrances CSV directly
#' into the raw data directory using HTTP streaming with retries.
#'
#' @param url Direct download URL for MTA Subway Entrances and Exits dataset.
#' @param dest_file Destination file path for saved CSV.
#' @return Character path of the downloaded file.
download_subway_entrances <- function(url = "https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD",
                                      dest_file = "data/raw/subway_entrances.csv") {
  message("Downloading MTA subway entrances dataset...")

  # Step 1: Ensure raw directory exists
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)

  # Step 2: Download file using httr::RETRY to stream directly to disk with timeout
  response <- httr::RETRY(
    verb = "GET",
    url  = url,
    httr::write_disk(dest_file, overwrite = TRUE),
    config = httr::timeout(60),
    httr::user_agent(HTTP_USER_AGENT),
    times = 3,
    pause_base = 2
  )
  httr::stop_for_status(response, task = "download subway entrances CSV")

  # Step 3: Verify downloaded file size
  file_size_kb <- round(file.info(dest_file)$size / 1024, 1)
  message(sprintf("Downloaded subway entrances dataset (%s KB) to '%s'.",
                  file_size_kb, dest_file))

  return(dest_file)
}

#' Fetch sample Citi Bike trip data (September 2024)
#'
#' Downloads and unzips a representative September 2024 Citi Bike trip dataset
#' into `data/raw/trips/` if no trip CSVs are currently present. This guarantees
#' that downstream database building and queries have operational data to process.
#'
#' @param url Download URL for trip zip archive (default: September 2024 Jersey City data).
#' @param dest_dir Destination directory for extracted trip CSVs.
#' @return Character vector of trip CSV file paths.
fetch_sample_trips <- function(url = "https://s3.amazonaws.com/tripdata/JC-202409-citibike-tripdata.csv.zip",
                               dest_dir = "data/raw/trips") {
  # Step 1: Check if trip files already exist
  existing_csvs <- list.files(dest_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(existing_csvs) > 0) {
    message(sprintf("Found %d existing trip CSV(s) in '%s'. Skipping sample download.",
                    length(existing_csvs), dest_dir))
    return(existing_csvs)
  }

  message("Downloading sample Citi Bike trips dataset for September 2024...")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Step 2: Download zip archive to a temporary file
  temp_zip <- tempfile(fileext = ".zip")
  utils::download.file(url, temp_zip, mode = "wb", quiet = TRUE)

  # Step 3: Extract CSV files into destination directory (excluding macOS artifacts)
  zip_contents <- utils::unzip(temp_zip, list = TRUE)$Name
  csv_to_extract <- zip_contents[grepl("\\.csv$", zip_contents) & !grepl("__MACOSX", zip_contents)]

  utils::unzip(temp_zip, files = csv_to_extract, exdir = dest_dir)
  unlink(temp_zip)

  extracted_files <- list.files(dest_dir, pattern = "\\.csv$", full.names = TRUE)
  message(sprintf("Extracted %d trip file(s) into '%s'.",
                  length(extracted_files), dest_dir))

  return(extracted_files)
}

#' Runner function to execute all API data fetchers
#'
#' Coordinates fetching station information, weather records, subway entrances,
#' and sample trip files, logging comprehensive progress information.
#'
#' @return Invisibly returns a list of results.
run_fetch_apis <- function() {
  message("=== Citi Bike Rebalancing: Fetch External APIs & Datasets ===\n")

  # 1. Station information
  stations <- fetch_station_info()

  # 2. Historical weather
  weather <- fetch_weather()

  # 3. Subway entrances
  subway_path <- download_subway_entrances()

  # 4. Sample trip data
  trip_paths <- fetch_sample_trips()

  message("\n=== API Fetching Completed Successfully ===")
  message(sprintf("- Stations: %d records in data/raw/station_information.csv", nrow(stations)))
  message(sprintf("- Weather:  %d hourly rows in data/raw/weather_sept2024.csv", nrow(weather)))
  message(sprintf("- Subway:   %s", subway_path))
  message(sprintf("- Trips:    %d CSV file(s) in data/raw/trips/", length(trip_paths)))

  invisible(list(
    stations = stations,
    weather  = weather,
    subway   = subway_path,
    trips    = trip_paths
  ))
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_fetch_apis()
