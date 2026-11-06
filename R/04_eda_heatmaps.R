# ==============================================================================
# R/04_eda_heatmaps.R
# Project: citibike-rebalancing
# Purpose: Exploratory Data Analysis, interactive Leaflet heatmaps, ggplot2 visuals,
#          interactive Plotly weather demand charts, and summary statistics table.
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Step 0: Load environment setup and shared utilities
source("R/00_setup.R")
source("R/utils.R")

#' Helper to save self-contained HTML widget and clean up temporary dependency folder
save_widget_selfcontained <- function(widget, file_path) {
  htmlwidgets::saveWidget(widget, file = file_path, selfcontained = TRUE)
  lib_dir <- paste0(tools::file_path_sans_ext(file_path), "_files")
  if (dir.exists(lib_dir)) {
    unlink(lib_dir, recursive = TRUE)
  }
}

#' Create unified interactive Leaflet heatmap of station activity and flow intensity
#'
#' Builds a self-contained Leaflet map on CartoDB.Positron tiles with layer controls
#' featuring 4 distinct heatmaps via leaflet.extras::addHeatmap:
#' 1. Departures (all stations)
#' 2. Arrivals (all stations)
#' 3. Drainer Intensity (stations with negative net flow, weighted by absolute net flow)
#' 4. Collector Intensity (stations with positive net flow, weighted by net flow)
#'
#' @param station_features Tibble of station metrics.
#' @param out_html Destination HTML file path.
#' @return The generated leaflet map object.
create_station_heatmaps <- function(station_features,
                                    out_html = "results/maps/station_heatmaps.html") {
  message("Generating multi-layer interactive Leaflet heatmaps...")
  dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)

  # Filter stations with valid coordinates
  stns <- station_features |>
    dplyr::filter(!is.na(lat), !is.na(lon))

  # Subsets for drainers (net_flow < 0) and collectors (net_flow > 0)
  drainers <- stns |>
    dplyr::filter(net_flow < 0) |>
    dplyr::mutate(weight = abs(net_flow))

  collectors <- stns |>
    dplyr::filter(net_flow > 0) |>
    dplyr::mutate(weight = net_flow)

  # Build single Leaflet map with 4 heatmap layers
  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = -73.98, lat = 40.75, zoom = 12) |>
    # Layer 1: Departures
    leaflet.extras::addHeatmap(
      data      = stns,
      lng       = ~lon,
      lat       = ~lat,
      intensity = ~departures,
      blur      = 20,
      max       = max(stns$departures, na.rm = TRUE),
      radius    = 15,
      group     = "Departures"
    ) |>
    # Layer 2: Arrivals
    leaflet.extras::addHeatmap(
      data      = stns,
      lng       = ~lon,
      lat       = ~lat,
      intensity = ~arrivals,
      blur      = 20,
      max       = max(stns$arrivals, na.rm = TRUE),
      radius    = 15,
      group     = "Arrivals"
    ) |>
    # Layer 3: Drainer Intensity (net_flow < 0)
    leaflet.extras::addHeatmap(
      data      = drainers,
      lng       = ~lon,
      lat       = ~lat,
      intensity = ~weight,
      blur      = 22,
      max       = max(drainers$weight, na.rm = TRUE),
      radius    = 16,
      gradient  = c("blue", "yellow", "red"),
      group     = "Drainer Intensity"
    ) |>
    # Layer 4: Collector Intensity (net_flow > 0)
    leaflet.extras::addHeatmap(
      data      = collectors,
      lng       = ~lon,
      lat       = ~lat,
      intensity = ~weight,
      blur      = 22,
      max       = max(collectors$weight, na.rm = TRUE),
      radius    = 16,
      gradient  = c("white", "cyan", "blue"),
      group     = "Collector Intensity"
    ) |>
    # Layer controls
    leaflet::addLayersControl(
      overlayGroups = c("Departures", "Arrivals", "Drainer Intensity", "Collector Intensity"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  save_widget_selfcontained(m, file_path = out_html)
  message(sprintf("Saved multi-layer station heatmaps to '%s'.", out_html))
  return(m)
}

#' Create interactive Leaflet asymmetry choropleth map
#'
#' Renders station circle markers colored by asymmetry index (diverging scale:
#' red for drainers, blue for collectors) with rich informative popups.
#'
#' @param station_features Tibble of station metrics.
#' @param out_html Destination HTML file path.
#' @return The generated leaflet map object.
create_station_asymmetry_map <- function(station_features,
                                         out_html = "results/maps/station_asymmetry.html") {
  message("Generating interactive station asymmetry Leaflet map...")
  dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)

  stns <- station_features |>
    dplyr::filter(!is.na(lat), !is.na(lon))

  # Diverging color palette: Red (drainer, -1) -> Grey/White (0) -> Blue (collector, +1)
  pal_asymm <- leaflet::colorNumeric(
    palette = c("#D73027", "#F7F7F7", "#2166AC"),
    domain  = c(-1, 1),
    na.color = "#999999"
  )

  popup_text <- sprintf(
    "<b>%s</b> (ID: %s)<br/>
     <b>Capacity:</b> %s<br/>
     <b>Departures:</b> %s | <b>Arrivals:</b> %s<br/>
     <b>Net Flow:</b> %s (Asymmetry: %s)<br/>
     <b>Subway Entrances (400m):</b> %s (Nearest: %s m)",
    stns$station_name,
    stns$station_id,
    ifelse(is.na(stns$capacity), "N/A", as.character(stns$capacity)),
    format(stns$departures, big.mark = ","),
    format(stns$arrivals, big.mark = ","),
    format(stns$net_flow, big.mark = ","),
    round(stns$asymmetry_index, 3),
    ifelse(is.na(stns$subway_400m), "N/A", as.character(stns$subway_400m)),
    ifelse(is.na(stns$dist_nearest_subway_m), "N/A", as.character(stns$dist_nearest_subway_m))
  )

  m <- leaflet::leaflet(stns) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = -73.98, lat = 40.75, zoom = 12) |>
    leaflet::addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      radius      = 5,
      stroke      = TRUE,
      weight      = 0.6,
      fillOpacity = 0.85,
      color       = ~pal_asymm(asymmetry_index),
      popup       = popup_text
    ) |>
    leaflet::addLegend(
      pal      = pal_asymm,
      values   = ~asymmetry_index,
      title    = "Asymmetry Index<br/>(Red = Drainer, Blue = Collector)",
      position = "bottomright",
      opacity  = 0.9
    )

  save_widget_selfcontained(m, file_path = out_html)
  message(sprintf("Saved station asymmetry map to '%s'.", out_html))
  return(m)
}

#' Plot tile heatmap: top 25 stations x 24 hours
#'
#' Visualizes departure volume bottlenecks for the 25 busiest departure stations
#' across each hour of the day.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param out_png Destination PNG file path.
#' @return ggplot object.
plot_top25_heatmap <- function(trips_clean,
                               out_png = "results/figures/bottleneck_heatmap_top25.png") {
  message("Plotting top 25 stations x 24 hours bottleneck heatmap...")
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  # Identify top 25 stations by total departures
  top_stns <- trips_clean |>
    dplyr::count(start_station_name, sort = TRUE) |>
    dplyr::slice_head(n = 25)

  # Aggregate departures by hour for top 25 stations
  heat_data <- trips_clean |>
    dplyr::semi_join(top_stns, by = "start_station_name") |>
    dplyr::count(start_station_name, start_hour, name = "departures") |>
    tidyr::complete(
      start_station_name = top_stns$start_station_name,
      start_hour = 0:23,
      fill = list(departures = 0L)
    )

  # Reverse factor levels so highest-volume station appears at top of y-axis
  heat_data <- heat_data |>
    dplyr::mutate(
      start_station_name = factor(start_station_name, levels = rev(top_stns$start_station_name))
    )

  p <- ggplot2::ggplot(heat_data, ggplot2::aes(x = start_hour, y = start_station_name, fill = departures)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "magma", labels = scales::comma) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 2)) +
    ggplot2::labs(
      title    = "Hourly Departure Bottleneck Matrix (Top 25 NYC Stations)",
      subtitle = "September 2024 trip departures aggregated across 24 diurnal hours",
      x        = "Hour of Day (EDT)",
      y        = NULL,
      fill     = "Departures"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = 13),
      axis.text.y  = ggplot2::element_text(size = 8),
      panel.grid   = ggplot2::element_blank()
    )

  ggplot2::ggsave(out_png, p, width = 11, height = 8, dpi = 200)
  message(sprintf("Saved top 25 heatmap to '%s'.", out_png))
  return(p)
}

#' Plot tile heatmap: day of week x hour (citywide)
#'
#' Depicts aggregate citywide Citi Bike demand across every day of the week
#' and hour of the day.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param out_png Destination PNG file path.
#' @return ggplot object.
plot_dow_hour_heatmap <- function(trips_clean,
                                  out_png = "results/figures/heatmap_dow_hour.png") {
  message("Plotting day of week x hour citywide demand heatmap...")
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  dow_hour_data <- trips_clean |>
    dplyr::count(day_of_week, start_hour, name = "trip_count") |>
    tidyr::complete(
      day_of_week = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
      start_hour = 0:23,
      fill = list(trip_count = 0L)
    ) |>
    dplyr::mutate(
      day_of_week = factor(day_of_week, levels = rev(c(
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
      )))
    )

  p <- ggplot2::ggplot(dow_hour_data, ggplot2::aes(x = start_hour, y = day_of_week, fill = trip_count)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(option = "viridis", labels = scales::comma) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 2)) +
    ggplot2::labs(
      title    = "Citywide Citi Bike Trips: Day of Week vs. Hour of Day",
      subtitle = "September 2024 total trip volume (diurnal commuter vs. weekend leisure curves)",
      x        = "Hour of Day (EDT)",
      y        = NULL,
      fill     = "Trips"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      panel.grid = ggplot2::element_blank()
    )

  ggplot2::ggsave(out_png, p, width = 10, height = 6, dpi = 200)
  message(sprintf("Saved day-of-week x hour heatmap to '%s'.", out_png))
  return(p)
}

#' Plot ridge density: departure hour (weekday vs. weekend)
#'
#' Draws a 300,000-row random sample to render smooth departure hour densities
#' contrasting bimodal weekday commuting peaks against unimodal weekend leisure peaks.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param out_png Destination PNG file path.
#' @param sample_size Subsample size (default: 300,000).
#' @param seed Random seed for reproducibility.
#' @return ggplot object.
plot_departure_ridges <- function(trips_clean,
                                  out_png = "results/figures/hourly_departure_ridges.png",
                                  sample_size = 300000L,
                                  seed = 42L) {
  message(sprintf("Plotting departure ridges using %s sampled trips...",
                  format(sample_size, big.mark = ",")))
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  set.seed(seed)
  n_draw <- min(sample_size, nrow(trips_clean))
  sample_df <- trips_clean |>
    dplyr::slice_sample(n = n_draw) |>
    dplyr::mutate(
      day_type = factor(dplyr::if_else(is_weekend, "Weekend", "Weekday"),
                         levels = c("Weekday", "Weekend"))
    )

  p <- ggplot2::ggplot(sample_df, ggplot2::aes(x = start_hour, y = day_type, fill = day_type)) +
    ggridges::geom_density_ridges(
      alpha          = 0.75,
      scale          = 1.15,
      rel_min_height = 0.01,
      color          = "white",
      linewidth      = 0.4
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    ggplot2::scale_fill_manual(values = c("Weekday" = "#2B5C8F", "Weekend" = "#D95F02")) +
    ggplot2::labs(
      title    = "Hourly Departure Profiles: Weekday vs. Weekend",
      subtitle = sprintf("Subsample of n = %s trips (Seed %d) | Commuter peaks at 08:00 & 17:00-18:00",
                         format(n_draw, big.mark = ","), seed),
      x        = "Hour of Day (departure)",
      y        = NULL,
      fill     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )

  ggplot2::ggsave(out_png, p, width = 10, height = 5, dpi = 200)
  message(sprintf("Saved departure ridges plot to '%s'.", out_png))
  return(p)
}

#' Plot boxplot: trip duration by member vs. casual (log scale)
#'
#' Compares duration distributions between annual members and casual riders
#' using the 300,000-row sample on a logarithmic scale.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param out_png Destination PNG file path.
#' @param sample_size Subsample size.
#' @param seed Random seed.
#' @return ggplot object.
plot_duration_member_casual_boxplot <- function(trips_clean,
                                                out_png = "results/figures/duration_member_casual_boxplot.png",
                                                sample_size = 300000L,
                                                seed = 42L) {
  message("Plotting trip duration boxplot by user membership type (log scale)...")
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  set.seed(seed)
  n_draw <- min(sample_size, nrow(trips_clean))
  sample_df <- trips_clean |>
    dplyr::slice_sample(n = n_draw)

  p <- ggplot2::ggplot(sample_df, ggplot2::aes(x = member_casual, y = trip_duration_mins, fill = member_casual)) +
    ggplot2::geom_boxplot(
      outlier.alpha = 0.08,
      outlier.size  = 0.6,
      width         = 0.45,
      color         = "#222222"
    ) +
    ggplot2::scale_y_log10(
      breaks = c(1, 2, 5, 10, 15, 30, 60, 120),
      labels = c("1m", "2m", "5m", "10m", "15m", "30m", "1h", "2h")
    ) +
    ggplot2::scale_fill_manual(values = c("member" = "#1B9E77", "casual" = "#7570B3")) +
    ggplot2::labs(
      title    = "Trip Duration Distribution: Annual Members vs. Casual Users",
      subtitle = sprintf("Subsample of n = %s trips (log10 scale) | Casual riders exhibit higher median duration",
                         format(n_draw, big.mark = ",")),
      x        = "User Membership Type",
      y        = "Trip Duration (log scale)",
      fill     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )

  ggplot2::ggsave(out_png, p, width = 8, height = 6, dpi = 200)
  message(sprintf("Saved duration boxplot to '%s'.", out_png))
  return(p)
}

#' Plot distance vs. duration scatter with GAM smooth
#'
#' Draws 30,000 sampled trips to render distance vs. duration relationship
#' overlaid with a cubic spline generalized additive model (`gam`, `bs = "cs"`).
#'
#' @param trips_clean Cleaned trips tibble.
#' @param out_png Destination PNG file path.
#' @param sample_size Subsample size (30,000).
#' @param seed Random seed (42).
#' @return ggplot object.
plot_distance_duration_gam <- function(trips_clean,
                                       out_png = "results/figures/distance_duration_gam.png",
                                       sample_size = 30000L,
                                       seed = 42L) {
  message("Plotting distance vs. duration scatter with GAM smoothing...")
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  set.seed(seed)
  n_draw <- min(sample_size, nrow(trips_clean))
  sample_df <- trips_clean |>
    dplyr::slice_sample(n = n_draw)

  p <- ggplot2::ggplot(sample_df, ggplot2::aes(x = distance_km, y = trip_duration_mins)) +
    ggplot2::geom_point(alpha = 0.12, size = 0.6, color = "#2C3E50") +
    ggplot2::geom_smooth(
      method    = "gam",
      formula   = y ~ s(x, bs = "cs"),
      color     = "#E74C3C",
      linewidth = 1.1,
      se        = TRUE
    ) +
    ggplot2::labs(
      title    = "Trip Distance vs. Duration Relationship",
      subtitle = sprintf("n = %s subsampled trips with Cubic Spline GAM smooth",
                         format(n_draw, big.mark = ",")),
      x        = "Haversine Distance (km)",
      y        = "Trip Duration (minutes)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_png, p, width = 9, height = 6, dpi = 200)
  message(sprintf("Saved distance-duration GAM plot to '%s'.", out_png))
  return(p)
}

#' Plot interactive hourly demand and weather chart via Plotly
#'
#' Builds a dual-axis interactive time-series chart displaying hourly trip volume
#' alongside temperature and precipitation.
#'
#' @param hourly_demand Tibble of hourly city demand and weather.
#' @param out_html Destination HTML file path.
#' @return Plotly widget object.
plot_hourly_demand_weather <- function(hourly_demand,
                                       out_html = "results/maps/hourly_demand_weather.html") {
  message("Generating interactive Plotly hourly demand & weather visualization...")
  dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)

  # Prepare date-time
  demand_df <- hourly_demand |>
    dplyr::mutate(
      time_stamp = as.POSIXct(hour_start)
    )

  # Construct Plotly object
  fig <- plotly::plot_ly(data = demand_df) |>
    # Trace 1: Precipitation bars on secondary y-axis
    plotly::add_bars(
      x      = ~time_stamp,
      y      = ~precipitation,
      name   = "Precipitation (mm)",
      yaxis  = "y2",
      marker = list(color = "rgba(70, 130, 180, 0.4)")
    ) |>
    # Trace 2: Citywide Trips line
    plotly::add_lines(
      x      = ~time_stamp,
      y      = ~trips,
      name   = "Total Trips",
      line   = list(color = "#1F77B4", width = 2)
    ) |>
    # Trace 3: Member Trips line
    plotly::add_lines(
      x      = ~time_stamp,
      y      = ~member_trips,
      name   = "Member Trips",
      line   = list(color = "#2CA02C", width = 1.5, dash = "dot")
    ) |>
    # Trace 4: Temperature line on secondary y-axis
    plotly::add_lines(
      x      = ~time_stamp,
      y      = ~temperature_2m,
      name   = "Temperature (°C)",
      yaxis  = "y2",
      line   = list(color = "#FF7F0E", width = 1.5, dash = "dash")
    ) |>
    plotly::layout(
      title  = "NYC Citi Bike Hourly Trip Demand vs. Weather (September 2024)",
      xaxis  = list(title = "Date & Time (EDT)"),
      yaxis  = list(title = "Hourly Trip Count"),
      yaxis2 = list(
        title      = "Temperature (°C) / Precipitation (mm)",
        overlaying = "y",
        side       = "right",
        showgrid   = FALSE
      ),
      legend = list(orientation = "h", x = 0.1, y = 1.1),
      hovermode = "x unified"
    )

  save_widget_selfcontained(fig, file_path = out_html)
  message(sprintf("Saved hourly demand Plotly chart to '%s'.", out_html))
  return(fig)
}

#' Generate comprehensive EDA statistical summary table
#'
#' Computes distribution metrics (n, mean, median, sd, min, max, skewness) for
#' trip duration, distance, speed, and hourly trips.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param hourly_demand Hourly demand tibble.
#' @param out_csv Destination CSV file path.
#' @return Summary metrics tibble.
create_eda_summary_table <- function(trips_clean,
                                     hourly_demand,
                                     out_csv = "results/tables/eda_summary.csv") {
  message("Computing exploratory data analysis summary statistics...")
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)

  calc_metric_stats <- function(var_name, values) {
    v <- values[!is.na(values) & is.finite(values)]
    dplyr::tibble(
      metric   = var_name,
      n        = length(v),
      mean     = round(mean(v), 2),
      median   = round(stats::median(v), 2),
      sd       = round(stats::sd(v), 2),
      min      = round(min(v), 2),
      max      = round(max(v), 2),
      skewness = round(calc_skewness(v), 2)
    )
  }

  summary_rows <- list(
    calc_metric_stats("duration_mins", trips_clean$trip_duration_mins),
    calc_metric_stats("distance_km", trips_clean$distance_km),
    calc_metric_stats("speed_kmh", trips_clean$trip_speed_kmh),
    calc_metric_stats("hourly_trips", hourly_demand$trips)
  )

  summary_tbl <- dplyr::bind_rows(summary_rows)
  readr::write_csv(summary_tbl, out_csv)
  message(sprintf("Saved EDA statistical summary table to '%s'.", out_csv))
  print(summary_tbl)
  return(summary_tbl)
}

#' Runner function for all EDA and visual generations
#'
#' Coordinates loading data from processed RDS and SQLite database, generating
#' interactive HTML widgets, rendering high-resolution figures, and saving the
#' statistical summary table.
#'
#' @return Invisibly returns list of outputs.
run_eda_heatmaps <- function() {
  message("=== Citi Bike Rebalancing: Generate EDA Visuals & Heatmaps ===\n")

  # Step 1: Load pre-cleaned trips from cache RDS
  trips_rds <- "data/processed/trips_clean.rds"
  if (!file.exists(trips_rds)) {
    stop(sprintf("Cleaned trips RDS '%s' not found. Run R/03_clean_features.R first.", trips_rds))
  }
  message("Loading cleaned trips from RDS cache...")
  trips_clean <- readRDS(trips_rds)

  # Step 2: Load station features and hourly demand from SQLite
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/citibike.sqlite")
  station_features <- DBI::dbGetQuery(con, "SELECT * FROM station_features") |>
    dplyr::as_tibble()
  hourly_demand <- DBI::dbGetQuery(con, "SELECT * FROM hourly_demand") |>
    dplyr::as_tibble()
  DBI::dbDisconnect(con)

  # Step 3: Interactive Leaflet Heatmaps
  create_station_heatmaps(station_features, "results/maps/station_heatmaps.html")

  # Step 4: Interactive Station Asymmetry Map
  create_station_asymmetry_map(station_features, "results/maps/station_asymmetry.html")

  # Step 5: Top 25 Station x 24h Bottleneck Heatmap
  plot_top25_heatmap(trips_clean, "results/figures/bottleneck_heatmap_top25.png")

  # Step 6: Day of Week x Hour Citywide Heatmap
  plot_dow_hour_heatmap(trips_clean, "results/figures/heatmap_dow_hour.png")

  # Step 7: Hourly Departure Ridges (300k sample)
  plot_departure_ridges(trips_clean, "results/figures/hourly_departure_ridges.png", sample_size = 300000L, seed = 42L)

  # Step 8: Boxplot by Member vs Casual (300k sample, log scale)
  plot_duration_member_casual_boxplot(trips_clean, "results/figures/duration_member_casual_boxplot.png", sample_size = 300000L, seed = 42L)

  # Step 9: Distance vs Duration GAM Scatter (30k sample)
  plot_distance_duration_gam(trips_clean, "results/figures/distance_duration_gam.png", sample_size = 30000L, seed = 42L)

  # Step 10: Interactive Plotly Hourly Demand vs Weather
  plot_hourly_demand_weather(hourly_demand, "results/maps/hourly_demand_weather.html")

  # Step 11: Statistical Summary Table
  summary_tbl <- create_eda_summary_table(trips_clean, hourly_demand, "results/tables/eda_summary.csv")

  message("\n=== All EDA Visuals and Artifacts Generated Successfully ===")
  invisible(list(summary_table = summary_tbl))
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_eda_heatmaps()
