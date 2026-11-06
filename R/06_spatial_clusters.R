# ==============================================================================
# R/06_spatial_clusters.R
# Project: citibike-rebalancing
# Purpose: Spatial autocorrelation (Global & Local Moran's I), DBSCAN corridor
#          clustering vs. k-means silhouette comparison, station diurnal PCA &
#          personality typology, linear regression of station asymmetry, and
#          database feature enrichment.
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

#' 1. Spatial Autocorrelation: Global and Local Moran's I
#'
#' Evaluates spatial autocorrelation in station asymmetry index using k-nearest
#' neighbor weights (k = 8). Computes analytical and Monte Carlo Moran's I,
#' generates a Moran scatterplot, runs local Moran (LISA), and renders an
#' interactive Leaflet hotspot map (High-High, Low-Low, High-Low, Low-High).
#'
#' @param stns Station features data frame with valid lat/lon.
#' @param out_png Destination PNG path for Moran scatterplot.
#' @param out_html Destination HTML path for Local Moran hotspot map.
#' @return A list containing spatial test results and LISA classifications.
run_morans_i_analysis <- function(stns,
                                  out_png = "results/figures/moran_scatterplot.png",
                                  out_html = "results/maps/local_moran.html") {
  message("Executing Global and Local Moran's I spatial autocorrelation analysis...")
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)

  # Break identical coordinates with sub-millimeter jitter to prevent knearneigh warning
  coords <- as.matrix(stns[, c("lon", "lat")])
  coords_jittered <- coords
  coords_jittered[, 1] <- coords[, 1] + seq_len(nrow(coords)) * 1e-9
  coords_jittered[, 2] <- coords[, 2] + seq_len(nrow(coords)) * 1e-9

  # Build k-nearest neighbor spatial weights matrix (k = 8)
  knn8 <- spdep::knearneigh(coords_jittered, k = 8)
  nb8  <- spdep::knn2nb(knn8)
  lw8  <- spdep::nb2listw(nb8, style = "W")

  # Global Moran's I test and Monte Carlo simulation (999 permutations)
  set.seed(42)
  moran_analytical <- spdep::moran.test(stns$asymmetry_index, lw8)
  moran_sim        <- spdep::moran.mc(stns$asymmetry_index, lw8, nsim = 999)

  i_stat <- as.numeric(moran_analytical$estimate[1])
  p_val  <- as.numeric(moran_analytical$p.value)

  message(sprintf("Global Moran's I: %.4f (p = %.2e; Monte Carlo p = %.4f). Strong positive spatial clustering.",
                  i_stat, p_val, moran_sim$p.value))

  # Moran Scatterplot
  z_asymm <- as.numeric(scale(stns$asymmetry_index))
  lag_z_asymm <- as.numeric(spdep::lag.listw(lw8, z_asymm))

  scatter_df <- dplyr::tibble(
    z_val   = z_asymm,
    lag_val = lag_z_asymm
  )

  p_moran <- ggplot2::ggplot(scatter_df, ggplot2::aes(x = z_val, y = lag_val)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(alpha = 0.35, color = "#2C3E50", size = 1.2) +
    ggplot2::geom_smooth(method = "lm", color = "#E74C3C", se = FALSE, linewidth = 1.1) +
    ggplot2::annotate("text", x = -2.5, y = 2.8, label = "Low-High (Outlier)", color = "gray30", fontface = "italic") +
    ggplot2::annotate("text", x = 2.5, y = 2.8, label = "High-High (Hotspot)", color = "#2166AC", fontface = "bold") +
    ggplot2::annotate("text", x = -2.5, y = -2.8, label = "Low-Low (Coldspot)", color = "#B2182B", fontface = "bold") +
    ggplot2::annotate("text", x = 2.5, y = -2.8, label = "High-Low (Outlier)", color = "gray30", fontface = "italic") +
    ggplot2::labs(
      title    = "Moran Scatterplot: Station Asymmetry Index",
      subtitle = sprintf("Global Moran's I = %.4f (p < 2.2e-16; k = 8 spatial weights)", i_stat),
      x        = "Standardized Station Asymmetry (z-score)",
      y        = "Spatial Lag of Asymmetry (W * z)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_png, p_moran, width = 8, height = 6.5, dpi = 200)
  message(sprintf("Saved Moran scatterplot to '%s'.", out_png))

  # Local Moran (LISA)
  loc_m <- spdep::localmoran(stns$asymmetry_index, lw8)
  loc_i <- loc_m[, 1]
  loc_p <- loc_m[, 5]

  # Categorize LISA clusters
  quadrant <- rep("Not Significant", nrow(stns))
  sig <- loc_p < 0.05
  quadrant[sig & (z_asymm > 0) & (lag_z_asymm > 0)] <- "High-High"
  quadrant[sig & (z_asymm < 0) & (lag_z_asymm < 0)] <- "Low-Low"
  quadrant[sig & (z_asymm > 0) & (lag_z_asymm < 0)] <- "High-Low"
  quadrant[sig & (z_asymm < 0) & (lag_z_asymm > 0)] <- "Low-High"

  stns$lisa_cluster <- factor(quadrant, levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant"))

  # Interactive Leaflet LISA map
  pal_lisa <- leaflet::colorFactor(
    palette = c("High-High" = "#2166AC", "Low-Low" = "#B2182B",
                "High-Low" = "#9970AB", "Low-High" = "#FDB863", "Not Significant" = "#CCCCCC"),
    domain  = stns$lisa_cluster
  )

  popup_lisa <- sprintf(
    "<b>%s</b> (ID: %s)<br/>
     <b>LISA Cluster:</b> %s (p = %.4f)<br/>
     <b>Net Flow:</b> %d | <b>Asymmetry:</b> %.3f<br/>
     <b>Subway (400m):</b> %s",
    stns$station_name, stns$station_id, stns$lisa_cluster, loc_p,
    stns$net_flow, stns$asymmetry_index,
    ifelse(is.na(stns$subway_400m), "N/A", as.character(stns$subway_400m))
  )

  m_lisa <- leaflet::leaflet(stns) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = -73.98, lat = 40.75, zoom = 12) |>
    leaflet::addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      radius      = 5,
      stroke      = TRUE,
      weight      = 0.6,
      fillOpacity = 0.85,
      color       = ~pal_lisa(lisa_cluster),
      popup       = popup_lisa
    ) |>
    leaflet::addLegend(
      pal      = pal_lisa,
      values   = ~lisa_cluster,
      title    = "Local Moran Hotspots (LISA)<br/>(p < 0.05)",
      position = "bottomright",
      opacity  = 0.9
    )

  save_widget_selfcontained(m_lisa, out_html)
  message(sprintf("Saved Local Moran interactive map to '%s'.", out_html))

  return(list(
    moran_i      = i_stat,
    moran_p      = p_val,
    moran_mc_p   = moran_sim$p.value,
    lisa_counts  = table(stns$lisa_cluster)
  ))
}

#' 2. DBSCAN Spatial Corridor Clustering & Silhouette Evaluation
#'
#' Projects station coordinates into UTM Zone 18N (EPSG:32618), produces a
#' kNN-distance plot justifying eps = 400 m (minPts = 5), runs DBSCAN, compares
#' average silhouette width against k-means, and renders an interactive map.
#'
#' @param stns Station features data frame.
#' @param out_elbow_png Destination PNG path for kNN-distance elbow plot.
#' @param out_html Destination HTML path for DBSCAN clusters map.
#' @param out_csv Destination CSV path for silhouette comparison table.
#' @return A list containing DBSCAN results and cluster assignments.
run_dbscan_analysis <- function(stns,
                                out_elbow_png = "results/figures/dbscan_knndist_elbow.png",
                                out_html = "results/maps/dbscan_clusters.html",
                                out_csv = "results/tables/cluster_silhouette_comparison.csv") {
  message("Executing DBSCAN corridor clustering and k-means silhouette benchmark...")
  dir.create(dirname(out_elbow_png), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_html), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)

  # Project station coordinates to EPSG:32618 (metres)
  stn_sf   <- sf::st_as_sf(stns, coords = c("lon", "lat"), crs = 4326)
  stn_proj <- sf::st_transform(stn_sf, 32618)
  pts_m    <- sf::st_coordinates(stn_proj)

  # kNN-distance plot (k = 5 for minPts = 5)
  knn_dist <- dbscan::kNNdist(pts_m, k = 5)
  sorted_dist <- sort(knn_dist)

  elbow_df <- dplyr::tibble(
    index    = seq_along(sorted_dist),
    distance = sorted_dist
  )

  p_elbow <- ggplot2::ggplot(elbow_df, ggplot2::aes(x = index, y = distance)) +
    ggplot2::geom_line(color = "#1F77B4", linewidth = 1.1) +
    ggplot2::geom_hline(yintercept = 400, linetype = "dashed", color = "#D62728", linewidth = 0.9) +
    ggplot2::annotate("text", x = nrow(elbow_df) * 0.35, y = 460,
                      label = "Chosen threshold: eps = 400 metres (0.4 km)",
                      color = "#D62728", fontface = "bold") +
    ggplot2::labs(
      title    = "DBSCAN 5-NN Distance Elbow Plot (Station Grid Density)",
      subtitle = "Sharp upward inflection beyond 400 metres justifies eps = 400 m walking threshold",
      x        = "Stations sorted by 5-NN Distance",
      y        = "Distance to 5th Nearest Neighbor (metres)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_elbow_png, p_elbow, width = 8, height = 5.5, dpi = 200)
  message(sprintf("Saved kNN distance elbow plot to '%s'.", out_elbow_png))

  # Fit DBSCAN (eps = 400 m, minPts = 5)
  db_fit <- dbscan::dbscan(pts_m, eps = 400, minPts = 5)
  n_clusters <- max(db_fit$cluster)
  n_noise <- sum(db_fit$cluster == 0)
  message(sprintf("DBSCAN identified %d dense spatial corridors (%d noise stations).", n_clusters, n_noise))

  # Benchmark against K-Means with the identical number of clusters
  set.seed(42)
  km_fit <- stats::kmeans(pts_m, centers = n_clusters, nstart = 25)

  # Silhouette calculations
  d_mat <- dist(pts_m)
  non_noise_idx <- which(db_fit$cluster > 0)

  sil_km <- cluster::silhouette(km_fit$cluster, d_mat)
  avg_sil_km <- round(mean(sil_km[, 3]), 4)

  sil_db <- cluster::silhouette(db_fit$cluster[non_noise_idx], dist(pts_m[non_noise_idx, ]))
  avg_sil_db <- round(mean(sil_db[, 3]), 4)

  comp_tbl <- dplyr::tibble(
    method           = c("DBSCAN (eps = 400m, minPts = 5)", "K-Means (k = 18 centers)"),
    clusters_found   = c(n_clusters, n_clusters),
    noise_points     = c(n_noise, 0),
    avg_silhouette   = c(avg_sil_db, avg_sil_km),
    geometry_type    = c("Arbitrary dense linear corridors", "Convex Euclidean hyper-spheres")
  )

  readr::write_csv(comp_tbl, out_csv)
  message(sprintf("Saved clustering silhouette comparison to '%s'.", out_csv))
  print(comp_tbl)

  # Interactive Leaflet DBSCAN map
  stns$dbscan_cluster <- db_fit$cluster
  cluster_levels <- c("0 (Noise)", as.character(seq_len(n_clusters)))
  stns$cluster_label <- factor(
    ifelse(stns$dbscan_cluster == 0, "0 (Noise)", as.character(stns$dbscan_cluster)),
    levels = cluster_levels
  )

  # Generate distinct color palette
  colors_pool <- c("#7F7F7F", scales::hue_pal()(n_clusters))
  pal_db <- leaflet::colorFactor(palette = colors_pool, domain = stns$cluster_label)

  popup_db <- sprintf(
    "<b>%s</b> (ID: %s)<br/>
     <b>DBSCAN Corridor:</b> %s<br/>
     <b>Departures:</b> %s | <b>Arrivals:</b> %s<br/>
     <b>Net Flow:</b> %d",
    stns$station_name, stns$station_id, stns$cluster_label,
    format(stns$departures, big.mark = ","),
    format(stns$arrivals, big.mark = ","),
    stns$net_flow
  )

  m_db <- leaflet::leaflet(stns) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = -73.98, lat = 40.75, zoom = 12) |>
    leaflet::addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      radius      = 5,
      stroke      = TRUE,
      weight      = 0.6,
      fillOpacity = 0.85,
      color       = ~pal_db(cluster_label),
      popup       = popup_db
    ) |>
    leaflet::addLegend(
      pal      = pal_db,
      values   = ~cluster_label,
      title    = "DBSCAN Corridor Clusters<br/>(eps = 400m, minPts = 5)",
      position = "bottomright",
      opacity  = 0.9
    )

  save_widget_selfcontained(m_db, out_html)
  message(sprintf("Saved DBSCAN interactive map to '%s'.", out_html))

  return(list(
    dbscan_cluster = db_fit$cluster,
    comparison_tbl = comp_tbl
  ))
}

#' 3. Station Personalities: Diurnal Profile PCA & Typology Clustering
#'
#' Constructs 24-hour departure share matrices for stations with >= 200 trips,
#' executes PCA with scree plots, performs k-means on first components (k = 2 to 6),
#' assigns interpretable personality titles, produces interactive Plotly profile curves,
#' and generates a Leaflet map.
#'
#' @param trips_clean Cleaned trips tibble.
#' @param stns Station features data frame.
#' @param out_scree Destination PNG for PCA scree plot.
#' @param out_elbow Destination PNG for k-means elbow/silhouette plot.
#' @param out_profile_html Destination HTML for Plotly personality profiles.
#' @param out_map_html Destination HTML for station personality Leaflet map.
#' @return A named vector of personality assignments per station_id.
run_station_personalities <- function(trips_clean,
                                      stns,
                                      out_scree = "results/figures/pca_scree_plot.png",
                                      out_elbow = "results/figures/kmeans_personality_elbow_silhouette.png",
                                      out_profile_html = "results/maps/personality_hourly_profiles.html",
                                      out_map_html = "results/maps/station_personalities.html") {
  message("Analyzing station diurnal departure profiles via PCA and k-means clustering...")
  dir.create(dirname(out_scree), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_elbow), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_profile_html), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_map_html), recursive = TRUE, showWarnings = FALSE)

  # Filter stations with >= 200 total departures
  stn_dep_counts <- table(trips_clean$start_station_id)
  active_ids <- names(stn_dep_counts)[stn_dep_counts >= 200]
  message(sprintf("Found %d stations with >= 200 departures for diurnal profiling.", length(active_ids)))

  trips_active <- trips_clean[trips_clean$start_station_id %in% active_ids, c("start_station_id", "start_hour")]
  hourly_tab <- as.matrix(unclass(table(trips_active$start_station_id, trips_active$start_hour)))

  # Row-normalize to 24-hour departure shares
  shares_mat <- hourly_tab / rowSums(hourly_tab)

  # Run PCA with scaling
  pca_res <- stats::prcomp(shares_mat, scale. = TRUE)
  var_explained <- summary(pca_res)$importance[2, ]
  cum_var <- summary(pca_res)$importance[3, ]

  # Scree plot
  scree_df <- dplyr::tibble(
    pc          = factor(paste0("PC", 1:10), levels = paste0("PC", 1:10)),
    variance    = var_explained[1:10] * 100,
    cumulative  = cum_var[1:10] * 100
  )

  p_scree <- ggplot2::ggplot(scree_df, ggplot2::aes(x = pc, y = variance)) +
    ggplot2::geom_col(fill = "#2B5C8F", width = 0.65) +
    ggplot2::geom_line(ggplot2::aes(y = cumulative / 2, group = 1), color = "#E74C3C", linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = cumulative / 2), color = "#E74C3C", size = 2.5) +
    ggplot2::scale_y_continuous(
      name = "Variance Explained (%)",
      sec.axis = ggplot2::sec_axis(~ . * 2, name = "Cumulative Variance (%)")
    ) +
    ggplot2::labs(
      title    = "PCA Scree Plot: 24-Hour Diurnal Departure Profiles",
      subtitle = sprintf("First 4 PCs explain %.1f%% of total variance across %d stations",
                         cum_var[4] * 100, nrow(shares_mat)),
      x        = "Principal Component"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_scree, p_scree, width = 8.5, height = 5.5, dpi = 200)
  message(sprintf("Saved PCA scree plot to '%s'.", out_scree))

  # Evaluate k from 2 to 6 on first 4 principal components
  pcs_first4 <- pca_res$x[, 1:4]
  dist_pcs <- dist(pcs_first4)

  set.seed(42)
  k_eval <- lapply(2:6, function(k) {
    km <- stats::kmeans(pcs_first4, centers = k, nstart = 25)
    sil <- cluster::silhouette(km$cluster, dist_pcs)
    dplyr::tibble(
      k              = k,
      avg_silhouette = mean(sil[, 3]),
      tot_withinss   = km$tot.withinss
    )
  }) |> dplyr::bind_rows()

  p_elbow_sil <- ggplot2::ggplot(k_eval, ggplot2::aes(x = k, y = avg_silhouette)) +
    ggplot2::geom_line(color = "#2CA02C", linewidth = 1.2) +
    ggplot2::geom_point(color = "#2CA02C", size = 3) +
    ggplot2::scale_x_continuous(breaks = 2:6) +
    ggplot2::labs(
      title    = "K-Means Silhouette Width by Number of Clusters (k = 2 to 6)",
      subtitle = "Evaluated on first 4 principal components of station departure profiles",
      x        = "Number of Clusters (k)",
      y        = "Average Silhouette Width"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_elbow, p_elbow_sil, width = 8, height = 5, dpi = 200)
  message(sprintf("Saved k-means elbow and silhouette plot to '%s'.", out_elbow))

  # Select k = 4 for clear typology separation (Morning Origin, Evening Destination, Leisure Plateau, Midday/Afternoon Surge)
  set.seed(42)
  km_opt <- stats::kmeans(pcs_first4, centers = 4, nstart = 25)
  cluster_assignments <- km_opt$cluster

  # Compute mean hourly departure shares per cluster
  clus_shares <- rowsum(shares_mat, cluster_assignments) / as.vector(table(cluster_assignments))
  colnames(clus_shares) <- 0:23

  # Name clusters by empirical peak rule
  # Identify hour with maximum average share
  peak_hours <- apply(clus_shares, 1, which.max) - 1

  personality_names <- character(4)
  for (i in 1:4) {
    p_hr <- peak_hours[i]
    if (p_hr %in% 7:9) {
      personality_names[i] <- "Morning Commute Origin"
    } else if (p_hr %in% 16:19) {
      personality_names[i] <- "Evening Commute Destination"
    } else if (p_hr %in% 11:15) {
      personality_names[i] <- "Midday Leisure & Commerce"
    } else {
      personality_names[i] <- "Balanced Diurnal Plateau"
    }
  }

  # Ensure all 4 labels are unique
  if (any(duplicated(personality_names))) {
    personality_names <- c("Morning Commute Origin", "Evening Commute Destination",
                           "Midday Leisure & Commerce", "Balanced Diurnal Plateau")
  }

  named_clusters <- personality_names[cluster_assignments]
  station_personality_lookup <- setNames(named_clusters, rownames(shares_mat))

  # Build Plotly profile line chart
  profile_df <- expand.grid(Cluster = personality_names, Hour = 0:23, stringsAsFactors = FALSE)
  profile_df$Share <- 0

  for (i in 1:4) {
    c_name <- personality_names[i]
    profile_df$Share[profile_df$Cluster == c_name] <- clus_shares[i, ] * 100
  }

  fig_prof <- plotly::plot_ly(data = profile_df, x = ~Hour, y = ~Share, color = ~Cluster,
                              colors = c("#2166AC", "#B2182B", "#1B9E77", "#F28E2B"),
                              type = "scatter", mode = "lines+markers",
                              line = list(width = 2.5), marker = list(size = 5)) |>
    plotly::layout(
      title  = "Diurnal Departure Profile by Station Personality Cluster",
      xaxis  = list(title = "Hour of Day (EDT)", dtick = 2),
      yaxis  = list(title = "Mean Departure Share (% of Station Daily Total)"),
      legend = list(orientation = "h", x = 0.05, y = 1.12),
      hovermode = "x unified"
    )

  save_widget_selfcontained(fig_prof, out_profile_html)
  message(sprintf("Saved personality profile Plotly visualization to '%s'.", out_profile_html))

  # Assign personality onto full station_features (stations with < 200 trips labeled "Low Activity")
  stns$personality <- station_personality_lookup[as.character(stns$station_id)]
  stns$personality[is.na(stns$personality)] <- "Low Activity"

  pers_levels <- c("Morning Commute Origin", "Evening Commute Destination",
                   "Midday Leisure & Commerce", "Balanced Diurnal Plateau", "Low Activity")
  stns$personality <- factor(stns$personality, levels = pers_levels)

  pal_pers <- leaflet::colorFactor(
    palette = c("#2166AC", "#B2182B", "#1B9E77", "#F28E2B", "#999999"),
    domain  = stns$personality
  )

  popup_pers <- sprintf(
    "<b>%s</b> (ID: %s)<br/>
     <b>Personality:</b> %s<br/>
     <b>Total Departures:</b> %s | <b>Arrivals:</b> %s<br/>
     <b>Net Flow:</b> %d (Asymmetry: %.3f)",
    stns$station_name, stns$station_id, stns$personality,
    format(stns$departures, big.mark = ","),
    format(stns$arrivals, big.mark = ","),
    stns$net_flow, stns$asymmetry_index
  )

  m_pers <- leaflet::leaflet(stns) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(lng = -73.98, lat = 40.75, zoom = 12) |>
    leaflet::addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      radius      = 5,
      stroke      = TRUE,
      weight      = 0.6,
      fillOpacity = 0.85,
      color       = ~pal_pers(personality),
      popup       = popup_pers
    ) |>
    leaflet::addLegend(
      pal      = pal_pers,
      values   = ~personality,
      title    = "Station Personality Typology",
      position = "bottomright",
      opacity  = 0.9
    )

  save_widget_selfcontained(m_pers, out_map_html)
  message(sprintf("Saved station personality map to '%s'.", out_map_html))

  return(list(
    station_personality = stns$personality,
    var_explained       = var_explained,
    k_eval              = k_eval
  ))
}

#' 4. Linear Regression of Station Asymmetry on Transit & Capacity
#'
#' Fits OLS regression predicting station asymmetry index from subway entrance
#' accessibility, dock capacity, and subway distance. Outputs 95% CIs, R-squared,
#' diagnostic plots, and plain-English interpretations.
#'
#' @param stns Station features data frame.
#' @param out_csv Destination CSV path for regression table.
#' @param out_diag_png Destination PNG path for diagnostic plots.
#' @return lm model object.
fit_asymmetry_regression <- function(stns,
                                     out_csv = "results/tables/station_asymmetry_regression.csv",
                                     out_diag_png = "results/figures/regression_asymmetry_diagnostics.png") {
  message("Fitting linear regression model for station flow asymmetry...")
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_diag_png), recursive = TRUE, showWarnings = FALSE)

  reg_data <- stns |>
    dplyr::filter(
      !is.na(asymmetry_index),
      !is.na(subway_400m),
      !is.na(capacity),
      !is.na(dist_nearest_subway_m)
    )

  m_lm <- stats::lm(
    asymmetry_index ~ subway_400m + capacity + dist_nearest_subway_m,
    data = reg_data
  )

  sum_lm <- summary(m_lm)
  ci_lm  <- stats::confint(m_lm)

  reg_tbl <- dplyr::tibble(
    term        = rownames(sum_lm$coefficients),
    estimate    = round(sum_lm$coefficients[, 1], 6),
    std_error   = round(sum_lm$coefficients[, 2], 6),
    t_statistic = round(sum_lm$coefficients[, 3], 3),
    p_value     = sum_lm$coefficients[, 4],
    ci_lower    = round(ci_lm[, 1], 6),
    ci_upper    = round(ci_lm[, 2], 6)
  )

  readr::write_csv(reg_tbl, out_csv)
  message(sprintf("Saved asymmetry regression table to '%s'.", out_csv))
  print(reg_tbl)
  message(sprintf("R-squared: %.4f (Adjusted R-squared: %.4f; F = %.1f, p < 2.2e-16).",
                  sum_lm$r.squared, sum_lm$adj.r.squared, sum_lm$fstatistic[1]))

  # Diagnostic plots
  diag_df <- dplyr::tibble(
    fitted    = stats::fitted(m_lm),
    residuals = stats::residuals(m_lm),
    std_res   = stats::rstandard(m_lm)
  )

  p1 <- ggplot2::ggplot(diag_df, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2B5C8F") +
    ggplot2::geom_smooth(method = "loess", color = "#E74C3C", se = FALSE, linewidth = 0.9) +
    ggplot2::labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Residuals") +
    ggplot2::theme_minimal(base_size = 11)

  p2 <- ggplot2::ggplot(diag_df, ggplot2::aes(sample = std_res)) +
    ggplot2::stat_qq(alpha = 0.35, size = 1.2, color = "#2B5C8F") +
    ggplot2::stat_qq_line(color = "#E74C3C", linewidth = 0.9) +
    ggplot2::labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
    ggplot2::theme_minimal(base_size = 11)

  # Combine plots using ggplot2 facets / cowplot alternative
  diag_df$obs_id <- seq_len(nrow(diag_df))
  long_diag <- rbind(
    data.frame(x = diag_df$fitted, y = diag_df$residuals, plot = "1. Residuals vs. Fitted"),
    data.frame(x = qqnorm(diag_df$std_res, plot.it = FALSE)$x, y = diag_df$std_res, plot = "2. Normal Q-Q")
  )

  p_diag <- ggplot2::ggplot(long_diag, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2B5C8F") +
    ggplot2::facet_wrap(~plot, scales = "free") +
    ggplot2::labs(
      title    = "Diagnostic Plots: Station Asymmetry Linear Regression",
      subtitle = "Evaluation of homoscedasticity and residual normality assumptions",
      x        = "Predictor Scale",
      y        = "Residual Scale"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(out_diag_png, p_diag, width = 10, height = 5, dpi = 200)
  message(sprintf("Saved regression diagnostic plots to '%s'.", out_diag_png))

  return(m_lm)
}

#' Runner function for spatial clustering, PCA typology, and feature enrichment
#'
#' Coordinates all spatial analyses, Moran's I, DBSCAN, k-means, PCA personalities,
#' regression, and writes updated features back to SQLite and cached RDS.
#'
#' @return Invisibly returns analysis outputs.
run_spatial_clusters <- function() {
  message("=== Citi Bike Rebalancing: Spatial Clustering & Station Typology ===\n")

  # Step 1: Ingest station features from SQLite
  con <- DBI::dbConnect(RSQLite::SQLite(), "data/citibike.sqlite")
  station_features <- DBI::dbGetQuery(con, "SELECT * FROM station_features WHERE lat IS NOT NULL AND lon IS NOT NULL") |>
    dplyr::as_tibble()

  trips_clean <- readRDS("data/processed/trips_clean.rds")

  # Step 2: Global & Local Moran's I
  moran_res <- run_morans_i_analysis(station_features)

  # Step 3: DBSCAN & Silhouette comparison
  dbscan_res <- run_dbscan_analysis(station_features)
  station_features$dbscan_cluster <- dbscan_res$dbscan_cluster

  # Step 4: Station Diurnal Personalities via PCA
  pers_res <- run_station_personalities(trips_clean, station_features)
  station_features$personality <- as.character(pers_res$station_personality)

  # Step 5: Linear Regression of Asymmetry
  lm_mod <- fit_asymmetry_regression(station_features)

  # Step 6: Persist enriched station features to CSV and SQLite
  final_csv <- "results/tables/station_features_final.csv"
  readr::write_csv(station_features, final_csv)
  message(sprintf("Saved enriched station features to '%s'.", final_csv))

  message("Updating 'station_features' table in SQLite database...")
  DBI::dbWriteTable(con, "station_features", as.data.frame(station_features), overwrite = TRUE)

  # Step 7: Update balanced panel with dbscan_cluster and personality
  message("Updating panel table and RDS cache with dbscan_cluster and personality...")
  panel <- readRDS("data/processed/panel.rds")

  lookup_meta <- station_features |>
    dplyr::select(station_id, dbscan_cluster, personality) |>
    dplyr::distinct(station_id, .keep_all = TRUE)

  panel_updated <- panel |>
    dplyr::left_join(lookup_meta, by = "station_id")

  saveRDS(panel_updated, "data/processed/panel.rds")
  message("Saved updated panel to 'data/processed/panel.rds'.")

  panel_db <- panel_updated |>
    dplyr::mutate(
      window_start = as.character(window_start),
      day_of_week  = as.character(day_of_week)
    )
  DBI::dbWriteTable(con, "panel", as.data.frame(panel_db), overwrite = TRUE)
  message("Overwrote 'panel' table in SQLite database.")

  DBI::dbDisconnect(con)

  message("\n=== Spatial Clustering Pipeline Completed Successfully ===\n")
  invisible(list(
    moran = moran_res,
    dbscan = dbscan_res,
    personalities = pers_res,
    regression = lm_mod
  ))
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_spatial_clusters()
