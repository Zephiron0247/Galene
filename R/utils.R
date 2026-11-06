# ==============================================================================
# R/utils.R
# Project: citibike-rebalancing
# Purpose: Shared utility functions for distance calculation, outlier filtering,
#          and metrics computation.
# Reused from: D:/Projects/bike/01_data_pipeline.R (working haversine and IQR)
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

#' Vectorized Haversine great-circle distance (km)
#'
#' Pure R implementation — O(n) time, O(n) auxiliary memory for trig vectors.
#' No spatial package dependency; suitable for millions of point pairs on CPU.
#'
#' @param lat1 Numeric vector of origin latitudes in degrees WGS84.
#' @param lon1 Numeric vector of origin longitudes in degrees WGS84.
#' @param lat2 Numeric vector of destination latitudes in degrees WGS84.
#' @param lon2 Numeric vector of destination longitudes in degrees WGS84.
#' @param earth_radius_km Scalar Earth radius in kilometers (default: 6371 km).
#' @return Numeric vector of great-circle distances in kilometers.
haversine_km <- function(lat1, lon1, lat2, lon2, earth_radius_km = 6371) {
  # Step 1: Convert degrees to radians
  rad <- pi / 180
  phi1 <- lat1 * rad
  phi2 <- lat2 * rad
  d_phi <- (lat2 - lat1) * rad
  d_lambda <- (lon2 - lon1) * rad

  # Step 2: Spherical law of cosines / Haversine formulation
  a <- sin(d_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(d_lambda / 2)^2

  # Step 3: Guard against floating-point overshoot beyond 1 for asin domain
  c <- 2 * asin(pmin(1, sqrt(a)))

  # Step 4: Multiply by earth radius in kilometers
  return(earth_radius_km * c)
}

#' Remove trip duration outliers via the Tukey IQR rule (non-parametric)
#'
#' Computes Q1 and Q3 with stats::quantile (type 7, Hyndman-Fan). Rows with
#' duration below Q1 - 1.5*IQR or above Q3 + 1.5*IQR are flagged as FALSE.
#'
#' @param duration Numeric vector of trip lengths (e.g., minutes).
#' @return Logical vector (mask) with TRUE for inliers to retain.
iqr_inlier_mask <- function(duration) {
  # Step 1: Compute first and third quartiles (type 7 standard in R)
  q <- stats::quantile(duration, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)

  # Step 2: Compute interquartile range (IQR)
  iqr <- q[[2L]] - q[[1L]]

  # Step 3: Establish Tukey inner fences (1.5 * IQR)
  lower <- q[[1L]] - 1.5 * iqr
  upper <- q[[2L]] + 1.5 * iqr

  # Step 4: Generate logical mask for values within fences
  return(duration >= lower & duration <= upper)
}

#' Compute sample skewness of a numeric vector
#'
#' Uses the adjusted Fisher-Pearson standardized moment coefficient (consistent
#' with e1071 / moments / SAS / SPSS).
#'
#' @param x Numeric vector.
#' @param na.rm Logical; whether to remove NA values.
#' @return Scalar numeric skewness value.
calc_skewness <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(0)
  m3 <- mean((x - m)^3)
  return((m3 / (s^3)) * (sqrt(n * (n - 1)) / (n - 2)))
}
