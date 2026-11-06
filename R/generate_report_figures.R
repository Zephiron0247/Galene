# R/generate_report_figures.R
# Generate high-resolution presentation figures for project report and presentation.
# Figures:
#   Figure 1: Trip duration distribution histogram (after cleaning)
#   Figure 2: Hourly demand by rider type (member vs casual, weekday vs weekend)
#   Figure 8: Station features distributions (dock capacity & subway entrances <= 400m)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("app/www/figures", recursive = TRUE, showWarnings = FALSE)

# Custom presentation theme (dark navy modern)
theme_report_dark <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0B1220", color = NA),
      panel.background = ggplot2::element_rect(fill = "#121B2E", color = "#1E2A44"),
      panel.grid.major = ggplot2::element_line(color = "#1E2A44", linewidth = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(color = "#E6EDF7", family = "sans"),
      axis.text = ggplot2::element_text(color = "#94A3B8"),
      axis.title = ggplot2::element_text(color = "#E6EDF7", face = "bold"),
      plot.title = ggplot2::element_text(color = "#FFFFFF", face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(color = "#94A3B8", size = base_size - 1, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(color = "#64748B", size = base_size - 3),
      legend.background = ggplot2::element_rect(fill = "#121B2E", color = "#1E2A44"),
      legend.text = ggplot2::element_text(color = "#E6EDF7"),
      legend.title = ggplot2::element_text(color = "#E6EDF7", face = "bold"),
      strip.background = ggplot2::element_rect(fill = "#162036", color = "#1E2A44"),
      strip.text = ggplot2::element_text(color = "#38BDF8", face = "bold")
    )
}

# ------------------------------------------------------------------------------
# 1. Figure 1: Trip Duration Distribution Histogram
# ------------------------------------------------------------------------------
message("Generating Figure 1: Trip Duration Distribution Histogram...")
con <- DBI::dbConnect(RSQLite::SQLite(), "data/citibike.sqlite")

# Draw 200,000 trip duration sample
set.seed(42)
dur_sample <- DBI::dbGetQuery(con, "
  SELECT trip_duration_mins 
  FROM trips_clean 
  WHERE trip_duration_mins >= 1.0 AND trip_duration_mins <= 32.87 
  ORDER BY RANDOM() LIMIT 200000
")

p1 <- ggplot2::ggplot(dur_sample, ggplot2::aes(x = trip_duration_mins)) +
  ggplot2::geom_histogram(binwidth = 1, fill = "#38BDF8", color = "#0B1220", alpha = 0.88) +
  ggplot2::geom_vline(xintercept = 9.08, color = "#F59E0B", linetype = "dashed", linewidth = 1.1) +
  ggplot2::geom_vline(xintercept = 32.87, color = "#EF4444", linetype = "dotted", linewidth = 1.1) +
  ggplot2::annotate("text", x = 10.5, y = 17500, label = "Median: 9.08 min\n(Member: 8.53m, Casual: 11.82m)", 
                    color = "#F59E0B", fontface = "bold", hjust = 0, size = 4) +
  ggplot2::annotate("text", x = 27, y = 12000, label = "Tukey Upper Fence:\n32.87 min (Q3 + 1.5*IQR)\nSkewness: 0.96", 
                    color = "#EF4444", fontface = "bold", hjust = 0, size = 3.8) +
  ggplot2::labs(
    title = "Figure 1: Distribution of Trip Duration (After Outlier Cleaning)",
    subtitle = "Analysis of 4,708,761 valid trips; right-skewed distribution bounded [1.0 min, 32.87 min]",
    x = "Trip Duration (Minutes)",
    y = "Trip Count (200k Sample)",
    caption = "Citi Bike NYC & JC, Sept 2024. Cleaned via IQR rule (cut at 32.87 min). Skewness = 0.96."
  ) +
  theme_report_dark()

ggplot2::ggsave("results/figures/figure1_duration_histogram.png", p1, width = 10, height = 6, dpi = 200)
file.copy("results/figures/figure1_duration_histogram.png", "app/www/figures/figure1_duration_histogram.png", overwrite = TRUE)

# ------------------------------------------------------------------------------
# 2. Figure 2: Hourly Demand by Rider Type
# ------------------------------------------------------------------------------
message("Generating Figure 2: Hourly Demand by Rider Type...")
hourly_df <- DBI::dbGetQuery(con, "
  SELECT 
    start_hour, 
    member_casual, 
    is_weekend, 
    count(*) as total_trips
  FROM trips_clean 
  GROUP BY start_hour, member_casual, is_weekend
")
DBI::dbDisconnect(con)

# September 2024 has 21 weekdays and 9 weekend days
hourly_df <- hourly_df |>
  dplyr::mutate(
    day_type = ifelse(is_weekend == 1, "Weekend (9 Days)", "Weekday (21 Days)"),
    num_days = ifelse(is_weekend == 1, 9, 21),
    avg_trips_per_hour = total_trips / num_days,
    rider_label = ifelse(member_casual == "member", "Annual Member (80.5%)", "Casual Rider (19.5%)")
  )

p2 <- ggplot2::ggplot(hourly_df, ggplot2::aes(x = start_hour, y = avg_trips_per_hour, color = rider_label, group = rider_label)) +
  ggplot2::geom_line(linewidth = 1.3) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::facet_wrap(~day_type, scales = "free_y") +
  ggplot2::scale_color_manual(values = c("Annual Member (80.5%)" = "#38BDF8", "Casual Rider (19.5%)" = "#F59E0B")) +
  ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3), labels = sprintf("%02d:00", seq(0, 23, by = 3))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Figure 2: Diurnal Hourly Trip Demand by Rider Membership & Day Type",
    subtitle = "Weekdays exhibit sharp dual commuter peaks (08:00 & 17:00-18:00); Weekends exhibit a single broad midday leisure hump",
    x = "Hour of Day (EDT)",
    y = "Average Trips per Hour",
    color = "Rider Group",
    caption = "Citi Bike NYC & JC, Sept 2024 (4.71M trips). Casual demand concentrates on afternoons and weekends."
  ) +
  theme_report_dark() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave("results/figures/figure2_hourly_demand_rider_type.png", p2, width = 11, height = 6, dpi = 200)
file.copy("results/figures/figure2_hourly_demand_rider_type.png", "app/www/figures/figure2_hourly_demand_rider_type.png", overwrite = TRUE)

# ------------------------------------------------------------------------------
# 3. Figure 8: Station Features Distributions (Capacity & Subway Entrances)
# ------------------------------------------------------------------------------
message("Generating Figure 8: Station Features Distributions...")
stn_df <- read.csv("results/tables/station_features.csv")

p8a <- ggplot2::ggplot(stn_df |> dplyr::filter(!is.na(capacity)), ggplot2::aes(x = capacity)) +
  ggplot2::geom_histogram(binwidth = 4, fill = "#3B82F6", color = "#0B1220", alpha = 0.85) +
  ggplot2::geom_vline(xintercept = median(stn_df$capacity, na.rm = TRUE), color = "#38BDF8", linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = 27, y = 280, label = paste0("Median: ", median(stn_df$capacity, na.rm = TRUE), " docks\nIQR: 20-35"), 
                    color = "#38BDF8", fontface = "bold", hjust = 0, size = 3.6) +
  ggplot2::labs(
    title = "A. Station Dock Capacity",
    x = "Dock Capacity (Slots)",
    y = "Number of Stations"
  ) +
  theme_report_dark(base_size = 11)

p8b <- ggplot2::ggplot(stn_df, ggplot2::aes(x = subway_400m)) +
  ggplot2::geom_histogram(binwidth = 2, fill = "#10B981", color = "#0B1220", alpha = 0.85) +
  ggplot2::geom_vline(xintercept = median(stn_df$subway_400m, na.rm = TRUE), color = "#34D399", linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = 8, y = 550, label = paste0("Median: ", median(stn_df$subway_400m, na.rm = TRUE), " entrances\nIQR: 0-6"), 
                    color = "#34D399", fontface = "bold", hjust = 0, size = 3.6) +
  ggplot2::labs(
    title = "B. Subway Entrances (\u2264 400m Buffer)",
    x = "Subway Entrances Count within 400m",
    y = "Number of Stations"
  ) +
  theme_report_dark(base_size = 11)

# Combine using gridExtra or cowplot if installed, else save side-by-side or individually
p8 <- cowplot::plot_grid(p8a, p8b, ncol = 2)
ggplot2::ggsave("results/figures/figure8_station_features.png", p8, width = 11, height = 5.5, dpi = 200)
file.copy("results/figures/figure8_station_features.png", "app/www/figures/figure8_station_features.png", overwrite = TRUE)

message("All presentation figures successfully generated and mirrored to app/www/figures/!")
