# run_app.R
# Project: citibike-rebalancing
# Purpose: Start the interactive Shiny dashboard on port 4040 and launch browser.

cat("====================================================================\n")
cat("Starting Citi Bike Fleet Optimization Core Shiny App...\n")
cat("Dashboard URL: http://127.0.0.1:4040\n")
cat("Press Ctrl+C in console to stop the server.\n")
cat("====================================================================\n")

shiny::runApp(
  appDir = "app",
  port = 4040,
  host = "127.0.0.1",
  launch.browser = TRUE
)
