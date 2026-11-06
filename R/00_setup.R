# ==============================================================================
# R/00_setup.R
# Project: citibike-rebalancing
# Purpose: Verify package installation, load required libraries (without
#          attaching data.table), and create required project directory structure.
# Environment: R 4.6.1 on Windows (CRAN binary packages only, no Rtools).
# ==============================================================================

# Ensure Pandoc path is discoverable for self-contained HTML widget generation
if (dir.exists("D:/R/pandoc-3.6.3")) {
  Sys.setenv(RSTUDIO_PANDOC = "D:/R/pandoc-3.6.3")
}

# List of required packages for the citibike-rebalancing project
REQUIRED_PACKAGES <- c(
  "dplyr",
  "tidyr",
  "readr",
  "lubridate",
  "ggplot2",
  "DBI",
  "RSQLite",
  "httr",
  "jsonlite",
  "data.table",
  "sf",
  "leaflet",
  "leaflet.extras",
  "ggridges",
  "plotly",
  "htmlwidgets",
  "scales"
)

# Packages that must NEVER be attached to search path via library()
UNATTACHED_PACKAGES <- c("data.table")

# Required project directories
PROJECT_DIRECTORIES <- c(
  "data",
  "data/raw",
  "data/raw/trips",
  "data/processed",
  "R",
  "docs",
  "results",
  "results/tables",
  "results/figures",
  "results/maps",
  "presentation"
)

#' Ensure required CRAN packages are installed as Windows binaries
#'
#' Checks whether the requested packages are installed on the local system.
#' If any package is missing, attempts installation using pre-compiled CRAN
#' Windows binaries (`type = "binary"`). If any binary is unavailable, the
#' function stops execution immediately per project requirements.
#'
#' @param packages Character vector of package names to check and install.
#' @param repos Character string specifying the CRAN mirror URL.
#' @return Logical named vector indicating whether each package is installed.
ensure_packages_installed <- function(packages = REQUIRED_PACKAGES,
                                      repos = "https://cloud.r-project.org") {
  # Step 1: Query currently installed packages
  installed <- rownames(utils::installed.packages())
  missing_pkgs <- setdiff(packages, installed)

  # Step 2: Install missing packages if any
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages as CRAN Windows binaries: ",
            paste(missing_pkgs, collapse = ", "))

    # Windows binary installation avoids requirement of Rtools compilation
    utils::install.packages(missing_pkgs, repos = repos, type = "binary")

    # Step 3: Verify successful installation
    recheck_installed <- rownames(utils::installed.packages())
    still_missing <- setdiff(missing_pkgs, recheck_installed)

    if (length(still_missing) > 0) {
      stop(sprintf(
        "CRAN Windows binary unavailable for package(s): %s. No Rtools available. Execution stopped.",
        paste(still_missing, collapse = ", ")
      ))
    }
  }

  status <- setNames(packages %in% rownames(utils::installed.packages()), packages)
  return(status)
}

#' Load required project libraries cleanly
#'
#' Attaches required libraries into the R search path, while strictly ensuring
#' that packages listed in `exclude_attach` (specifically `data.table`) are NOT
#' attached via `library()`. For unattached packages, namespace loading is checked.
#'
#' @param packages Character vector of packages to load.
#' @param exclude_attach Packages to exclude from attaching (e.g. data.table).
#' @return Character vector of attached package names.
load_libraries <- function(packages = REQUIRED_PACKAGES,
                           exclude_attach = UNATTACHED_PACKAGES) {
  # Step 1: Identify packages to attach via library()
  packages_to_attach <- setdiff(packages, exclude_attach)

  for (pkg in packages_to_attach) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }

  # Step 2: Verify unattached packages are accessible via namespace without attaching
  for (pkg in exclude_attach) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' namespace could not be loaded.", pkg))
    }
    # Ensure package is NOT present in the search path
    attached_pattern <- paste0("package:", pkg)
    if (attached_pattern %in% search()) {
      detach(attached_pattern, character.only = TRUE, unload = FALSE)
    }
  }

  return(packages_to_attach)
}

#' Create required project directory structure
#'
#' Ensures that all standard project folders exist on disk.
#'
#' @param directories Character vector of directory paths.
#' @return Character vector of created or existing directory paths.
create_project_directories <- function(directories = PROJECT_DIRECTORIES) {
  for (dir_path in directories) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Created directory: %s", dir_path))
    }
  }
  return(directories)
}

#' Setup runner function
#'
#' Coordinates package verification, library loading, and directory creation,
#' printing a summary report of the setup status.
#'
#' @return Invisibly returns TRUE on successful completion.
run_setup <- function() {
  message("=== Citi Bike Rebalancing: Setup & Environment Initialization ===")

  # 1. Package verification and installation
  install_status <- ensure_packages_installed(REQUIRED_PACKAGES)
  message(sprintf("Package verification: %d/%d packages installed.",
                  sum(install_status), length(install_status)))

  # 2. Package loading
  attached <- load_libraries(REQUIRED_PACKAGES, UNATTACHED_PACKAGES)
  message(sprintf("Attached packages: %s", paste(attached, collapse = ", ")))
  message("Unattached (namespaced only): data.table (accessible via data.table::fread)")

  # 3. Directory creation
  dirs <- create_project_directories(PROJECT_DIRECTORIES)
  message(sprintf("Project directories verified (%d total).", length(dirs)))

  message("=== Setup completed successfully. ===\n")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Script Runner
# ------------------------------------------------------------------------------
run_setup()
