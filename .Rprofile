# Suppress interactive prompts
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Define required packages
packages <- c(
  "tidyverse",
  "vegan",
  "glmmTMB",
  "DHARMa",
  "easystats",
  "deSolve",
  "emmeans",
  "viridis"
)

# Set up personal library without prompting
lib_path <- Sys.getenv("R_LIBS_USER")
if (lib_path == "") {
  lib_path <- file.path(Sys.getenv("HOME"), ".R", "library")
}
dir.create(lib_path, showWarnings = FALSE, recursive = TRUE)
.libPaths(lib_path)

# Install missing packages non-interactively
installed <- packages %in% rownames(utils::installed.packages())
if (any(!installed)) {
  utils::install.packages(
    packages[!installed],
    lib = lib_path,
    repos = "https://cran.rstudio.com/",
    dependencies = TRUE,
    quiet = TRUE
  )
}

# Load all packages silently
suppressPackageStartupMessages(
  invisible(lapply(packages, library, character.only = TRUE))
)
