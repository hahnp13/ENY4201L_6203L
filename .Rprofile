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

# Install any packages not yet installed
installed <- packages %in% rownames(utils::installed.packages())
if (any(!installed)) {
  utils::install.packages(packages[!installed])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
