# Define required packages
packages <- c(
  "tidyverse",
  "vegan",
  "glmmTMB",
  "DHARMa",
  "easystats",
  "deSolve",
  "emmeans"
)

# Install any packages not yet installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))