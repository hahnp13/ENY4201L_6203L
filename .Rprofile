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

installed <- packages %in% rownames(utils::installed.packages())

if (any(!installed)) {
  message("Installing missing packages...")
  utils::install.packages(packages[!installed], repos = "https://cran.rstudio.com/")
  message("Installation complete.")
}

message("Loading packages...")
invisible(lapply(packages, library, character.only = TRUE))
message("All packages loaded.")
