# File purpose: Make a video os number os records in CCMA through time
# Data: 30/11/020

library(tidyverse)
library(animation)
library(sf)


conflicted::conflict_prefer("filter", "dplyr")

source("./R-scripts/functions/06b-funs-do-animation-plot-map.R")

# Load data -------------------------------------------------------------------
record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = sp::CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )
g025_geom <-
  st_read(
    dsn = "../data/processed-data",
    layer = "grid-025-ucs-joined"
  )
g025_geom <- st_transform(g025_geom, sp::CRS("+proj=longlat +datum=WGS84"))

# Pre-process data) -----------------------------------------------------------
map_nreg_along_years <- nreg.along.years(record_data, g025_geom[1:5])

# Plot -----------------------------------------------------------------------
myfill_list <- colnames(map_nreg_along_years)[6:(ncol(map_nreg_along_years) - 1)]

plot_list <- lapply(myfill_list, FUN = plot.along.years)

# Save video ----------------------------------------------------------------

saveVideo(
  expr = print(plot_list),
  video.name = "../data/results/animation-nreg-all-mammals.mp4",
  img.name = "animation-nreg-all-mammals",
  ffmpeg = ani.options("ffmpeg"),
  other.opts='-pix_fmt yuv420p -b 1000k'
)

saveHTML(
  expr = {
    png(ani.options("img.fmt"))
    print(plot_list)
    dev.off()
  },
  img.name = "custom_plot",
  use.dev = FALSE,
  ani.type = "png",
  htmlfile = "custom_device.html"
)
