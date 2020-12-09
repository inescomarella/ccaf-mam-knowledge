# File purpose: Make a video of number of records in CCMA through time
# Data: 30/11/020

# Load libraries
library(tidyverse)
library(animation)
library(sf)
library(brazilmaps)


conflicted::conflict_prefer("filter", "dplyr")

# Source functions
source("./R-scripts/functions/07-funs-animation.R")

# Load data ------------------------------------------------

record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = sp::CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )

corridors <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
)

# Pre-process map ------------------------------------------

longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Get Brazil map
br_longlat <-
  get_brmap(geo = "Brazil")%>% 
  st_as_sf() %>%
  st_transform(longlat)

# Keep only CCAF
ccaf_longlat <- 
  corridors %>% 
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat)

ccaf_clipped <-
  st_intersection(ccaf_longlat, br_longlat)

# Clip, make grid and convert to metric coordinate system
# Ignore the warnings

ccaf_grid <- 
  ccaf_clipped %>%
  st_make_grid(square = FALSE, cellsize = 0.3) %>%
  st_intersection(ccaf_longlat, br_longlat) %>%
  st_as_sf()

ccaf_grid <-
  st_intersection(ccaf_grid, ccaf_clipped)

# Pre-process data -----------------------------------------

# Create the table with counts of records along years
map_nrec_along_years <- nrec.along.years(record_data, ccaf_grid)

map_nsp_along_years <- nsp.along.years(record_data, ccaf_grid)

# Plot -----------------------------------------------------

# Get columns names
myfill_list <- colnames(map_nrec_along_years)[3:(ncol(map_nrec_along_years) - 1)]

# Create plot list
# Important! The map_nrec_along_years is read inside the function, so pay
# attention if you chance the name
nrec_plot_list <- 
  lapply(myfill_list, FUN = plot.nrec.along.years)

nsp_plot_list <- 
  lapply(myfill_list, FUN = plot.nsp.along.years)

together_plot_list <- 
  lapply(myfill_list, FUN = plot.together.years)

# Save video -----------------------------------------------
saveGIF(
  print(together_plot_list),
  movie.name = "../animation_together.gif",
  img.name = "Rplot",
  convert = "magick"
)

saveGIF(
  print(nsp_plot_list),
  movie.name = "../animation_nsp.gif",
  img.name = "Rplot",
  convert = "magick"
)

# Save in gif format
# The resolution is better than mp4
saveGIF(
  print(nrec_plot_list),
  movie.name = "../animation.gif",
  img.name = "Rplot",
  convert = "magick"
)

# Save in mp4 format
# Unfortunately the resolution is not very good :c
saveVideo(
  expr = print(plot_list),
  video.name = "../data/results/animation-nrec-all-mammals.mp4",
  img.name = "animation-nrec-all-mammals",
  ffmpeg = ani.options("ffmpeg"),
  other.opts = "-pix_fmt yuv420p -b 1000k"
)


# Save in html
# I got an error when trying to change de directory
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
