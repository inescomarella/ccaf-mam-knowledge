# File purpose: Make a video of number of records in CCMA through time
# Data: 30/11/020

# Load libraries
xfun::pkg_attach(c("tidyverse", "sf", "animation", "brazilmaps"))

conflicted::conflict_prefer("filter", "dplyr")

# Source functions
source("./R-scripts/functions/11-funs-animation.R")

# Set projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data ------------------------------------------------

record_utm <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  st_transform(utm)

br_longlat <-
  get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)

ccaf_utm <-
  st_read(
    dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
    layer = "corredores_ppg7",
    check_ring_dir = TRUE
  ) %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  st_transform(utm)

# Make grid ------------------------------------------------

# Expected area = 1000m * 1000000m = 1e+9 m2 = 1000 km2
cellarea <- 1000 * (1e+6)
cellsize <- 2 * sqrt(cellarea / ((3 * sqrt(3) / 2))) * sqrt(3) / 2

# Make the hexagon grid with the expected area
ccaf_grid_utm <-
  ccaf_utm %>%
  st_make_grid(cellsize = cellsize, square = FALSE) %>%
  st_as_sf() %>%
  st_intersection(ccaf_utm)

# Pre-process data -----------------------------------------

# Create the table with counts of records along years
map_nrec_along_years <-
  nrec.along.years(record_utm, ccaf_grid_utm) %>%
  st_transform(longlat)

map_nsp_along_years <-
  nsp.along.years(record_utm, ccaf_grid_utm) %>%
  st_transform(longlat)

# Plot -----------------------------------------------------

# Get columns names
myfill_list <-
  colnames(map_nrec_along_years)[3:(ncol(map_nrec_along_years) - 1)]

# Create plot list
# Important! The map_nrec_along_years and map_nsp_along_years are read inside the function, so pay
# attention if you chance the name
nrec_plot_list <-
  lapply(myfill_list, FUN = plot.nrec.along.years)

nsp_plot_list <-
  lapply(myfill_list, FUN = plot.nsp.along.years)

together_plot_list <-
  lapply(myfill_list, FUN = plot.together.years)

# Save video -----------------------------------------------
# Save in gif format
saveGIF(
  print(nrec_plot_list),
  movie.name = "animation-nrec.gif",
  img.name = "Rplot",
  convert = "magick"
)

saveGIF(
  print(nsp_plot_list),
  movie.name = "animation-nsp.gif",
  img.name = "Rplot",
  convert = "magick"
)

saveGIF(
  print(together_plot_list),
  movie.name = "animation-nrec-nsp.gif",
  img.name = "Rplot",
  convert = "magick"
)
