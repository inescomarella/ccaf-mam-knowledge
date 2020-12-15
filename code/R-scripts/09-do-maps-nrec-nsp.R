# File purpose: Make a video of number of records in CCMA through time
# Data: 30/11/020

# Load libraries
xfun::pkg_attach(c("tidyverse", "sf", "animation", "brazilmaps"))

conflicted::conflict_prefer("filter", "dplyr")

# Source functions
source("./R-scripts/functions/09-funs-maps-nrec-nsp.R")

# Set projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data ------------------------------------------------

br_longlat <-
  get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)

ccaf_utm <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  st_transform(utm)

cus_utm <-
  read_sf("../data/processed-data/CUs-map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(st_transform(ccaf_utm, longlat)) %>%
  st_transform(utm)

records_utm <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  st_transform(utm)

# Make grid --------------------------------------------------

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

# Count mammal species in a grid
# Takes 29s to run
ccaf_grid_utm <- count.sp.in.polygons(records_utm, ccaf_grid_utm)

# Count records of mammals in a grid
ccaf_grid_utm$nrec <- lengths(st_intersects(ccaf_grid_utm, records_utm))

ccaf_grid <-
  ccaf_grid_utm %>%
  st_transform(longlat)

# Plot -----------------------------------------------------

# Create plot list
# Important! The map_nrec_along_years is read inside the function, so pay
# attention if you change the name
nrec_plot <- 
  plot.ccaf.grid.count("nrec")

nsp_plot <- plot.ccaf.grid.count("nsp")

nrec_nsp_plot_list <- plot.nrec.nsps(nrec, nsp)

# Save plots -----------------------------------------------
nrec_plot
ggsave("../data/results/map-all-mammals-nrec.pdf",
       width = 6,
       height = 8
)

nsp_plot
ggsave("../data/results/map-all-mammals-nsp.pdf",
       width = 6,
       height = 8
)

nrec_nsp_plot_list
ggsave("../data/results/map-all-mammals-nrec-nsp.pdf",
       width = 8,
       height = 6
)
