# File purpose: Do scatter plot of the number of mammal records
# Data: 17/11/2020

# Load in libraries
xfun::pkg_attach(c("tidyverse", "sf", "brazilmaps"))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/07-funs-scatter-graph.R")

# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <- sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data --------------------------------------------------

br_longlat <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf_utm <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  st_crop(xmax = -38.7, xmin = -41.87851, ymax = -13.00164, ymin = -21.30178) %>%
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

institute_utm <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
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
  st_as_sf()

# Process data -----------------------------------------------

# Measure the distance of each cell grid to the nearest research institute
ccaf_grid_utm <- get.nearest.dist(institute_utm, ccaf_grid_utm)

# Count records per cell
ccaf_grid_utm$nrec <- lengths(st_intersects(ccaf_grid_utm, records_utm))

# Get CU presence in each cell
ccaf_grid_utm$CU <- lengths(st_intersects(ccaf_grid_utm, cus_utm))

# Plot -------------------------------------------------------
scatter_plot <-
  st_drop_geometry(ccaf_grid_utm) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  mutate(dist_inst = dist_inst / 1000) %>%
  ggplot() +
  geom_point(aes(x = dist_inst, y = nrec, color = factor(CU))) +
  theme_light() +
  labs(color = "Conservation Unit") +
  ylab("Number of records") +
  xlab("Distance to research institute (km)")

# Save plot --------------------------------------------------
scatter_plot
ggsave(
  "../data/results/scatter-graph.pdf",
  width = 6,
  height = 4
)
