# File purpose: Do scatter plot of the number of mammal records
# Data: 17/11/2020

# Load in libraries
library(tidyverse)
library(sf)
library(brazilmaps)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-scatter-plot.R")

# Projections
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load data -------------------------------------------------------------------
record_pts <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )

institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

cus_geom <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map")

corridors <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
)

# Pre-process map ------------------------------------------------------------

# Keep only CCAF
ccaf_longlat <- 
  corridors %>% 
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat)

# Get Brazil map
br_longlat <-
  get_brmap(geo = "Brazil")%>% 
  st_as_sf() %>%
  st_transform(longlat)

# Clip, make grid and convert to metric coordinate system
ccaf_grid <- 
  st_intersection(ccaf_longlat, br_longlat) %>%
  st_make_grid(square = FALSE, cellsize = 0.3) %>%
  st_transform(utm) %>%
  st_as_sf()

# Check cellsize
ccaf_grid %>% 
  st_area() %>% 
  mean()

# Process data ----------------------------------------------------------------

# Reproject to a metric coordinate system
rcrd_utm <- st_transform(record_pts, utm)
inst_utm <- st_transform(institute_pts, utm)
cus_utm <- st_set_crs(cus_geom, utm)

# Measure the distance of each cell grid to the nearest research institute
ccaf_grid <- get.nearest.dist(inst_utm, ccaf_grid)

# Count records per cell
ccaf_grid$nrec <- lengths(st_intersects(ccaf_grid, rcrd_utm))

# Get CU presence in each cell
ccaf_grid$CU <- lengths(st_intersects(ccaf_grid, cus_utm))

# Save plots -----------------------------------------------------------------
scatter_plot <-
  st_drop_geometry(ccaf_grid) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  mutate(dist_inst = dist_inst / 1000) %>%
  ggplot() +
  geom_point(aes(x = dist_inst, y = nrec, color = factor(CU))) +
  theme_light() +
  labs(color = "Conservation Unit") +
  ylab("Number of records") +
  xlab("Distance to research institute (km)")

scatter_plot
ggsave(
  "../data/results/scatter-plot-all-nrec-dist-cu.pdf",
  width = 6,
  height = 4
)
