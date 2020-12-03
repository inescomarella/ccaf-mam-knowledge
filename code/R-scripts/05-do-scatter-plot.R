# File purpose: Do scatter plot of the number of mammal records
# Data: 17/11/2020

# Load in libraries
library(tidyverse)
library(sf)
library(sp)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-scatter-plot.R")

# Load data -------------------------------------------------------------------
record_pts <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )

institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

cus_geom <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map")

g025_geom <-
  st_read(dsn = "../data/processed-data/", layer = "grid-025-clipped")

# Process data ----------------------------------------------------------------

# Reproject to a metric coordinate system
utm <-
  CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rcrd_utm <- st_transform(record_pts, utm)
inst_utm <- st_transform(institute_pts, utm)
cus_utm <- st_set_crs(cus_geom, utm)
g025_utm <- st_transform(g025_geom, utm)

# Measure mean polygon area
g025_area <- mean(st_area(g025_utm)) # 484.516.066 m² = 484 km²

# Measure the distance of each cell grid to the nearest research institute
g025_utm <- get.nearest.dist(inst_utm, g025_utm)

# Count records per cell
g025_utm$nrec <- lengths(st_intersects(g025_utm, rcrd_utm))

# Get CU presence in each cell
g025_utm$CU <- lengths(st_intersects(g025_utm, cus_utm))

# Save plots -----------------------------------------------------------------
scatter_plot <-
  st_drop_geometry(g025_utm) %>%
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
