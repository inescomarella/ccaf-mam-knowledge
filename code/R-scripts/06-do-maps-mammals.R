# File purpose: Plot and export all mammal maps
# Data: 17/11/2020

# Load libraries
library(tidyverse)
library(sf)
library(viridis)
library(brazilmaps)

extrafont::loadfonts()
conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-maps-mammals.R")

# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load data --------------------------------------------------
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

corridors <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
)
# Pre-process map ------------------------------------------

longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Get Brazil map
br_longlat <-
  get_brmap(geo = "Brazil") %>%
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

# Process data ---------------------------------------------

# Count records of mammals in a grid
ccaf_grid$nrec <- lengths(st_intersects(ccaf_grid, record_pts))

# Count records of mammal orders in a grid
# Takes 336.622s to run
orders_list <- unique(record_pts$order)
ccaf_grid <-
  count.orders.recs.in.polygons(record_pts, ccaf_grid, orders_list)


# Count mammal species in a grid
# Takes 29s to run
ccaf_grid <- count.sp.in.polygons(record_pts, ccaf_grid)

# Plot -----------------------------------------------------
list_order <-
  list(
    "Chiroptera",
    "Rodentia",
    "Didelphimorphia",
    "Carnivora",
    "Pilosa",
    "Cingulata",
    "Artiodactyla",
    "Lagomorpha"
  )

plot_order <- lapply(list_order, plot.nrec.order)

legend_order <- lapply(plot_order, get_legend)

plot_order <- lapply(plot_order, remove.legend)

plot_drawed_order <- list()
for (i in 1:length(plot_order)) {
  plot_drawed_order[[i]] <-
    ggdraw(plot_order[[i]]) +
    draw_plot(legend_order[[i]],
      hjust = -0.296
    )
}

final_plot_together <-
  plot_grid(
    plot_drawed_order[[1]],
    plot_drawed_order[[3]],
    plot_drawed_order[[2]],
    plot_drawed_order[[4]],
    plot_drawed_order[[5]],
    plot_drawed_order[[6]],
    plot_drawed_order[[7]],
    plot_drawed_order[[8]],
    nrow = 2
  )

# Save -----------------------------------------------------
save_plot(
  filename = "../data/results/orders-maps-together.pdf",
  plot = final_plot_together,
  base_width = 8,
  base_height = 7
)
