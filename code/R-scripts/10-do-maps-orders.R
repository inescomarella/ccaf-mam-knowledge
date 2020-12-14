# File purpose: Plot and export all mammal maps
# Data: 17/11/2020

# Load libraries
xfun::pkg_attach(c("tidyverse", "sf", "viridis", "brazilmaps"))

# To save Lato font
extrafont::loadfonts()

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/10-funs-maps-orders.R")

# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data --------------------------------------------------

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

institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

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

# Process data ---------------------------------------------

# Count records of mammal order in a grid
# Takes 336.622s to run
order_names <- sort(unique(records_utm$order))

ccaf_grid_utm <-
  count.orders.recs.in.polygons(records_utm, ccaf_grid_utm, order_names)

# Convert to longlat to apply plot functions
ccaf_grid <-
  ccaf_grid_utm %>%
  st_transform(longlat)

# Plot -----------------------------------------------------

order_list <- as.list(order_names)
names(order_list) <- c(order_names)

# Remove Sirenia and Perissodactyla, too few records
order_list <-
  list_modify(
    order_list,
    Sirenia = zap(),
    Perissodactyla = zap()
  )

# Important! The ccaf_grid and institute_pts are read inside the function, so
# pay  attention if you change the name
plot_order <- lapply(order_list, plot.nrec.order)

legend_order <- lapply(plot_order, get_legend)

plot_order <- lapply(plot_order, remove.legend)

plot_drawed_order <- list()
for (i in 1:length(plot_order)) {
  plot_drawed_order[[i]] <-
    ggdraw(plot_order[[i]]) +
    draw_plot(legend_order[[i]],
      hjust = -0.25
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
  filename = "../data/results/maps-orders-nrec.pdf",
  plot = final_plot_together,
  base_width = 8,
  base_height = 6 #7
)
