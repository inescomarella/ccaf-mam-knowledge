# File purpose: Plot and export all mammal maps
# Data: 17/11/2020

# Load libraries
library(tidyverse)
library(sf)
library(viridis)
library(brazilmaps)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-maps-mammals.R")

# Projections
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

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
# Pre-process map --------------------------------------------

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
# Ignore the warnings
ccaf_grid <- 
  st_intersection(ccaf_longlat, br_longlat) %>%
  st_make_grid(square = FALSE, cellsize = 0.3) %>%
  st_transform(utm) %>%
  st_as_sf()

# Keep just the shape
ccaf_utm <- 
  st_intersection(ccaf_longlat, br_longlat) %>%
  st_transform(utm) %>%
  st_as_sf()


# Process data -----------------------------------------------

# Reproject to a metric coordinate system
rcrd_utm <- st_transform(record_pts, utm)
inst_utm <- st_transform(institute_pts, utm)

# Count records of mammals in a grid
ccaf_grid$nrec <- lengths(st_intersects(ccaf_grid, rcrd_utm))

# Count records of mammal orders in a grid
# Takes 336.622s to run
orders_list <- unique(rcrd_utm$order)
ccaf_grid <-
  count.orders.recs.in.polygons(rcrd_utm, ccaf_grid, orders_list)


# Count mammal species in a grid
# Takes 29s to run
ccaf_grid <- count.sp.in.polygons(rcrd_utm, ccaf_grid)

ccaf_grid <- st_transform(ccaf_grid, longlat)
ccaf_utm <- st_transform(ccaf_utm, longlat)

# Save plots -------------------------------------------------

# Number of total records
nmax <-
  st_drop_geometry(ccaf_grid) %>%
  select(nrec) %>%
  max()

st_intersection(ccaf_grid, ccaf_utm) %>%
  ggplot() +
  geom_sf(aes(fill = nrec), size = 0.2) +
  geom_sf(data = inst_utm, size = 0.7, color = 'white', pch = 17) +
  coord_sf(
    label_graticule = "NW"
  ) +
  labs(fill = "Records") +
  scale_fill_viridis(
    limits = c(1, nmax),
    breaks = c(
      1,
      round(nmax / 6, 0),
      round(nmax * 2 / 6, 0),
      round(nmax * 3 / 6, 0),
      round(nmax * 4 / 6, 0),
      round(nmax * 5 / 6, 0),
      nmax
    ),
    labels = c(
      1,
      round(nmax / 6, 0),
      round(nmax * 2 / 6, 0),
      round(nmax * 3 / 6, 0),
      round(nmax * 4 / 6, 0),
      round(nmax * 5 / 6, 0),
      nmax
    )
  ) +
  theme_light() +
  cowplot::background_grid("none") +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.3),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 7),
    title = element_text(size = 12)
  ) +
  guides(fill = guide_colorbar(
    draw.ulim = FALSE,
    draw.llim = FALSE
  )) +
  
  # Scale bar in the bottom right
  annotation_scale(location = "br", width_hint = 0.2) +
  
  # North arrow in the bottom right above scale bar
  annotation_north_arrow(
    location = "br",
    style = north_arrow_fancy_orienteering(text_size = 8),
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    pad_y = unit(0.26, "in")
  ) +
  theme(legend.key.size = unit(0.5, 'cm'))


plot.nrec.order("nrec") +
  ggtitle(element_blank())

# Number of species recorded
plot.nrec.order("nsp") +
  labs(fill = "Number of mammal \n species recorded") +
  ggtitle(element_blank())


list_order1 <- list("Chiroptera", "Rodentia", "Didelphimorphia", "Carnivora")
list_order2 <- list("Pilosa", "Cingulata", "Artiodactyla", "Lagomorpha")

plot_order1 <- lapply(list_order1, plot.nrec.order)
plot_order2 <- lapply(list_order2, plot.nrec.order)

legend_order1 <-lapply(plot_order1, get_legend)
legend_order2 <-lapply(plot_order2, get_legend)

title_order1 <-lapply(plot_order1, get_title)
title_order2 <-lapply(plot_order2, get_title)

plot_order1 <-lapply(plot_order1, remove.legend.title)
plot_order2 <-lapply(plot_order2, remove.legend.title)

plot_drawed_order1 <- list()
for(i in 1:length(plot_order1)) {
  plot_drawed_order1[[i]] <-
    ggdraw(plot_order1[[i]]) +
    draw_plot(legend_order1[[i]], 
              hjust = -0.27) +
    draw_plot(title_order1[[i]],
              hjust = -0.18,
              vjust = -0.22)
}

plot_drawed_order2 <- list()
for(i in 1:length(plot_order2)) {
  plot_drawed_order2[[i]] <-
    ggdraw(plot_order2[[i]]) +
    draw_plot(legend_order2[[i]], 
              hjust = -0.27) +
    draw_plot(title_order2[[i]],
              hjust = -0.18,
              vjust = -0.22)
}

final_plot1 <-
  plot_grid(
    plot_drawed_order1[[1]],
    plot_drawed_order1[[2]],
    plot_drawed_order1[[3]],
    plot_drawed_order1[[4]],
    align = "h",
    nrow = 1
  )

final_plot2 <-
  plot_grid(
    plot_drawed_order2[[1]],
    plot_drawed_order2[[2]],
    plot_drawed_order2[[3]],
    plot_drawed_order2[[4]],
    align = "h",
    nrow = 1
  )
save_plot(filename = "../data/results/orders-maps1.pdf",
          plot = final_plot1, 
          base_width = 8,
          base_height = 6)
save_plot(filename = "../data/results/orders-maps2.pdf",
          plot = final_plot2, 
          base_width = 8,
          base_height = 6)

ggsave("../data/results/map-all-mammals-nreg.pdf",
       width = 3,
       height = 4
)


ggsave("../data/results/map-all-mammals-nsp.pdf",
       width = 3,
       height = 4
)
