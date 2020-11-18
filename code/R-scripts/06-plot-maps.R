# File purpose: Plot and export all mammal maps
# Data: 17/11/2020
#
# To do:
#   - Map with Brazil in background

# Load libraries
x <-
  c("dplyr", "sf", "raster", "ggplot2", "conflicted", "scatterpie")
lapply(x, library, character.only = TRUE)

conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/04-funs-plot-maps.R")

# Load iin data
record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )
municipalities <-
  st_read(
    dsn = "../data/processed-data",
    layer = "municipalities-joined"
  )
g025_geom <-
  st_read(
    dsn = "../data/processed-data",
    layer = "grid-025-ucs-joined",
    crs = CRS("+proj=longlat +datum=WGS84")
  )
g050_geom <-
  st_read(
    dsn = "../data/processed-data",
    layer = "grid-050-ucs-joined",
    crs = CRS("+proj=longlat +datum=WGS84")
  )

municipalities <- 
  st_transform(municipalities, CRS("+proj=longlat +datum=WGS84"))

############################ Prepare data #####################################

# Count records of mammals in a grid
g025_geom$nreg <- lengths(st_intersects(g025_geom, record_data))
g050_geom$nreg <- lengths(st_intersects(g050_geom, record_data))

# Count records of mammals in municipalities
municipalities$nreg <-
  lengths(st_intersects(municipalities, record_data))

# Count records of mammal orders in a grid
# Takes 336.622s to run
orders_list <- unique(record_data$order)
g025_geom <-
  count.orders.regs.in.polygons(record_data, g025_geom, orders_list)
g050_geom <-
  count.orders.regs.in.polygons(record_data, g050_geom, orders_list)

# Count mammal species in a grid
# Takes 29s to run
g025_geom <- count.sp.in.polygons(record_data, g025_geom)
g050_geom <- count.sp.in.polygons(record_data, g050_geom)

# Get pies chart coordinates
# Use centre point coordinates to specify pie chart XY coords in the map
g025_coord <-
  as.data.frame(coordinates(as(g025_geom, "Spatial")))
g050_coord <-
  as.data.frame(coordinates(as(g050_geom, "Spatial")))

g025_geom <- bind_cols(g025_coord, g025_geom)
g050_geom <- bind_cols(g050_coord, g050_geom)

# Set radius proportional to the number of records
g025_geom$radius <- g025_geom$nreg / (max(g025_geom$nreg) * 2)
g050_geom$radius <- g050_geom$nreg / (max(g050_geom$nreg) * 2)

# Return to sf class to plot as geom_sf()
g025_geom <- st_as_sf(g025_geom)
g050_geom <- st_as_sf(g050_geom)

############################### Plot maps #####################################

# Customized theme
customPlot = list(
  theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title =  element_text(size = 8),
      legend.text = element_text(size = 8)
    )
)

# Plot number of mammal records in grid
plot_nreg_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_nreg_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot number of mammal species in grid
plot_nsp_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  labs(fill = "Number of mammal \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_nsp_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  labs(fill = "Number of mammal \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot number of orders records in grid
plot_roden_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Rodentia), size = 0.2) +
  ggtitle("Rodentia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_prima_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Primates), size = 0.2) +
  ggtitle("Primates") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_carni_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Carnivora), size = 0.2) +
  ggtitle("Carnivora") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_didel_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.2) +
  ggtitle("Didelphimorphia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_peris_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Perissodactyla), size = 0.2) +
  ggtitle("Perissodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_chiro_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Chiroptera), size = 0.2) +
  ggtitle("Chiroptera") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_lagom_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Lagomorpha), size = 0.2) +
  ggtitle("Lagomorpha") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_pilos_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Pilosa), size = 0.2) +
  ggtitle("Pilosa") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_artio_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Artiodactyla), size = 0.2) +
  ggtitle("Artiodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_cingu_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Cingulata), size = 0.2) +
  ggtitle("Cingulata") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_siren_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Sirenia), size = 0.2) +
  ggtitle("Sirenia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_roden_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Rodentia), size = 0.2) +
  ggtitle("Rodentia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_prima_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Primates), size = 0.2) +
  ggtitle("Primates") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_carni_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Carnivora), size = 0.2) +
  ggtitle("Carnivora") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_didel_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.2) +
  ggtitle("Didelphimorphia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_peris_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Perissodactyla), size = 0.2) +
  ggtitle("Perissodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_chiro_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Chiroptera), size = 0.2) +
  ggtitle("Chiroptera") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_lagom_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Lagomorpha), size = 0.2) +
  ggtitle("Lagomorpha") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_pilos_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Pilosa), size = 0.2) +
  ggtitle("Pilosa") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_artio_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Artiodactyla), size = 0.2) +
  ggtitle("Artiodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_cingu_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Cingulata), size = 0.2) +
  ggtitle("Cingulata") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_siren_g050 <-
  ggplot(g050_geom) +
  geom_sf(aes(fill = Sirenia), size = 0.2) +
  ggtitle("Sirenia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot municipalities
municipalities_cropped <-
  st_crop(
    municipalities,
    xmin = -41.8798,
    xmax = -38,
    ymin = -21.30178,
    ymax = -13.00164
  )

plot_nreg_municipalities <-
  ggplot(municipalities_cropped) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot pie chart maps
plot_pie_orders_g025 <-
  ggplot(g025_geom) +
  geom_sf(size = 0.2) +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = as.data.frame(g025_geom),
    cols = orders_list,
    size = 0.2,
    alpha = 1.5
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.75),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm")
  ) +
  ylab(element_blank()) +
  xlab(element_blank())

plot_pie_orders_g050 <-
  ggplot(g050_geom) +
  geom_sf(size = 0.2) +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = as.data.frame(g050_geom),
    cols = orders_list,
    size = 0.2,
    alpha = 1.5
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.75),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm")
  ) +
  ylab(element_blank()) +
  xlab(element_blank())

############################### Save maps #####################################
plot_nreg_g025
ggsave("../data/results/map-nreg-g025.pdf",
       width = 3,
       height = 4)

plot_nreg_g050
ggsave("../data/results/map-nreg-g050.pdf",
       width = 3,
       height = 4)

plot_nsp_g025
ggsave("../data/results/map-nsp-g025.pdf",
       width = 3,
       height = 4)

plot_nsp_g050
ggsave("../data/results/map-nsp-g050.pdf",
       width = 3,
       height = 4)

plot_roden_g025
ggsave("../data/results/map-rodentia-g025.pdf",
       width = 3,
       height = 4)
plot_prima_g025
ggsave("../data/results/map-primates-g025.pdf",
       width = 3,
       height = 4)
plot_carni_g025
ggsave("../data/results/map-carnivora-g025.pdf",
       width = 3,
       height = 4)
plot_didel_g025
ggsave("../data/results/map-didelphimorphia-g025.pdf",
       width = 3,
       height = 4)
plot_peris_g025
ggsave("../data/results/map-perissodactyla-g025.pdf",
       width = 3,
       height = 4)
plot_chiro_g025
ggsave("../data/results/map-chiroptera-g025.pdf",
       width = 3,
       height = 4)
plot_lagom_g025
ggsave("../data/results/map-lagomorpha-g025.pdf",
       width = 3,
       height = 4)
plot_pilos_g025
ggsave("../data/results/map-pilosa-g025.pdf",
       width = 3,
       height = 4)
plot_artio_g025
ggsave("../data/results/map-artiodactyla-g025.pdf",
       width = 3,
       height = 4)
plot_cingu_g025
ggsave("../data/results/map-cingulata-g025.pdf",
       width = 3,
       height = 4)
plot_siren_g025
ggsave("../data/results/map-sirenia-g025.pdf",
       width = 3,
       height = 4)

plot_roden_g050
ggsave("../data/results/map-rodentia-g050.pdf",
       width = 3,
       height = 4)
plot_prima_g050
ggsave("../data/results/map-primates-g050.pdf",
       width = 3,
       height = 4)
plot_carni_g050
ggsave("../data/results/map-carnivora-g050.pdf",
       width = 3,
       height = 4)
plot_didel_g050
ggsave("../data/results/map-didelphimorphia-g050.pdf",
       width = 3,
       height = 4)
plot_peris_g050
ggsave("../data/results/map-perissodactyla-g050.pdf",
       width = 3,
       height = 4)
plot_chiro_g050
ggsave("../data/results/map-chiroptera-g050.pdf",
       width = 3,
       height = 4)
plot_lagom_g050
ggsave("../data/results/map-lagomorpha-g050.pdf",
       width = 3,
       height = 4)
plot_pilos_g050
ggsave("../data/results/map-pilosa-g050.pdf",
       width = 3,
       height = 4)
plot_artio_g050
ggsave("../data/results/map-artiodactyla-g050.pdf",
       width = 3,
       height = 4)
plot_cingu_g050
ggsave("../data/results/map-cingulata-g050.pdf",
       width = 3,
       height = 4)
plot_siren_g050
ggsave("../data/results/map-sirenia-g050.pdf",
       width = 3,
       height = 4)

plot_nreg_municipalities
ggsave("../data/results/plot_nreg_municipalities.pdf",
       width = 3,
       height = 4)

plot_pie_orders_g025
ggsave("../data/results/map-pie-orders-g025.pdf",
       width = 3,
       height = 4)

plot_pie_orders_g050
ggsave("../data/results/map-pie-orders-g050.pdf",
       width = 3,
       height = 4)

