# File purpose: Plot and export all mammal maps
# Data: 17/11/2020

# Load libraries
x <-
  c("dplyr", "ggplot2", "sf")
lapply(x, library, character.only = TRUE)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-plot-mammal-maps.R")

# Load in data
record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = sp::CRS("+proj=longlat +datum=WGS84"),
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
    crs = sp::CRS("+proj=longlat +datum=WGS84")
  )
g050_geom <-
  st_read(
    dsn = "../data/processed-data",
    layer = "grid-050-ucs-joined",
    crs = sp::CRS("+proj=longlat +datum=WGS84")
  )

municipalities <-
  st_transform(municipalities, sp::CRS("+proj=longlat +datum=WGS84"))

# Process data ---------------------------------------------------------------

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


# Plot -----------------------------------------------------------------------

# Customized theme
customPlot <- list(
  theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title = element_text(size = 8),
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

# Plot number of mammal species in grid
plot_nsp_g025 <-
  ggplot(g025_geom) +
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

# Save plots -----------------------------------------------------------------
plot_nreg_g025
ggsave("../data/results/map-all-mammals-nreg.pdf",
  width = 3,
  height = 4
)

plot_nsp_g025
ggsave("../data/results/map-all-mammals-nsp.pdf",
  width = 3,
  height = 4
)

plot_roden_g025
ggsave("../data/results/map-order-rodentia.pdf",
  width = 3,
  height = 4
)

plot_prima_g025
ggsave("../data/results/map-order-primates.pdf",
  width = 3,
  height = 4
)

plot_carni_g025
ggsave("../data/results/map-order-carnivora.pdf",
  width = 3,
  height = 4
)

plot_didel_g025
ggsave("../data/results/map-order-didelphimorphia.pdf",
  width = 3,
  height = 4
)

plot_peris_g025
ggsave("../data/results/map-order-perissodactyla.pdf",
  width = 3,
  height = 4
)

plot_chiro_g025
ggsave("../data/results/map-order-chiroptera.pdf",
  width = 3,
  height = 4
)

plot_lagom_g025
ggsave("../data/results/map-order-lagomorpha.pdf",
  width = 3,
  height = 4
)

plot_pilos_g025
ggsave("../data/results/map-order-pilosa.pdf",
  width = 3,
  height = 4
)

plot_artio_g025
ggsave("../data/results/map-order-artiodactyla.pdf",
  width = 3,
  height = 4
)

plot_cingu_g025
ggsave("../data/results/map-order-cingulata.pdf",
  width = 3,
  height = 4
)

plot_siren_g025
ggsave("../data/results/map-order-sirenia.pdf",
  width = 3,
  height = 4
)
