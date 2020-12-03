# File purpose: Plot and export all mammal maps
# Data: 17/11/2020

# Load libraries
library(tidyverse)
library(sf)
library(viridis)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-maps-mammals.R")

# Load data -----------------------------------------------------------------
record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = sp::CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )

g025_geom <-
  st_read(dsn = "../data/processed-data/", layer = "grid-025-clipped")

g025_geom <-
  st_transform(g025_geom, sp::CRS("+proj=longlat +datum=WGS84"))

# Process data ---------------------------------------------------------------

# Count records of mammals in a grid
g025_geom$nrec <- lengths(st_intersects(g025_geom, record_data))

# Count records of mammal orders in a grid
# Takes 336.622s to run
orders_list <- unique(record_data$order)
g025_geom <-
  count.orders.recs.in.polygons(record_data, g025_geom, orders_list)


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
plot_nrec_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = nrec), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis(
    limits = c(5, max(g025_geom$nrec)),
    breaks = c(
      5,
      round(max(g025_geom$nrec) / 6, 0),
      round(max(g025_geom$nrec) * 2 / 6, 0),
      round(max(g025_geom$nrec) * 3 / 6, 0),
      round(max(g025_geom$nrec) * 4 / 6, 0),
      round(max(g025_geom$nrec) * 5 / 6, 0),
      max(g025_geom$nrec)
    ),
    labels = c(
      5,
      round(max(g025_geom$nrec) / 6, 0),
      round(max(g025_geom$nrec) * 2 / 6, 0),
      round(max(g025_geom$nrec) * 3 / 6, 0),
      round(max(g025_geom$nrec) * 4 / 6, 0),
      round(max(g025_geom$nrec) * 5 / 6, 0),
      max(g025_geom$nrec)
    )
  ) +
  customPlot

# Plot number of mammal species in grid
plot_nsp_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  labs(fill = "Number of mammal \n species recorded") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$nsp)),
    breaks = c(
      1,
      round(max(g025_geom$nsp) / 6, 0),
      round(max(g025_geom$nsp) * 2 / 6, 0),
      round(max(g025_geom$nsp) * 3 / 6, 0),
      round(max(g025_geom$nsp) * 4 / 6, 0),
      round(max(g025_geom$nsp) * 5 / 6, 0),
      max(g025_geom$nsp)
    ),
    labels = c(
      1,
      round(max(g025_geom$nsp) / 6, 0),
      round(max(g025_geom$nsp) * 2 / 6, 0),
      round(max(g025_geom$nsp) * 3 / 6, 0),
      round(max(g025_geom$nsp) * 4 / 6, 0),
      round(max(g025_geom$nsp) * 5 / 6, 0),
      max(g025_geom$nsp)
    )
  ) +
  customPlot

# Plot number of orders records in grid
plot_roden_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Rodentia), size = 0.2) +
  ggtitle("Rodentia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Rodentia)),
    breaks = c(
      1,
      round(max(g025_geom$Rodentia) / 6, 0),
      round(max(g025_geom$Rodentia) * 2 / 6, 0),
      round(max(g025_geom$Rodentia) * 3 / 6, 0),
      round(max(g025_geom$Rodentia) * 4 / 6, 0),
      round(max(g025_geom$Rodentia) * 5 / 6, 0),
      max(g025_geom$Rodentia)
    ),
    labels = c(
      1,
      round(max(g025_geom$Rodentia) / 6, 0),
      round(max(g025_geom$Rodentia) * 2 / 6, 0),
      round(max(g025_geom$Rodentia) * 3 / 6, 0),
      round(max(g025_geom$Rodentia) * 4 / 6, 0),
      round(max(g025_geom$Rodentia) * 5 / 6, 0),
      max(g025_geom$Rodentia)
    )
  ) +
  customPlot

plot_prima_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Primates), size = 0.2) +
  ggtitle("Primates") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Primates)),
    breaks = c(
      1,
      round(max(g025_geom$Primates) / 6, 0),
      round(max(g025_geom$Primates) * 2 / 6, 0),
      round(max(g025_geom$Primates) * 3 / 6, 0),
      round(max(g025_geom$Primates) * 4 / 6, 0),
      round(max(g025_geom$Primates) * 5 / 6, 0),
      max(g025_geom$Primates)
    ),
    labels = c(
      1,
      round(max(g025_geom$Primates) / 6, 0),
      round(max(g025_geom$Primates) * 2 / 6, 0),
      round(max(g025_geom$Primates) * 3 / 6, 0),
      round(max(g025_geom$Primates) * 4 / 6, 0),
      round(max(g025_geom$Primates) * 5 / 6, 0),
      max(g025_geom$Primates)
    )
  ) +
  customPlot

plot_carni_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Carnivora), size = 0.2) +
  ggtitle("Carnivora") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Carnivora)),
    breaks = c(
      1,
      round(max(g025_geom$Carnivora) / 6, 0),
      round(max(g025_geom$Carnivora) * 2 / 6, 0),
      round(max(g025_geom$Carnivora) * 3 / 6, 0),
      round(max(g025_geom$Carnivora) * 4 / 6, 0),
      round(max(g025_geom$Carnivora) * 5 / 6, 0),
      max(g025_geom$Carnivora)
    ),
    labels = c(
      1,
      round(max(g025_geom$Carnivora) / 6, 0),
      round(max(g025_geom$Carnivora) * 2 / 6, 0),
      round(max(g025_geom$Carnivora) * 3 / 6, 0),
      round(max(g025_geom$Carnivora) * 4 / 6, 0),
      round(max(g025_geom$Carnivora) * 5 / 6, 0),
      max(g025_geom$Carnivora)
    )
  ) +
  customPlot

plot_didel_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.2) +
  ggtitle("Didelphimorphia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Didelphimorphia)),
    breaks = c(
      1,
      round(max(g025_geom$Didelphimorphia) / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 2 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 3 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 4 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 5 / 6, 0),
      max(g025_geom$Didelphimorphia)
    ),
    labels = c(
      1,
      round(max(g025_geom$Didelphimorphia) / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 2 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 3 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 4 / 6, 0),
      round(max(g025_geom$Didelphimorphia) * 5 / 6, 0),
      max(g025_geom$Didelphimorphia)
    )
  ) +
  customPlot

plot_peris_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Perissodactyla), size = 0.2) +
  ggtitle("Perissodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Perissodactyla)),
    breaks = c(
      1,
      round(max(g025_geom$Perissodactyla) / 6, 0),
      round(max(g025_geom$Perissodactyla) * 2 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 3 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 4 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 5 / 6, 0),
      max(g025_geom$Perissodactyla)
    ),
    labels = c(
      1,
      round(max(g025_geom$Perissodactyla) / 6, 0),
      round(max(g025_geom$Perissodactyla) * 2 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 3 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 4 / 6, 0),
      round(max(g025_geom$Perissodactyla) * 5 / 6, 0),
      max(g025_geom$Perissodactyla)
    )
  ) +
  customPlot

plot_chiro_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Chiroptera), size = 0.2) +
  ggtitle("Chiroptera") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Chiroptera)),
    breaks = c(
      1,
      round(max(g025_geom$Chiroptera) / 6, 0),
      round(max(g025_geom$Chiroptera) * 2 / 6, 0),
      round(max(g025_geom$Chiroptera) * 3 / 6, 0),
      round(max(g025_geom$Chiroptera) * 4 / 6, 0),
      round(max(g025_geom$Chiroptera) * 5 / 6, 0),
      max(g025_geom$Chiroptera)
    ),
    labels = c(
      1,
      round(max(g025_geom$Chiroptera) / 6, 0),
      round(max(g025_geom$Chiroptera) * 2 / 6, 0),
      round(max(g025_geom$Chiroptera) * 3 / 6, 0),
      round(max(g025_geom$Chiroptera) * 4 / 6, 0),
      round(max(g025_geom$Chiroptera) * 5 / 6, 0),
      max(g025_geom$Chiroptera)
    )
  ) +
  customPlot

plot_lagom_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Lagomorpha), size = 0.2) +
  ggtitle("Lagomorpha") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Lagomorpha)),
    breaks = c(
      1,
      round(max(g025_geom$Lagomorpha) / 6, 0),
      round(max(g025_geom$Lagomorpha) * 2 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 3 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 4 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 5 / 6, 0),
      max(g025_geom$Lagomorpha)
    ),
    labels = c(
      1,
      round(max(g025_geom$Lagomorpha) / 6, 0),
      round(max(g025_geom$Lagomorpha) * 2 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 3 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 4 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 5 / 6, 0),
      max(g025_geom$Lagomorpha)
    )
  ) +
  customPlot

plot_pilos_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Pilosa), size = 0.2) +
  ggtitle("Pilosa") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Lagomorpha)),
    breaks = c(
      1,
      round(max(g025_geom$Lagomorpha) / 6, 0),
      round(max(g025_geom$Lagomorpha) * 2 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 3 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 4 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 5 / 6, 0),
      max(g025_geom$Lagomorpha)
    ),
    labels = c(
      1,
      round(max(g025_geom$Lagomorpha) / 6, 0),
      round(max(g025_geom$Lagomorpha) * 2 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 3 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 4 / 6, 0),
      round(max(g025_geom$Lagomorpha) * 5 / 6, 0),
      max(g025_geom$Lagomorpha)
    )
  ) +
  customPlot

plot_artio_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Artiodactyla), size = 0.2) +
  ggtitle("Artiodactyla") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Artiodactyla)),
    breaks = c(
      1,
      round(max(g025_geom$Artiodactyla) / 6, 0),
      round(max(g025_geom$Artiodactyla) * 2 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 3 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 4 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 5 / 6, 0),
      max(g025_geom$Artiodactyla)
    ),
    labels = c(
      1,
      round(max(g025_geom$Artiodactyla) / 6, 0),
      round(max(g025_geom$Artiodactyla) * 2 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 3 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 4 / 6, 0),
      round(max(g025_geom$Artiodactyla) * 5 / 6, 0),
      max(g025_geom$Artiodactyla)
    )
  ) +
  customPlot

plot_cingu_g025 <-
  ggplot(g025_geom) +
  geom_sf(aes(fill = Cingulata), size = 0.2) +
  ggtitle("Cingulata") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis(
    limits = c(1, max(g025_geom$Cingulata)),
    breaks = c(
      1,
      round(max(g025_geom$Cingulata) / 6, 0),
      round(max(g025_geom$Cingulata) * 2 / 6, 0),
      round(max(g025_geom$Cingulata) * 3 / 6, 0),
      round(max(g025_geom$Cingulata) * 4 / 6, 0),
      round(max(g025_geom$Cingulata) * 5 / 6, 0),
      max(g025_geom$Cingulata)
    ),
    labels = c(
      1,
      round(max(g025_geom$Cingulata) / 6, 0),
      round(max(g025_geom$Cingulata) * 2 / 6, 0),
      round(max(g025_geom$Cingulata) * 3 / 6, 0),
      round(max(g025_geom$Cingulata) * 4 / 6, 0),
      round(max(g025_geom$Cingulata) * 5 / 6, 0),
      max(g025_geom$Cingulata)
    )
  ) +
  customPlot

# Save plots -----------------------------------------------------------------
plot_nrec_g025
ggsave("../data/results/map-all-mammals-nrec.pdf",
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

