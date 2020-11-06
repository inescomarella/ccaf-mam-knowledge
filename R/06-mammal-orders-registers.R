setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c('dplyr', 'sf', 'raster', 'ggplot2', 'conflicted', 'scatterpie')
lapply(x, library, character.only = TRUE)

conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'select', winner = 'dplyr')

source('functions.R')

# Inputs ----
data <-
  st_read(
    '../data/mamm-data-clean.csv',
    options = c(
      'X_POSSIBLE_NAMES=decimalLongitude',
      'Y_POSSIBLE_NAMES=decimalLatitude'
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
g025 <-
  st_read(
    dsn = '../outputs',
    layer = 'grid_025_ucs_joined',
    crs = CRS("+proj=longlat +datum=WGS84")
  )
g050 <-
  st_read(
    dsn = '../outputs',
    layer = 'grid_050_ucs_joined',
    crs = CRS("+proj=longlat +datum=WGS84")
  )

# Count register per order in polygons ----
orders_list <- unique(data$order)
g025_orders_counted <- count.orders.list.in.polygons(data, g025, orders_list)
g050_orders_counted <- count.orders.list.in.polygons(data, g050, orders_list)

# Pie chart data.frame ----
g025_coord <- as.data.frame(coordinates(as(g025_orders_counted, 'Spatial')))
g050_coord <- as.data.frame(coordinates(as(g050_orders_counted, 'Spatial')))

g025_nreg <- lengths(st_intersects(g025_orders_counted, data))
g050_nreg <- lengths(st_intersects(g050_orders_counted, data))

g025_df_pie <- bind_cols(g025_coord, g025_orders_counted[83:93])
g050_df_pie <- bind_cols(g050_coord, g050_orders_counted[83:93])

g025_df_pie$radius <- g025_nreg/(max(g025_nreg)*2)
g050_df_pie$radius <- g050_nreg/(max(g050_nreg)*2)

# Plot orders maps ----
plot025_roden <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Rodentia), size = 0.25) +
  ggtitle('Rodentia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_prima <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Primates), size = 0.25) +
  ggtitle('Primates') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_carni <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Carnivora), size = 0.25) +
  ggtitle('Carnivora') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_didel <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.25) +
  ggtitle('Didelphimorphia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_peris <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Perissodactyla), size = 0.25) +
  ggtitle('Perissodactyla') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_chiro <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Chiroptera), size = 0.25) +
  ggtitle('Chiroptera') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_lagom <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Lagomorpha), size = 0.25) +
  ggtitle('Lagomorpha') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_pilos <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Pilosa), size = 0.25) +
  ggtitle('Pilosa') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_artio <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Artiodactyla), size = 0.25) +
  ggtitle('Artiodactyla') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_cingu <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Cingulata), size = 0.25) +
  ggtitle('Cingulata') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot025_siren <-
  ggplot(g025_orders_counted) +
  geom_sf(aes(fill = Sirenia), size = 0.25) +
  ggtitle('Sirenia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

plot050_roden <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Rodentia), size = 0.25) +
  ggtitle('Rodentia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_prima <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Primates), size = 0.25) +
  ggtitle('Primates') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_carni <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Carnivora), size = 0.25) +
  ggtitle('Carnivora') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_didel <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.25) +
  ggtitle('Didelphimorphia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_peris <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Perissodactyla), size = 0.25) +
  ggtitle('Perissodactyla') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_chiro <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Chiroptera), size = 0.25) +
  ggtitle('Chiroptera') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_lagom <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Lagomorpha), size = 0.25) +
  ggtitle('Lagomorpha') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_pilos <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Pilosa), size = 0.25) +
  ggtitle('Pilosa') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_artio <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Artiodactyla), size = 0.25) +
  ggtitle('Artiodactyla') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_cingu <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Cingulata), size = 0.25) +
  ggtitle('Cingulata') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
plot050_siren <-
  ggplot(g050_orders_counted) +
  geom_sf(aes(fill = Sirenia), size = 0.25) +
  ggtitle('Sirenia') +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

# Map pie chart -----
plot025_pie_orders <-
  ggplot(g025_orders_counted) +
  geom_sf() +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = g025_df_pie,
    cols = orders_list,
    size = 0.25,
    alpha = 1.5
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
        legend.title = element_blank()) +
  ylab(element_blank()) +
  xlab(element_blank())

plot050_pie_orders <- 
  ggplot(g050_orders_counted) +
  geom_sf() +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = g050_df_pie,
    cols = orders_list,
    size = 0.25,
    alpha = 1.5
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
        legend.title = element_blank()) +
  ylab(element_blank()) +
  xlab(element_blank())

# Saving maps ----
plot025_roden
ggsave('../results/plot025-rodentia.pdf',
       width = 3,
       height = 4)
plot025_prima
ggsave('../results/plot025-primates.pdf',
       width = 3,
       height = 4)
plot025_carni
ggsave('../results/plot025-carnivora.pdf',
       width = 3,
       height = 4)
plot025_didel
ggsave('../results/plot025-didelphimorphia.pdf',
       width = 3,
       height = 4)
plot025_peris
ggsave('../results/plot025-perissodactyla.pdf',
       width = 3,
       height = 4)
plot025_chiro
ggsave('../results/plot025-chiroptera.pdf',
       width = 3,
       height = 4)
plot025_lagom
ggsave('../results/plot025-lagomorpha.pdf',
       width = 3,
       height = 4)
plot025_pilos
ggsave('../results/plot025-pilosa.pdf',
       width = 3,
       height = 4)
plot025_artio
ggsave('../results/plot025-artiodactyla.pdf',
       width = 3,
       height = 4)
plot025_cingu
ggsave('../results/plot025-cingulata.pdf',
       width = 3,
       height = 4)
plot025_siren
ggsave('../results/plot025-sirenia.pdf',
       width = 3,
       height = 4)

plot050_roden
ggsave('../results/plot050-rodentia.pdf',
       width = 3,
       height = 4)
plot050_prima
ggsave('../results/plot050-primates.pdf',
       width = 3,
       height = 4)
plot050_carni
ggsave('../results/plot050-carnivora.pdf',
       width = 3,
       height = 4)
plot050_didel
ggsave('../results/plot050-didelphimorphia.pdf',
       width = 3,
       height = 4)
plot050_peris
ggsave('../results/plot050-perissodactyla.pdf',
       width = 3,
       height = 4)
plot050_chiro
ggsave('../results/plot050-chiroptera.pdf',
       width = 3,
       height = 4)
plot050_lagom
ggsave('../results/plot050-lagomorpha.pdf',
       width = 3,
       height = 4)
plot050_pilos
ggsave('../results/plot050-pilosa.pdf',
       width = 3,
       height = 4)
plot050_artio
ggsave('../results/plot050-artiodactyla.pdf',
       width = 3,
       height = 4)
plot050_cingu
ggsave('../results/plot050-cingulata.pdf',
       width = 3,
       height = 4)
plot050_siren
ggsave('../results/plot050-sirenia.pdf',
       width = 3,
       height = 4)
plot025_pie_orders
ggsave('../results/plot050-pie-oreders.pdf',
       width = 3,
       height = 4)
plot050_pie_orders
ggsave('../results/plot050-pie-orders.pdf',
       width = 3,
       height = 4)