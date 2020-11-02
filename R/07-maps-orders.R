x <- c('dplyr', 'sf', 'raster', 'ggplot2')
lapply(x, library, character.only = TRUE)

source('functions.R')

# Inputs
data <- st_read('../data/data-all-clean.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
g025 <- st_read(dsn = '../outputs', layer = 'grid_025_ucs_joined')
g050 <- st_read(dsn = '../outputs', layer = 'grid_050_ucs_joined')

# Reproject
g025_crs <- st_transform(g025, crs = st_crs(data))
g050_crs <- st_transform(g050, crs = st_crs(data))

g025_roden <- count.order.in.polygons(data, g025_crs, 'Rodentia')
g025_prima <- count.order.in.polygons(data, g025_crs, 'Primates')
g025_carni <- count.order.in.polygons(data, g025_crs, 'Carnivora')
g025_didel <- count.order.in.polygons(data, g025_crs, 'Didelphimorphia')
g025_peris <- count.order.in.polygons(data, g025_crs, 'Perissodactyla')
g025_chiro <- count.order.in.polygons(data, g025_crs, 'Chiroptera')
g025_lagom <- count.order.in.polygons(data, g025_crs, 'Lagomorpha')
g025_pilos <- count.order.in.polygons(data, g025_crs, 'Pilosa')
g025_artio <- count.order.in.polygons(data, g025_crs, 'Artiodactyla')
g025_cingu <- count.order.in.polygons(data, g025_crs, 'Cingulata')
g025_siren <- count.order.in.polygons(data, g025_crs, 'Sirenia')

g050_roden <- count.order.in.polygons(data, g050_crs, 'Rodentia')
g050_prima <- count.order.in.polygons(data, g050_crs, 'Primates')
g050_carni <- count.order.in.polygons(data, g050_crs, 'Carnivora')
g050_didel <- count.order.in.polygons(data, g050_crs, 'Didelphimorphia')
g050_peris <- count.order.in.polygons(data, g050_crs, 'Perissodactyla')
g050_chiro <- count.order.in.polygons(data, g050_crs, 'Chiroptera')
g050_lagom <- count.order.in.polygons(data, g050_crs, 'Lagomorpha')
g050_pilos <- count.order.in.polygons(data, g050_crs, 'Pilosa')
g050_artio <- count.order.in.polygons(data, g050_crs, 'Artiodactyla')
g050_cingu <- count.order.in.polygons(data, g050_crs, 'Cingulata')
g050_siren <- count.order.in.polygons(data, g050_crs, 'Sirenia')

plot025_roden <- ggplot(g025_roden) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Rodentia')
plot025_prima <- ggplot(g025_prima) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Primates')
plot025_carni <- ggplot(g025_carni) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Carnivora')
plot025_didel <- ggplot(g025_didel) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Didelphimorphia')
plot025_peris <- ggplot(g025_peris) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Perissodactyla')
plot025_chiro <- ggplot(g025_chiro) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Chiroptera')
plot025_lagom <- ggplot(g025_lagom) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Lagomorpha')
plot025_pilos <- ggplot(g025_pilos) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Pilosa')
plot025_artio <- ggplot(g025_artio) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Artiodactyla')
plot025_cingu <- ggplot(g025_cingu) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Cingulata')
plot025_siren <- ggplot(g025_siren) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Sirenia')

plot050_roden <- ggplot(g050_roden) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Rodentia')
plot050_prima <- ggplot(g050_prima) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Primates')
plot050_carni <- ggplot(g050_carni) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Carnivora')
plot050_didel <- ggplot(g050_didel) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Didelphimorphia')
plot050_peris <- ggplot(g050_peris) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Perissodactyla')
plot050_chiro <- ggplot(g050_chiro) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Chiroptera')
plot050_lagom <- ggplot(g050_lagom) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Lagomorpha')
plot050_pilos <- ggplot(g050_pilos) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Pilosa')
plot050_artio <- ggplot(g050_artio) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Artiodactyla')
plot050_cingu <- ggplot(g050_cingu) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Cingulata')
plot050_siren <- ggplot(g050_siren) + geom_sf(aes(fill = countPts), size = 0.25) + ggtitle('Sirenia')

plot025_roden
ggsave('../results/plot025_rodentia.pdf', width = 3, height = 4)
plot025_prima
ggsave('../results/plot025_primates.pdf', width = 3, height = 4)
plot025_carni
ggsave('../results/plot025_carnivora.pdf', width = 3, height = 4)
plot025_didel
ggsave('../results/plot025_didelphimorphia.pdf', width = 3, height = 4)
plot025_peris
ggsave('../results/plot025_perissodactyla.pdf', width = 3, height = 4)
plot025_chiro
ggsave('../results/plot025_chiroptera.pdf', width = 3, height = 4)
plot025_lagom
ggsave('../results/plot025_lagomorpha.pdf', width = 3, height = 4)
plot025_pilos
ggsave('../results/plot025_pilosa.pdf', width = 3, height = 4)
plot025_artio
ggsave('../results/plot025_artiodactyla.pdf', width = 3, height = 4)
plot025_cingu
ggsave('../results/plot025_cingulara.pdf', width = 3, height = 4)
plot025_siren
ggsave('../results/plot025_sirenia.pdf', width = 3, height = 4)

plot050_roden
ggsave('../results/plot050_rodentia.pdf', width = 3, height = 4)
plot050_prima
ggsave('../results/plot050_primates.pdf', width = 3, height = 4)
plot050_carni
ggsave('../results/plot050_carnivora.pdf', width = 3, height = 4)
plot050_didel
ggsave('../results/plot050_didelphimorphia.pdf', width = 3, height = 4)
plot050_peris
ggsave('../results/plot050_perissodactyla.pdf', width = 3, height = 4)
plot050_chiro
ggsave('../results/plot050_chiroptera.pdf', width = 3, height = 4)
plot050_lagom
ggsave('../results/plot050_lagomorpha.pdf', width = 3, height = 4)
plot050_pilos
ggsave('../results/plot050_pilosa.pdf', width = 3, height = 4)
plot050_artio
ggsave('../results/plot050_artiodactyla.pdf', width = 3, height = 4)
plot050_cingu
ggsave('../results/plot050_cingulara.pdf', width = 3, height = 4)
plot050_siren
ggsave('../results/plot050_sirenia.pdf', width = 3, height = 4)

