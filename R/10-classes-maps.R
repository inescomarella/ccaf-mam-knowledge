setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <-
  c("tidyverse", "rocc", "rgbif", "plyr", "sf", 'dplyr', 'raster')
lapply(x, library, character.only = TRUE)

source('functions.R')

# Inputs
g050 <- st_read(dsn = '../outputs', layer = 'grid_050_ucs_joined')
mamm_data <- st_read('../data/mamm-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
aves_data <- st_read('../data/aves-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
amph_data <- st_read('../data/amph-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
rept_data <- st_read('../data/rept-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
inse_data <- st_read('../data/inse-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
arac_data <- st_read('../data/arac-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))

# Reproject
g050_crs <- st_transform(g050, crs = st_crs(arac_data))

# Count species per cell
g050_mamm <- count.scientificName.in.polygons(mamm_data, g050_crs)
g050_aves <- count.scientificName.in.polygons(aves_data, g050_crs)
g050_amph <- count.scientificName.in.polygons(amph_data, g050_crs)
g050_rept <- count.scientificName.in.polygons(rept_data, g050_crs)
g050_inse <- count.scientificName.in.polygons(inse_data, g050_crs)
g050_arac <- count.scientificName.in.polygons(arac_data, g050_crs)

# Count registers per cell
g050_mamm$nreg <- lengths(st_intersects(g050_mamm, mamm_data))
g050_aves$nreg <- lengths(st_intersects(g050_aves, aves_data))
g050_amph$nreg <- lengths(st_intersects(g050_amph, amph_data))
g050_rept$nreg <- lengths(st_intersects(g050_rept, rept_data))
g050_inse$nreg <- lengths(st_intersects(g050_inse, inse_data))
g050_arac$nreg <- lengths(st_intersects(g050_arac, arac_data))

mamm_nsp <-
  ggplot(g050_aves) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Mammalia') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
aves_nsp <-
  ggplot(g050_aves) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Aves') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
amph_nsp <-
  ggplot(g050_amph) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Amphibia') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
rept_nsp <-
  ggplot(g050_rept) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Reptilia') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
inse_nsp <-
  ggplot(g050_inse) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Insecta') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
arac_nsp <-
  ggplot(g050_aves) + 
  geom_sf(aes(fill = countPts), size = 0.25) + 
  ggtitle('Arachnida') +
  labs(fill = "Species number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

mamm_nreg <-
  ggplot(g050_mamm) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Aves') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
aves_nreg <-
  ggplot(g050_aves) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Aves') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
amph_nreg <-
  ggplot(g050_amph) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Amphibia') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
rept_nreg <-
  ggplot(g050_rept) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Reptilia') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
inse_nreg <-
  ggplot(g050_inse) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Insect') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
arac_nreg <-
  ggplot(g050_arac) + 
  geom_sf(aes(fill = nreg), size = 0.25) + 
  ggtitle('Arachnida') +
  labs(fill = "Registers number") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

mamm_nsp
aves_nsp
amph_nsp
rept_nsp
inse_nsp
arac_nsp
aves_nsp
mamm_nreg
amph_nreg
rept_nreg
inse_nreg
arac_nreg
