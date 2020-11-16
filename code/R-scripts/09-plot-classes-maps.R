setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <-
  c("tidyverse", "rocc", "rgbif", "plyr", "sf", 'dplyr', 'raster')
lapply(x, library, character.only = TRUE)

source('functions.R')

# Inputs ----
g025 <- st_read(dsn = '../outputs', layer = 'grid_025_ucs_joined')
mamm_data <- st_read('../data/mamm-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
aves_data <- st_read('../data/aves-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
amph_data <- st_read('../data/amph-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
rept_data <- st_read('../data/rept-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
inse_data <- st_read('../data/inse-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))
arac_data <- st_read('../data/arac-data.csv', options = c('X_POSSIBLE_NAMES=decimalLongitude', 'Y_POSSIBLE_NAMES=decimalLatitude'), crs = CRS("+proj=longlat +datum=WGS84"))

# Reproject
g025_crs <- st_transform(g025, crs = st_crs(arac_data))

# Count species -----
g025_mamm <- count.scientificName.in.polygons(mamm_data, g025_crs)
g025_aves <- count.scientificName.in.polygons(aves_data, g025_crs)
g025_amph <- count.scientificName.in.polygons(amph_data, g025_crs)
g025_rept <- count.scientificName.in.polygons(rept_data, g025_crs)
g025_inse <- count.scientificName.in.polygons(inse_data, g025_crs)
g025_arac <- count.scientificName.in.polygons(arac_data, g025_crs)

# Count records -----
g025_mamm$nreg <- lengths(st_intersects(g025_mamm, mamm_data))
g025_aves$nreg <- lengths(st_intersects(g025_aves, aves_data))
g025_amph$nreg <- lengths(st_intersects(g025_amph, amph_data))
g025_rept$nreg <- lengths(st_intersects(g025_rept, rept_data))
g025_inse$nreg <- lengths(st_intersects(g025_inse, inse_data))
g025_arac$nreg <- lengths(st_intersects(g025_arac, arac_data))

# Customized theme ----
customPlot = list(
  theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title =  element_text(size = 8),
      legend.text = element_text(size = 8)
    )
)

# Plot number of species -----
mamm_nsp <-
  ggplot(g025_aves) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Mammalia') +
  labs(fill = "Number of \n species recorded") + 
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

aves_nsp <-
  ggplot(g025_aves) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Aves') +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

amph_nsp <-
  ggplot(g025_amph) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Amphibia') +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

rept_nsp <-
  ggplot(g025_rept) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Reptilia') +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

inse_nsp <-
  ggplot(g025_inse) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Insecta') +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

arac_nsp <-
  ggplot(g025_aves) + 
  geom_sf(aes(fill = countPts), size = 0.2) + 
  ggtitle('Arachnida') +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot number of records ------
mamm_nreg <-
  ggplot(g025_mamm) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Mammalia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

aves_nreg <-
  ggplot(g025_aves) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Aves') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

amph_nreg <-
  ggplot(g025_amph) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Amphibia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

rept_nreg <-
  ggplot(g025_rept) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Reptilia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

inse_nreg <-
  ggplot(g025_inse) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Insecta') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

arac_nreg <-
  ggplot(g025_arac) + 
  geom_sf(aes(fill = nreg), size = 0.2) + 
  ggtitle('Arachnida') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Save number of species maps ----
mamm_nsp
ggsave('../results/class-mamm-nsp.pdf',
       width = 3,
       height = 4)
aves_nsp
ggsave('../results/class-aves-nsp.pdf',
       width = 3,
       height = 4)
amph_nsp
ggsave('../results/class-amph-nsp.pdf',
       width = 3,
       height = 4)
rept_nsp
ggsave('../results/class-rept-nsp.pdf',
       width = 3,
       height = 4)
inse_nsp
ggsave('../results/class-inse-nsp.pdf',
       width = 3,
       height = 4)
arac_nsp
ggsave('../results/class-arac-nsp.pdf',
       width = 3,
       height = 4)

# Save number of records maps -----
mamm_nreg
ggsave('../results/class-mamm-nreg.pdf',
       width = 3,
       height = 4)
aves_nreg
ggsave('../results/class-aves-nreg.pdf',
       width = 3,
       height = 4)
amph_nreg
ggsave('../results/class-amph-nreg.pdf',
       width = 3,
       height = 4)
rept_nreg
ggsave('../results/class-rept-nreg.pdf',
       width = 3,
       height = 4)
inse_nreg
ggsave('../results/class-inse-nreg.pdf',
       width = 3,
       height = 4)
arac_nreg
ggsave('../results/class-arac-nreg.pdf',
       width = 3,
       height = 4)
