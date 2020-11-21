# File purpose: Plot and save classes maps
# Date: 20/11/2020

# Load in libraries
x <-
  c("tidyverse", "rocc", "rgbif", "plyr", "sf", "dplyr", "sp")
lapply(x, library, character.only = TRUE)

source("./R-scripts/functions/06-funs-plot-classes-maps.R")

# Load in data
g025 <-
  st_read(dsn = "../data/processed-data", layer = "grid-025-ucs-joined")
mamm_data <-
  st_read(
    "../data/processed-data/clean-mamm-data-gbif-spLk.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
aves_data <-
  st_read(
    "../data/processed-data/clean-aves-data.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
amph_data <-
  st_read(
    "../data/processed-data/clean-amph-data.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
rept_data <-
  st_read(
    "../data/processed-data/clean-rept-data.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
inse_data <-
  st_read(
    "../data/processed-data/clean-inse-data.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
arac_data <-
  st_read(
    "../data/processed-data/clean-arac-data.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )

# Prepare data ---------------------------------------------------------------

# Reproject
g025 <- st_transform(g025, crs = CRS("+proj=longlat +datum=WGS84"))

# Count species
g025_mamm <- count.sp.in.polygons(mamm_data, g025)
g025_aves <- count.sp.in.polygons(aves_data, g025)
g025_amph <- count.sp.in.polygons(amph_data, g025)
g025_rept <- count.sp.in.polygons(rept_data, g025)
g025_inse <- count.sp.in.polygons(inse_data, g025)
g025_arac <- count.sp.in.polygons(arac_data, g025)

# Count records
g025_mamm$nreg <- lengths(st_intersects(g025_mamm, mamm_data))
g025_aves$nreg <- lengths(st_intersects(g025_aves, aves_data))
g025_amph$nreg <- lengths(st_intersects(g025_amph, amph_data))
g025_rept$nreg <- lengths(st_intersects(g025_rept, rept_data))
g025_inse$nreg <- lengths(st_intersects(g025_inse, inse_data))
g025_arac$nreg <- lengths(st_intersects(g025_arac, arac_data))

# Customized theme
customPlot <- list(
  theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title =  element_text(size = 8),
      legend.text = element_text(size = 8)
    )
)

# Plot maps -----------------------------------------------------------------

# Plot number of species
mamm_nsp <-
  ggplot(g025_aves) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Mammalia") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

aves_nsp <-
  ggplot(g025_aves) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Aves") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

amph_nsp <-
  ggplot(g025_amph) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Amphibia") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

rept_nsp <-
  ggplot(g025_rept) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Reptilia") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

inse_nsp <-
  ggplot(g025_inse) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Insecta") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

arac_nsp <-
  ggplot(g025_aves) +
  geom_sf(aes(fill = nsp), size = 0.2) +
  ggtitle("Arachnida") +
  labs(fill = "Number of \n species recorded") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot number of records
mamm_nreg <-
  ggplot(g025_mamm) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Mammalia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

aves_nreg <-
  ggplot(g025_aves) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Aves") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

amph_nreg <-
  ggplot(g025_amph) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Amphibia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

rept_nreg <-
  ggplot(g025_rept) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Reptilia") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

inse_nreg <-
  ggplot(g025_inse) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Insecta") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

arac_nreg <-
  ggplot(g025_arac) +
  geom_sf(aes(fill = nreg), size = 0.2) +
  ggtitle("Arachnida") +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Save maps -----------------------------------------------------------------

mamm_nsp
ggsave("../data/results/map-class-mamm-nsp.pdf",
       width = 3,
       height = 4)
aves_nsp
ggsave("../data/results/map-class-aves-nsp.pdf",
       width = 3,
       height = 4)
amph_nsp
ggsave("../data/results/map-class-amph-nsp.pdf",
       width = 3,
       height = 4)
rept_nsp
ggsave("../data/results/map-class-rept-nsp.pdf",
       width = 3,
       height = 4)
inse_nsp
ggsave("../data/results/map-class-inse-nsp.pdf",
       width = 3,
       height = 4)
arac_nsp
ggsave("../data/results/map-class-arac-nsp.pdf",
       width = 3,
       height = 4)

# Number of records
mamm_nreg
ggsave("../data/results/map-class-mamm-nreg.pdf",
       width = 3,
       height = 4)
aves_nreg
ggsave("../data/results/map-class-aves-nreg.pdf",
       width = 3,
       height = 4)
amph_nreg
ggsave("../data/results/map-class-amph-nreg.pdf",
       width = 3,
       height = 4)
rept_nreg
ggsave("../data/results/map-class-rept-nreg.pdf",
       width = 3,
       height = 4)
inse_nreg
ggsave("../data/results/map-class-inse-nreg.pdf",
       width = 3,
       height = 4)
arac_nreg
ggsave("../data/results/map-class-arac-nreg.pdf",
       width = 3,
       height = 4)

