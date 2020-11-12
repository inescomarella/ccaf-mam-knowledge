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
municipalities <-
  st_read("../outputs/backup/municipios_joined.shp",
          crs = CRS("+proj=longlat +datum=WGS84"))
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

# Count records ----
# All mammals - grid
g025_mamm_all <- g025
g050_mamm_all <- g050

g025_mamm_all$n_reg <- lengths(st_intersects(g025, data))
g050_mamm_all$n_reg <- lengths(st_intersects(g050, data))

# All mammals - municipalities
municipalities$n_reg <- lengths(st_intersects(municipalities, data))

# Mammal orders - grid
orders_list <- unique(data$order)
g025_mamm_orders <- count.orders.list.in.polygons(data, g025, orders_list)
g050_mamm_orders <- count.orders.list.in.polygons(data, g050, orders_list)

# Count species ----
g025_mamm_all <- count.sp.in.polygons(data, g025_mamm_all)
g050_mamm_all <- count.sp.in.polygons(data, g050_mamm_all)

# Pie chart data.frame ----
g025_coord <- as.data.frame(coordinates(as(g025_mamm_orders, 'Spatial')))
g050_coord <- as.data.frame(coordinates(as(g050_mamm_orders, 'Spatial')))

g025_nreg <- lengths(st_intersects(g025_mamm_orders, data))
g050_nreg <- lengths(st_intersects(g050_mamm_orders, data))

g025_df_pie <- bind_cols(g025_coord, g025_mamm_orders[83:93])
g050_df_pie <- bind_cols(g050_coord, g050_mamm_orders[83:93])

g025_df_pie$radius <- g025_nreg/(max(g025_nreg)*2)
g050_df_pie$radius <- g050_nreg/(max(g050_nreg)*2)

# Customized theme ----
customPlot = list(
  theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title =  element_text(size = 8),
      legend.text = element_text(size = 8)
    )
)

# Plot mammal maps -----
plot_nreg_025_mamm_all <-
  ggplot(g025_mamm_all) +
  geom_sf(aes(fill = n_reg), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_nreg_050_mamm_all <-
  ggplot(g050_mamm_all) +
  geom_sf(aes(fill = n_reg), size = 0.2) +
  labs(fill = "Number of \n mammal records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_nsp_025_mamm_all<-
  ggplot(g025_mamm_all) +
  geom_sf(aes(fill = countPts), size = 0.2) + 
  labs(fill = "Number of mammal \n species recorded") + 
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_nsp_050_mamm_all <-
  ggplot(g050_mamm_all) +
  geom_sf(aes(fill = countPts), size = 0.2) + 
  labs(fill = "Number of mammal \n species recorded") + 
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot orders maps ----
plot_025_roden <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Rodentia), size = 0.2) +
  ggtitle('Rodentia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_prima <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Primates), size = 0.2) +
  ggtitle('Primates') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_carni <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Carnivora), size = 0.2) +
  ggtitle('Carnivora') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_didel <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.2) +
  ggtitle('Didelphimorphia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_peris <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Perissodactyla), size = 0.2) +
  ggtitle('Perissodactyla') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_chiro <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Chiroptera), size = 0.2) +
  ggtitle('Chiroptera') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_lagom <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Lagomorpha), size = 0.2) +
  ggtitle('Lagomorpha') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_pilos <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Pilosa), size = 0.2) +
  ggtitle('Pilosa') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_artio <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Artiodactyla), size = 0.2) +
  ggtitle('Artiodactyla') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_cingu <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Cingulata), size = 0.2) +
  ggtitle('Cingulata') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_025_siren <-
  ggplot(g025_mamm_orders) +
  geom_sf(aes(fill = Sirenia), size = 0.2) +
  ggtitle('Sirenia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_roden <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Rodentia), size = 0.2) +
  ggtitle('Rodentia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_prima <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Primates), size = 0.2) +
  ggtitle('Primates') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_carni <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Carnivora), size = 0.2) +
  ggtitle('Carnivora') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_didel <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Didelphimorphia), size = 0.2) +
  ggtitle('Didelphimorphia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_peris <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Perissodactyla), size = 0.2) +
  ggtitle('Perissodactyla') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_chiro <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Chiroptera), size = 0.2) +
  ggtitle('Chiroptera') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_lagom <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Lagomorpha), size = 0.2) +
  ggtitle('Lagomorpha') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_pilos <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Pilosa), size = 0.2) +
  ggtitle('Pilosa') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_artio <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Artiodactyla), size = 0.2) +
  ggtitle('Artiodactyla') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_cingu <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Cingulata), size = 0.2) +
  ggtitle('Cingulata') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

plot_050_siren <-
  ggplot(g050_mamm_orders) +
  geom_sf(aes(fill = Sirenia), size = 0.2) +
  ggtitle('Sirenia') +
  labs(fill = "Number of \n records") +
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot

# Plot municipalities ----
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
  geom_sf(aes(fill = n_reg), size = 0.2) +   
  labs(fill = "Number of \n mammal records") +   
  scale_fill_viridis_b(show.limits = TRUE) +
  customPlot


# Plot pie chart maps -----
plot_025_pie_orders <-
  ggplot(g025_mamm_orders) +
  geom_sf(size = 0.2) +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = g025_df_pie,
    cols = orders_list,
    size = 0.2,
    alpha = 1.5
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.height=unit(0.4, "cm")) +
  ylab(element_blank()) +
  xlab(element_blank())

plot_050_pie_orders <- 
  ggplot(g050_mamm_orders) +
  geom_sf(size = 0.2) +
  geom_scatterpie(
    aes(x = V1, y = V2, r = radius),
    data = g050_df_pie,
    cols = orders_list,
    size = 0.2,
    alpha = 1.5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.height=unit(0.4, "cm")) +
  ylab(element_blank()) +
  xlab(element_blank())

# Save mammal maps ----
plot_nreg_025_mamm_all
ggsave('../results/plot-nreg-25.pdf',
       width = 3,
       height = 4)

plot_nreg_050_mamm_all
ggsave('../results/plot-nreg-50.pdf',
       width = 3,
       height = 4)

plot_nsp_025_mamm_all
ggsave('../results/plot-nsp-25.pdf',
       width = 3,
       height = 4)

plot_nsp_050_mamm_all
ggsave('../results/plot-nsp-50.pdf',
       width = 3,
       height = 4)

# Save orders maps ----
plot_025_roden
ggsave('../results/plot025-rodentia.pdf',
       width = 3,
       height = 4)
plot_025_prima
ggsave('../results/plot025-primates.pdf',
       width = 3,
       height = 4)
plot_025_carni
ggsave('../results/plot025-carnivora.pdf',
       width = 3,
       height = 4)
plot_025_didel
ggsave('../results/plot025-didelphimorphia.pdf',
       width = 3,
       height = 4)
plot_025_peris
ggsave('../results/plot025-perissodactyla.pdf',
       width = 3,
       height = 4)
plot_025_chiro
ggsave('../results/plot025-chiroptera.pdf',
       width = 3,
       height = 4)
plot_025_lagom
ggsave('../results/plot025-lagomorpha.pdf',
       width = 3,
       height = 4)
plot_025_pilos
ggsave('../results/plot025-pilosa.pdf',
       width = 3,
       height = 4)
plot_025_artio
ggsave('../results/plot025-artiodactyla.pdf',
       width = 3,
       height = 4)
plot_025_cingu
ggsave('../results/plot025-cingulata.pdf',
       width = 3,
       height = 4)
plot_025_siren
ggsave('../results/plot025-sirenia.pdf',
       width = 3,
       height = 4)

plot_050_roden
ggsave('../results/plot050-rodentia.pdf',
       width = 3,
       height = 4)
plot_050_prima
ggsave('../results/plot050-primates.pdf',
       width = 3,
       height = 4)
plot_050_carni
ggsave('../results/plot050-carnivora.pdf',
       width = 3,
       height = 4)
plot_050_didel
ggsave('../results/plot050-didelphimorphia.pdf',
       width = 3,
       height = 4)
plot_050_peris
ggsave('../results/plot050-perissodactyla.pdf',
       width = 3,
       height = 4)
plot_050_chiro
ggsave('../results/plot050-chiroptera.pdf',
       width = 3,
       height = 4)
plot_050_lagom
ggsave('../results/plot050-lagomorpha.pdf',
       width = 3,
       height = 4)
plot_050_pilos
ggsave('../results/plot050-pilosa.pdf',
       width = 3,
       height = 4)
plot_050_artio
ggsave('../results/plot050-artiodactyla.pdf',
       width = 3,
       height = 4)
plot_050_cingu
ggsave('../results/plot050-cingulata.pdf',
       width = 3,
       height = 4)
plot_050_siren
ggsave('../results/plot050-sirenia.pdf',
       width = 3,
       height = 4)

# Save municipalities -----
plot_nreg_municipalities
ggsave('../results/plot_nreg_municipalities.pdf',
       width = 3,
       height = 4)

# Save pie chart maps ----
plot_025_pie_orders
ggsave('../results/plot025-pie-orders.pdf',
       width = 3,
       height = 4)

plot_050_pie_orders
ggsave('../results/plot050-pie-orders.pdf',
       width = 3,
       height = 4)

# To do #############################

brasil_sf <- st_as_sf(brasil_spT)
st_agr(brasil_sf) = "constant"
brasil_cropped <- st_crop(brasil_sf, xmin = 201620, xmax = 539295.8,
                          ymin = -2356808, ymax = -1438805)
ggplot(brasil_cropped) + geom_sf() + geom_sf(data = grid_025_sp_counted, aes(fill = countPts)) + theme_bw()

