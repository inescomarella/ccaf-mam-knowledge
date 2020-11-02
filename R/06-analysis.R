x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'FNN', 'vegan', 'corrplot', 'ggplot2')
lapply(x, library, character.only = TRUE)

conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'select', winner = 'dplyr')

source('./R/functions.R')

data <-
  st_read(
    './data/data-all-clean.csv',
    options = c(
      'X_POSSIBLE_NAMES=decimalLongitude',
      'Y_POSSIBLE_NAMES=decimalLatitude'
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )
inst_df <-
  read.csv('./data/institutions-ccma.csv', header = T)
g025 <- st_read(dsn = './outputs', layer = 'grid_025_ucs_joined')
g050 <- st_read(dsn = './outputs', layer = 'grid_050_ucs_joined')

# Reorder lat/lon 
inst_layer <- inst_df %>% select(longitude, latitude)

# Convert data.frame do Simple Features
inst_layer <- st_as_sf(inst_layer, coords = c('longitude', 'latitude'))
inst_layer <- st_set_crs(inst_layer, CRS("+proj=longlat +datum=WGS84"))

# Reproject to a metric coordinate system
crs <- CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
data_utm <- st_transform(data, crs)
inst_utm <- st_transform(inst_layer, crs)
g025_utm <- st_transform(g025, crs)
g050_utm <- st_transform(g050, crs)

# Count registers per cell
g025_utm$nreg <- lengths(st_intersects(g025_utm, data_utm))
g050_utm$nreg <- lengths(st_intersects(g050_utm, data_utm))

# Count species per cell
g025_utm <- count.sp.in.polygons(data_utm, g025_utm)
g050_utm <- count.sp.in.polygons(data_utm, g050_utm)

# Getting centroid points
g025_centroid <- coordinates(as(g025_utm, 'Spatial'))
g050_centroid <- coordinates(as(g050_utm, 'Spatial'))
inst_coords <- st_coordinates(inst_utm)

# Centre point distance to the nearest institution
dist_025 <-
  get.knnx(inst_coords, g025_centroid, k = 1)
dist_050 <-
  get.knnx(inst_coords, g050_centroid, k = 1)

# Add distance to grid attribute table
g025_utm$dist_inst <- as.data.frame(dist_025)$nn.dist
g050_utm$dist_inst <- as.data.frame(dist_050)$nn.dist

# Adding centre point to attribute table (to be used as covariate in GLM)
g025_utm$lon <- as.data.frame(g025_centroid)$V1
g025_utm$lat <- as.data.frame(g025_centroid)$V2

g050_utm$lon <- as.data.frame(g050_centroid)$V1
g050_utm$lat <- as.data.frame(g050_centroid)$V2

# Extract dataframe
g025_df <- st_set_geometry(g025_utm, NULL)
g050_df <- st_set_geometry(g050_utm, NULL)

# Selecting the continuous variables
g025_df_selected <-
  g025_df %>% select(countPts, nreg, dist_inst, lon, lat)
g050_df_selected <-
  g050_df %>% select(countPts, nreg, dist_inst, lon, lat)

# Categorical variable - UC presence/absence
g025_df_selected$uc_pres <- !is.na(g025_df$UF_unique)
g050_df_selected$uc_pres <- !is.na(g050_df$UF_unique)

colnames(g025_df_selected) <- c('n_sp', 'n_reg', 'dist_inst', 'lon', 'lat', 'uc_pres')
colnames(g050_df_selected) <- c('n_sp', 'n_reg', 'dist_inst', 'lon', 'lat', 'uc_pres')

# Correlation test 
g025_cor <- cor(g025_df_selected[,-ncol(g025_df_selected)])
g050_cor <- cor(g050_df_selected[,-ncol(g050_df_selected)])

# Plot correlation test
corrplot(
  g025_cor,
  type = "upper",
  method = 'circle',
  tl.col = "black",
  tl.srt = 45,
  order = "hclust"
)
corrplot(
  g050_cor,
  type = "upper",
  method = 'circle',
  tl.col = "black",
  tl.srt = 45,
  order = "hclust"
)

# Pearson's chi-squared test
g025_chisq <- chisq.test(g025_df_selected$n_reg, g025_df_selected$uc_pres)
g050_chisq <- chisq.test(g050_df_selected$n_reg, g050_df_selected$uc_pres)

# Mean polygon area
g025_area <- mean(st_area(g025_utm)) # 484.516.066 m² = 484 km²
g050_area <- mean(st_area(g050_utm)) # 1.646.877.595 m² = 1.646 km²

# Plot maps
ggplot_nreg_025 <-
  ggplot(g025_utm) +
  geom_sf(aes(fill = nreg))
ggplot_nreg_050 <-
  ggplot(g050_utm) + 
  geom_sf(aes(fill = nreg))

ggplot_nsp_025 <-
  ggplot(g025_utm) + 
  geom_sf(aes(fill = countPts))
ggplot_nsp_050 <-
  ggplot(g050_utm) + 
  geom_sf(aes(fill = countPts))

# Edit plots
ggplot_nreg_025_edited <-
  ggplot_nreg_025 +
  geom_sf(aes(fill = nreg), size = 0.25) +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
ggplot_nreg_050_edited <-
  ggplot_nreg_050 +
  geom_sf(aes(fill = nreg), size = 0.25) + 
  labs(fill = "Nº de registros") + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

ggplot_nsp_025_edited <-
  ggplot_nsp_025 +
  geom_sf(aes(fill = countPts), size = 0.25) + 
  labs(fill = "Nº de espécies") + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
ggplot_nsp_050_edited <-
  ggplot_nsp_050 +
  geom_sf(aes(fill = countPts), size = 0.25) + 
  labs(fill = "Nº de espécies") + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))

# Save maps
ggplot_nreg_025_edited
ggsave('../results/plot_nreg_25.pdf', width = 3, height = 4)
ggplot_nreg_050_edited
ggsave('../results/plot_nreg_50.pdf', width = 3, height = 4)

ggplot_nsp_025_edited
ggsave('../results/plot_nsp_25.pdf', width = 3, height = 4)
ggplot_nsp_050_edited
ggsave('../results/plot_nsp_50.pdf', width = 3, height = 4)


# To do #############################

brasil_sf <- st_as_sf(brasil_spT)
st_agr(brasil_sf) = "constant"
brasil_cropped <- st_crop(brasil_sf, xmin = 201620, xmax = 539295.8,
                                    ymin = -2356808, ymax = -1438805)
ggplot(brasil_cropped) + geom_sf() + geom_sf(data = grid_025_sp_counted, aes(fill = countPts)) + theme_bw()

