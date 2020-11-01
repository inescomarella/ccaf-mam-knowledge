x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'GISTools', 'FNN', 'vegan', 'corrplot', 'ggplot2')
lapply(x, library, character.only = TRUE)

conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'select', winner = 'dplyr')

count.sp.in.polygons <- function(pts, polygons){
  pts <- st_as_sf(pts)
  polygons <- st_as_sf(polygons)
  st_agr(polygons) = "constant"
  st_agr(pts) = "constant"
  countPts = c()
  for (i in 1:nrow(polygons)) {
    polySelect <- polygons[i,]
    pts2 <- st_intersection(pts, polySelect)
    countPts[i] = length(unique(pts2$species))
    
  }
  
  return(cbind(polygons,countPts))
}

data <- read.csv('data-all-clean.csv')
inst_df <-
  read.csv('institutions-ccma.csv', header = T)
grid_025 <- readOGR(dsn = '../outputs', layer = 'grid_025_ucs_joined')
grid_050 <- readOGR(dsn = '../outputs', layer = 'grid_050_ucs_joined')
brasil <- readOGR(dsn = '../maps/IBGE/br_unidades_da_federacao', layer = 'BRUFE250GC_SIR')

to_remove <- data %>% filter(is.na(decimalLongitude))
data <- anti_join(data, to_remove)

# Reorder lat/lon 
inst_lat <- inst_df$latitude
inst_lon <- inst_df$longitude
inst_coord <- bind_cols(inst_lon, inst_lat)
inst_layer <- inst_coord

data_layer <- data %>% select(decimalLongitude, decimalLatitude, eventYear, order, family, species)

# Convert dataframe do SpatialPoints
inst_layer <-
  SpatialPoints(inst_coord, proj4string = CRS("+proj=longlat +datum=WGS84"))

coordinates(data_layer) <- ~decimalLongitude+decimalLatitude
proj4string(data_layer) <- CRS("+proj=longlat +datum=WGS84")

# Reproject to a metric coordinate system
crs <- CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
data_points_spT <- spTransform(data_layer, crs)
inst_points_spT <- spTransform(inst_layer, crs)
grid_025_spT <- spTransform(grid_025, crs)
grid_050_spT <- spTransform(grid_050, crs)
brasil_spT <- spTransform(brasil, crs)

# Clean points
data_clipped <- intersect(data_points_spT, grid_050_spT)
data_clipped_spT <- spTransform(data_clipped, crs)

# Count registers per cell
grid_025_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_025_spT)
grid_050_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_050_spT)

# Count species per cell
grid_025_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_025_spT)
grid_050_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_050_spT)

# Converting objects from sf to SpatialPolygonDataFrame to get centre point using coordinates()
grid_025_sp_counted_SPDF <- as(grid_025_sp_counted, 'Spatial')
grid_050_sp_counted_SPDF <- as(grid_050_sp_counted, 'Spatial')

# Grid centre point distance to nearest the institution
dist_knnx_025 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_025_sp_counted_SPDF), k = 1)
dist_knnx_050 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_050_sp_counted_SPDF), k = 1)

# Add distance to grid attribute table
grid_025_sp_counted$dist_inst <- as.data.frame(dist_knnx_025)$nn.dist
grid_050_sp_counted$dist_inst <- as.data.frame(dist_knnx_050)$nn.dist

# Adding centre point to attribute table (to be used as covariate in GLM)
grid_025_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_025_sp_counted_SPDF))$V1
grid_025_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_025_sp_counted_SPDF))$V2

grid_050_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_050_sp_counted_SPDF))$V1
grid_050_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_050_sp_counted_SPDF))$V2

# Extract dataframe
grid_025_df <- st_set_geometry(grid_025_sp_counted, NULL)
grid_050_df <- st_set_geometry(grid_050_sp_counted, NULL)

# Selecting the continuous variables
grid_025_df_seleted <-
  grid_025_df %>% select(countPts, n_reg, dist_inst, centre_point_lon, centre_point_lat)
grid_050_df_seleted <-
  grid_050_df %>% select(countPts, n_reg, dist_inst, centre_point_lon, centre_point_lat)

# Categorical variable - UC presence/absence
grid_025_df_seleted$uc_pres <- !is.na(grid_025_df$UF_unique)
grid_050_df_seleted$uc_pres <- !is.na(grid_050_df$UF_unique)

colnames(grid_025_df_seleted) <- c('n_species', 'n_registers', 'dist_inst', 'lon', 'lat', 'uc_pres')
colnames(grid_050_df_seleted) <- c('n_species', 'n_registers', 'dist_inst', 'lon', 'lat', 'uc_pres')

# Correlation test 
grid_025_cor <- cor(grid_025_df_seleted[,-ncol(grid_025_df_seleted)])
grid_050_cor <- cor(grid_050_df_seleted[,-ncol(grid_050_df_seleted)])

# Plot correlation test
corrplot(grid_025_cor, type="upper", method = 'circle', tl.col="black", tl.srt=45, order="hclust")
corrplot(grid_050_cor, type="upper", method = 'circle', tl.col="black", tl.srt=45, order="hclust")

# Pearson's chi-squared test
grid_025_chisq <- chisq.test(grid_025_df_seleted$n_registers, grid_025_df_seleted$uc_pres)
grid_050_chisq <- chisq.test(grid_050_df_seleted$n_registers, grid_050_df_seleted$uc_pres)

# Mean polygon area
grid_025_area <- mean(st_area(grid_025_sp_counted)) # 484.516.066 m² = 484 km²
grid_050_area <- mean(st_area(grid_050_sp_counted)) # 1.646.877.595 m² = 1.646 km²

# Plot maps
ggplot_nreg_025 <-
  ggplot(grid_025_sp_counted) +
  geom_sf(aes(fill = n_reg))
ggplot_nreg_050 <-
  ggplot(grid_050_sp_counted) + 
  geom_sf(aes(fill = n_reg))

ggplot_nsp_025 <-
  ggplot(grid_025_sp_counted) + 
  geom_sf(aes(fill = countPts))
ggplot_nsp_050 <-
  ggplot(grid_050_sp_counted) + 
  geom_sf(aes(fill = countPts))

# Edit plots
ggplot_nreg_025_edited <-
  ggplot_nreg_025 +
  geom_sf(aes(fill = n_reg), size = 0.25) +
  labs(fill = "Nº de registros") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75))
ggplot_nreg_050_edited <-
  ggplot_nreg_050 +
  geom_sf(aes(fill = n_reg), size = 0.25) + 
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

