x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'GISTools', 'FNN', 'vegan', 'MASS', 'DHARMa')
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

data <- read.csv('./data/data-all-clean.csv')
grid_015 <- readOGR(dsn = './outputs', layer = 'grid_015_ucs_joined')
grid_020 <- readOGR(dsn = './outputs', layer = 'grid_020_ucs_joined')
grid_025 <- readOGR(dsn = './outputs', layer = 'grid_025_ucs_joined')
grid_050 <- readOGR(dsn = './outputs', layer = 'grid_050_ucs_joined')
inst_df <-
  read.csv('./data/institutions-ccma.csv', header = T) # Institutions

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
grid_015_spT <- spTransform(grid_015, crs)
grid_020_spT <- spTransform(grid_020, crs)
grid_025_spT <- spTransform(grid_025, crs)
grid_050_spT <- spTransform(grid_050, crs)

# Clean points
data_clipped <- intersect(data_points_spT, grid_015_spT)
data_clipped_spT <- spTransform(data_clipped, crs)

# Count registers per cell
grid_015_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_015_spT)
grid_020_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_020_spT)
grid_025_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_025_spT)
grid_050_spT@data$n_reg <- poly.counts(data_clipped_spT, grid_050_spT)

# Count species per cell
grid_015_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_015_spT)
grid_020_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_020_spT)
grid_025_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_025_spT)
grid_050_sp_counted <- count.sp.in.polygons(data_clipped_spT, grid_050_spT)

# Converting objects from sf to SpatialPolygonDataFrame to get centre point using coordinates()
grid_015_sp_counted_SPDF <- as(grid_015_sp_counted, 'Spatial')
grid_020_sp_counted_SPDF <- as(grid_020_sp_counted, 'Spatial')
grid_025_sp_counted_SPDF <- as(grid_025_sp_counted, 'Spatial')
grid_050_sp_counted_SPDF <- as(grid_050_sp_counted, 'Spatial')

# Grid centre point distance to nearest the institution
dist_knnx_015 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_015_sp_counted_SPDF), k = 1)
dist_knnx_020 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_020_sp_counted_SPDF), k = 1)
dist_knnx_025 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_025_sp_counted_SPDF), k = 1)
dist_knnx_050 <-
  get.knnx(coordinates(inst_points_spT), coordinates(grid_050_sp_counted_SPDF), k = 1)

# Add distance to grid attribute table
grid_015_sp_counted$dist_inst <- as.data.frame(dist_knnx_015)$nn.dist
grid_020_sp_counted$dist_inst <- as.data.frame(dist_knnx_020)$nn.dist
grid_025_sp_counted$dist_inst <- as.data.frame(dist_knnx_025)$nn.dist
grid_050_sp_counted$dist_inst <- as.data.frame(dist_knnx_050)$nn.dist

# Adding centre point to attribute table (to be used as covariate in GLM)
grid_015_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_015_sp_counted_SPDF))$V1
grid_015_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_015_sp_counted_SPDF))$V2

grid_020_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_020_sp_counted_SPDF))$V1
grid_020_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_020_sp_counted_SPDF))$V2

grid_025_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_025_sp_counted_SPDF))$V1
grid_025_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_025_sp_counted_SPDF))$V2

grid_050_sp_counted$centre_point_lon <- as.data.frame(coordinates(grid_050_sp_counted_SPDF))$V1
grid_050_sp_counted$centre_point_lat <- as.data.frame(coordinates(grid_050_sp_counted_SPDF))$V2


# Statistics -----
# Extract dataframe
grid_015_df <- st_set_geometry(grid_015_sp_counted, NULL)
grid_020_df <- st_set_geometry(grid_020_sp_counted, NULL)
grid_025_df <- st_set_geometry(grid_025_sp_counted, NULL)
grid_050_df <- st_set_geometry(grid_050_sp_counted, NULL)

# Selecting the continuous covariates
grid_015_df_continuous <-
  grid_015_df %>% select(dist_inst, centre_point_lon, centre_point_lat,)
grid_020_df_continuous <-
  grid_020_df %>% select(dist_inst, centre_point_lon, centre_point_lat,)
grid_025_df_continuous <-
  grid_025_df %>% select(dist_inst, centre_point_lon, centre_point_lat,)
grid_050_df_continuous <-
  grid_050_df %>% select(dist_inst, centre_point_lon, centre_point_lat,)

# Standardizing continuous covariates to eliminate effect of scale
grid_015_df_std <-
  decostand(grid_015_df_continuous, method = "standardize", MARGIN = 2)
grid_020_df_std <-
  decostand(grid_020_df_continuous, method = "standardize", MARGIN = 2)
grid_025_df_std <-
  decostand(grid_025_df_continuous, method = "standardize", MARGIN = 2)
grid_050_df_std <-
  decostand(grid_050_df_continuous, method = "standardize", MARGIN = 2)

# Categorical covariate - UC presence/absence
grid_015_df_std$uc_pres <- !is.na(grid_015_df$UF_unique)
grid_020_df_std$uc_pres <- !is.na(grid_020_df$UF_unique)
grid_025_df_std$uc_pres <- !is.na(grid_025_df$UF_unique)
grid_050_df_std$uc_pres <- !is.na(grid_050_df$UF_unique)

# Response variable - number of registers per cell
grid_015_df_std$num_reg <- grid_015_df$n_reg
grid_020_df_std$num_reg <- grid_020_df$n_reg
grid_025_df_std$num_reg <- grid_025_df$n_reg
grid_050_df_std$num_reg <- grid_050_df$n_reg

# Response variable - number of species per cell
grid_015_df_std$num_spp <- grid_015_df$countPts
grid_020_df_std$num_spp <- grid_020_df$countPts
grid_025_df_std$num_spp <- grid_025_df$countPts
grid_050_df_std$num_spp <- grid_050_df$countPts

# Fitting negative binomial distribution test
summary(
  num_reg_nbm_015 <-
    glm.nb(
      num_reg ~ dist_inst + centre_point_lon + centre_point_lat + uc_pres,
      data = grid_015_df_std
    )
)
summary(
  num_reg_nbm_020 <-
    glm.nb(
      num_reg ~ dist_inst + centre_point_lon + centre_point_lat + uc_pres,
      data = grid_020_df_std
    )
)
summary(
  num_reg_nbm_025 <-
    glm.nb(
      num_reg ~ dist_inst + centre_point_lon + centre_point_lat + uc_pres,
      data = grid_025_df_std
    )
)
summary(
  num_reg_nbm_050 <-
    glm.nb(
      num_reg ~ dist_inst + centre_point_lon + centre_point_lat + uc_pres,
      data = grid_050_df_std
    )
)

summary(
  num_sp_nbm_015 <-
    glm.nb(num_spp ~ num_reg + centre_point_lon + centre_point_lat + uc_pres,
           data = grid_015_df_std)
)
summary(
  num_sp_nbm_020 <-
    glm.nb(num_spp ~ num_reg + centre_point_lon + centre_point_lat + uc_pres,
           data = grid_020_df_std)
)
summary(
  num_sp_nbm_025 <-
    glm.nb(num_spp ~ num_reg + centre_point_lon + centre_point_lat + uc_pres,
           data = grid_025_df_std)
)
summary(
  num_sp_nbm_050 <-
    glm.nb(num_spp ~ num_reg + centre_point_lon + centre_point_lat + uc_pres,
           data = grid_050_df_std)
)

# Test dispersion to confirm distribution family
num_reg_nbm_015_sim <- simulateResiduals(num_reg_nbm_015, refit = T, n = 99)
plot(num_reg_nbm_015_sim)
testDispersion(num_reg_nbm_015_sim, alternative = 'greater') # testing overdispersion

num_reg_nbm_020_sim <- simulateResiduals(num_reg_nbm_020, refit = T, n = 99)
plotSimulatedResiduals(num_reg_nbm_020_sim)
testDispersion(num_reg_nbm_020_sim, alternative = 'greater') # testing overdispersion

num_reg_nbm_025_sim <- simulateResiduals(num_reg_nbm_025, refit = T, n = 99)
plot(num_reg_nbm_025_sim)
testDispersion(num_reg_nbm_025_sim, alternative = 'greater') # positive overdispersion (?)

num_reg_nbm_050_sim <- simulateResiduals(num_reg_nbm_050, refit = T, n = 99)
plot(num_reg_nbm_050_sim)
testDispersion(num_reg_nbm_050_sim, alternative = 'greater') # no overdispersion (?)

num_sp_nbm_050_sim <- simulateResiduals(num_sp_nbm_050, refit = T, n = 99)
plot(num_sp_nbm_050_sim)
testDispersion(num_sp_nbm_050_sim, alternative = 'greater') # no overdispersion (?)

grid_size_015 <- st_area(grid_015_sp_counted[1,])
grid_size_020 <- st_area(grid_020_sp_counted[1,])
grid_size_025 <- st_area(grid_025_sp_counted[1,]) # 6.0626.115 m² = 6 km²
grid_size_050 <- st_area(grid_050_sp_counted[1,]) # 1.458.820.697 m² ~ 1.500 km²

# For details on overdispersion test check: Overdispersion, and how to deal with it in R and JAGS
##### Grid qith cellsize equal to 0.50 (1.500 km²) had better fitting ####