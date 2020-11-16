setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'FNN', 'vegan', 'corrplot', 'DHARMa', 'MASS', 'MuMIn')
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
inst_df <-
  read.csv('../data/institutions-ccma.csv', header = T)
g025 <- st_read(dsn = '../outputs', layer = 'grid_025_ucs_joined')
g050 <- st_read(dsn = '../outputs', layer = 'grid_050_ucs_joined')

# Prepare data ----
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

# Keep long/lat to statistics
g025_longlat <- st_transform(g025, CRS("+proj=longlat +datum=WGS84"))
g050_longlat <- st_transform(g050, CRS("+proj=longlat +datum=WGS84"))

# Count records per cell
g025_utm$nreg <- lengths(st_intersects(g025_utm, data_utm))
g050_utm$nreg <- lengths(st_intersects(g050_utm, data_utm))

# Count species per cell
g025_utm <- count.sp.in.polygons(data_utm, g025_utm)
g050_utm <- count.sp.in.polygons(data_utm, g050_utm)

# Getting centroid points
g025_centroid <- coordinates(as(g025_utm, 'Spatial'))
g050_centroid <- coordinates(as(g050_utm, 'Spatial'))
inst_coords <- st_coordinates(inst_utm)

#for estatistics
g025_centroid_longlat <- coordinates(as(g025_longlat, 'Spatial'))
g050_centroid_longlat <- coordinates(as(g050_longlat, 'Spatial'))

# Centre point distance to the nearest institution
dist_025 <-
  get.knnx(inst_coords, g025_centroid, k = 1)
dist_050 <-
  get.knnx(inst_coords, g050_centroid, k = 1)

# Add distance to grid attribute table
g025_utm$dist_inst <- as.data.frame(dist_025)$nn.dist
g050_utm$dist_inst <- as.data.frame(dist_050)$nn.dist

# Adding centre point to attribute table (to be used as covariate in GLM)
g025_utm$lat <- as.data.frame(g025_centroid_longlat)$V2
g050_utm$lat <- as.data.frame(g050_centroid_longlat)$V2

# Extract dataframe
g025_df <- st_set_geometry(g025_utm, NULL)
g050_df <- st_set_geometry(g050_utm, NULL)

# Selecting the continuous variables
g025_df_std <-
  g025_df %>% select(countPts, nreg, dist_inst, lat)
g050_df_std <-
  g050_df %>% select(countPts, nreg, dist_inst, lat)

# Categorical variable - UC presence/absence
g025_df_std$uc_pres <- !is.na(g025_df$UF_unique)
g050_df_std$uc_pres <- !is.na(g050_df$UF_unique)

colnames(g025_df_std) <- c('n_sp', 'n_reg', 'dist_inst', 'lat', 'uc_pres')
colnames(g050_df_std) <- c('n_sp', 'n_reg', 'dist_inst', 'lat', 'uc_pres')

g025_df_std <- g025_df_std %>% select(n_reg, n_sp, dist_inst, uc_pres, lat)
g050_df_std <- g050_df_std %>% select(n_reg, n_sp, dist_inst, uc_pres, lat)

# Correlation test ----
g025_cor <-
  cor(g025_df_std, method = c("pearson", "kendall", "spearman"))
g050_cor <-
  cor(g050_df_std, method = c("pearson", "kendall", "spearman"))

# Plot correlation test
corrplot(
  g025_cor,
  type = "upper",
  method = 'circle',
  tl.col = "black",
  tl.srt = 45,
  order = "original"
)
corrplot(
  g050_cor,
  type = "upper",
  method = 'circle',
  tl.col = "black",
  tl.srt = 45,
  order = "original"
)

# Pearson's chi-squared test
g025_chisq <- chisq.test(g025_df_std$n_reg, g025_df_std$uc_pres)
g050_chisq <- chisq.test(g050_df_std$n_reg, g050_df_std$uc_pres)

# Statistics -----

# Fit Negative Binomial Generalized Linear Model
summary(
  nreg_025_fitted <-
    glm.nb(
      n_reg ~ dist_inst + lat + uc_pres,
      data = g025_df_std, na.action = 'na.fail')
)
summary(
  nreg_050_fitted <-
    glm.nb(
      n_reg ~ dist_inst + lat + uc_pres,
      data = g050_df_std, na.action = 'na.fail')
)

# Rank by AIC
nreg_025_ranked <- dredge(nreg_025_fitted)
nreg_050_ranked <- dredge(nreg_050_fitted)

# Conventional Residuals (fittedModel)
nreg_025_simulation <- simulateResiduals(nreg_025_fitted, plot = T)
nreg_050_simulation <- simulateResiduals(nreg_050_fitted, plot = T)

# Detect possible misspecifications
plotResiduals(nreg_025_simulation, g025_df_std$lat)
plotResiduals(nreg_025_simulation, g025_df_std$dist_inst)
plotResiduals(nreg_025_simulation, g025_df_std$uc_pres)

plotResiduals(nreg_050_simulation, g050_df_std$lat)
plotResiduals(nreg_050_simulation, g050_df_std$dist_inst)
plotResiduals(nreg_050_simulation, g050_df_std$uc_pres)

# Test overdispersion
testDispersion(nreg_025_simulation, alternative = 'greater')
testDispersion(nreg_050_simulation, alternative = 'greater')

# Test if there are more zeros than expected
testZeroInflation(nreg_025_simulation, alternative = 'greater')
testZeroInflation(nreg_050_simulation, alternative = 'greater') #zero inflated?

# For details on overdispersion test check: 
# > Overdispersion, and how to deal with it in R and JAGS
# > DHARMa: residual diagnostics for hierarchical (multi-level/mixed) regression models

# Mean polygon area ----
g025_area <- mean(st_area(g025_utm)) # 484.516.066 m² = 484 km²
g050_area <- mean(st_area(g050_utm)) # 1.646.877.595 m² = 1.646 km²

