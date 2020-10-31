x <- c('rgdal', 'tidyverse', 'FNN', 'sp', 'vegan', 'MASS', 'DHARMa')
lapply(x, library, character.only = TRUE)
options(stringsAsFactors = FALSE)

# Inputs
inst_df <-
  read.csv('./data/institutions-ccma.csv', header = T) # Institutions
grid_layer <- readOGR(dsn = './outputs', layer = 'grid_joined')

# Change lat/lon order
inst_lat <- inst_df$latitude
inst_lon <- inst_df$longitude
inst_coord <- bind_cols(inst_lon, inst_lat)

# Convert dataframe do SpatialPoints
inst_layer <-
  SpatialPoints(inst_coord, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Convert projection to Albers Equal Area
albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=42.5")
grid_alb <- spTransform(grid_layer, albers)
inst_alb <- spTransform(inst_layer, albers)

# Grid centre point distance to nearest institution
dist_knnx <-
  get.knnx(coordinates(inst_alb), coordinates(grid_alb), k = 1)

# Add distance to grid data table
grid_alb@data$dist_inst <- as.data.frame(dist_knnx)$nn.dist

# Dataframe to GLM
coord_centre_grid <- coordinates(grid_alb)
data_df <-
  data.frame(
    dist_inst = as.numeric(grid_alb@data$dist_inst),
    centre_lon = coord_centre_grid[, 1],
    centre_lat = coord_centre_grid[, 2]
  )

# Standardizing continuous predictive covariates to eliminate effect of scale
data_df_std <- decostand(data_df, method = "standardize", MARGIN = 2)

# Categorical covariate - UC presence/absence
data_df_std$uc_pres <- !is.na(grid_alb@data$NOME_ORG12)

# Response variables
data_df_std$num_reg <- as.numeric(grid_alb@data$species_un)
data_df_std$num_sp <- as.numeric(grid_alb@data$species_co)
data_df_std[is.na(data_df_std)] <- 0 # consider NA = 0 species

# Fitting negative binomial distribution test
summary(num_reg_nbm <-
          glm.nb(num_reg ~ dist_inst + centre_lon + centre_lat + uc_pres, 
                 data = data_df_std))
summary(num_sp_nbm <-
          glm.nb(num_sp ~ num_reg + centre_lon + centre_lat + uc_pres, 
                 data = data_df_std))

# Test dispersion to confirm distribution family
num_reg_nbm_sim <- simulateResiduals(num_reg_nbm, refit = T, n = 99)
plotSimulatedResiduals(num_reg_nbm_sim)
testDispersion(num_reg_nbm_sim, alternative = 'greater') # testing overdispersion

# Visualizing register numbers per grid cell
ggplot(data_df_std, aes(x = num_reg)) + geom_bar()


# For details on overdispersion test check: Overdispersion, and how to deal with it in R and JAGS