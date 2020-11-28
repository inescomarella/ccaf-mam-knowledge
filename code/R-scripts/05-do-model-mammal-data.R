# File purpose: Do data analysis, run models, correlation test
# Data: 17/11/2020

########################################################################
# To do:
#   - Re-run python script to see the best way to count for UC presence
########################################################################

# Load in libraries
x <-
  c(
    "conflicted",
    "tidyverse",
    "raster",
    "sf",
    "rgdal",
    "openxlsx",
    "FNN",
    "vegan",
    "corrplot",
    "DHARMa",
    "MASS",
    "MuMIn"
  )
lapply(x, library, character.only = TRUE)

conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/03-funs-model-mammal-data.R")

# Load in data
record_pts <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )
institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )
g025_geom <-
  st_read(dsn = "../data/processed-data/", layer = "grid-025-ucs-joined")

############################ Pre-process data ################################

# Before converting to UTM, save coordinates in degree
# Latitude in degree will be used as a variable in the GLM
g025_centroid_longlat <- coordinates(as(g025_geom, "Spatial"))

g025_geom$lat <- as.data.frame(g025_centroid_longlat)$V2

# Reproject to a metric coordinate system
utm <-
  CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
rcrd_utm <- st_transform(record_pts, utm)
inst_utm <- st_transform(institute_pts, utm)
g025_utm <- st_transform(g025_geom, utm)

# Measure mean polygon area
g025_area <- mean(st_area(g025_utm)) # 484.516.066 m² = 484 km²

# Measure the distance of each cell grid to the nearest research institute
g025_utm <- get.nearest.dist(inst_utm, g025_utm)

# Count records per cell
g025_utm$nreg <- lengths(st_intersects(g025_utm, rcrd_utm))

# Count species per cell
# Take 14.33s and 16.5s to run each respectively
g025_utm <- count.sp.in.polygons(rcrd_utm, g025_utm)

# Extract dataframe
g025_df <- st_set_geometry(g025_utm, NULL)

# Select the variables of interest
g025_df_std <-
  g025_df %>%
  # UC presence as a categorical variable (TRUE/FALSE)
  mutate(uc_pres = is.na(UF_unique)) %>%
  select(nreg, nsp, dist_inst, uc_pres, lat)

############################## Correlation test ###############################

g025_cor <-
  cor(g025_df_std, method = c("pearson", "kendall", "spearman"))

############################## Run global models #############################

# Fit Negative Binomial Generalized Linear Model
summary(
  glm_025_fitted <-
    glm.nb(
      nreg ~ dist_inst + lat + uc_pres,
      data = g025_df_std,
      na.action = "na.fail"
    )
)

############################## Model selection ###############################

# Rank by AIC
glm_025_ranked <- dredge(glm_025_fitted)

############################# Run selected models #############################

# Fit Negative Binomial Generalized Linear Model
summary(
  glm_025_refitted <-
    glm.nb(
      nreg ~ dist_inst + uc_pres,
      data = g025_df_std,
      na.action = "na.fail"
    )
)

######################### Test model quality of fit ###########################

# Conventional Residuals (fittedModel)
glm_025_simulation <- simulateResiduals(glm_025_refitted, plot = T)

# Detect possible misspecifications
plotResiduals(glm_025_simulation, g025_df_std$dist_inst)
plotResiduals(glm_025_simulation, g025_df_std$uc_pres)

# Test overdispersion
testDispersion(glm_025_simulation, alternative = "greater")

# Test if there are more zeros than expected
testZeroInflation(glm_025_simulation, alternative = "greater")

# For details on overdispersion test check:
# > Overdispersion, and how to deal with it in R and JAGS
# > DHARMa: residual diagnostics for hierarchical (multi-level/mixed)
# regression models

############################ Save correlation plot ############################

# Correlation test plot
pdf(
  file = "../data/results/correlation-plot-g025.pdf",
  width = 5,
  height = 6
)
corrplot(
  g025_cor,
  type = "upper",
  method = "circle",
  tl.col = "black",
  tl.srt = 45,
  order = "original"
)
dev.off()

############################## Save model output ##############################

glm_025_fitted_coef <-
  as.data.frame(coef(summary(glm_025_fitted))) %>%
  mutate_if(is.numeric, ~ round(., 3))

glm_025_ranked_df <-
  as.data.frame(glm_025_ranked) %>%
  mutate_if(is.numeric, ~ round(., 3))

glm_025_refitted_coef <-
  as.data.frame(coef(summary(glm_025_fitted))) %>%
  mutate_if(is.numeric, ~ round(., 3))

OUT <- createWorkbook()

addWorksheet(OUT, "global-model-coef-g025")
addWorksheet(OUT, "model-selection-g025")
addWorksheet(OUT, "selected-model-coef-g025")

writeData(OUT, sheet = "global-model-coef-g025", x = glm_025_fitted_coef)
writeData(OUT, sheet = "model-selection-g025", x = glm_025_ranked_df)
writeData(OUT, sheet = "selected-model-coef-g025", x = glm_025_refitted_coef)

saveWorkbook(OUT, "../data/results/model-results.xlsx", overwrite = TRUE)

########################## Save overdispersion plots ##########################

pdf(
  file = "../data/results/model-residual-plot-g025.pdf",
  width = 7.5,
  height = 5
)
plot(glm_025_simulation)
dev.off()
pdf(
  file = "../data/results/model-residual-variables-plot-g025.pdf",
  width = 8,
  height = 5
)
par(mfrow = c(1, 2))
plotResiduals(glm_025_simulation, g025_df_std$dist_inst)
plotResiduals(glm_025_simulation, g025_df_std$uc_pres)
dev.off()

pdf(
  file = "../data/results/model-overdispersion-plot-g025.pdf",
  width = 7.5,
  height = 5
)
testDispersion(glm_025_simulation, alternative = "greater")
dev.off()

pdf(
  file = "../data/results/model-zeroinflation-plot-g025.pdf",
  width = 7.5,
  height = 5
)
testZeroInflation(glm_025_simulation, alternative = "greater")
dev.off()
