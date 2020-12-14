# File purpose: Do data analysis, run models, correlation test
# Data: 17/11/2020
# 
##############################################################
# For details on overdispersion test check:
# > Overdispersion, and how to deal with it in R and JAGS
# > DHARMa: residual diagnostics for hierarchical (multi-level/mixed)
# regression models
##############################################################

# Load libraries
xfun::pkg_attach(c(
  "tidyverse",
  "sf",
  "openxlsx",
  "brazilmaps",
  "vegan",
  "DHARMa",
  "MASS"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-scatter-plot.R")

# Set projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data --------------------------------------------------
records_utm <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  st_transform(utm)

institutes_utm <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  ) %>%
  st_transform(utm)

br_longlat <-
  get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)

ccaf_longlat <- 
  st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
  ) %>%
  st_set_crs(longlat) %>%
  # Only Central Corridor of Atlantic Forest
  filter(str_detect(NOME1, "Mata")) %>%
  # Only terrestrial area
  st_intersection(br_longlat) %>%
  # Fix name
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

cus_utm <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf_longlat) %>%
  st_transform(utm)

# Make grid --------------------------------------------------

# Reproject CCAF map to a metric coordinate system
ccaf_utm <- 
  ccaf_longlat %>%
  st_transform(utm)

# Expected area = 1000m * 1000000m = 1e+9 m2 = 1000 km2
cellarea <- 1000 * (1e+6)
cellsize <- 2 * sqrt(cellarea / ((3 * sqrt(3) / 2))) * sqrt(3) / 2

# Make the hexagon grid with the expected area as simple feature
ccaf_grid_utm <-
  ccaf_utm %>%
  st_make_grid(cellsize = cellsize, square = FALSE) %>%
  st_as_sf()

# Area
a <- st_area(ccaf_grid_utm)[1]
units(a) <- "km^2"
a

# Pre-process data -------------------------------------------

# Get CU presence in each cell
ccaf_grid_utm$CU <- lengths(st_intersects(ccaf_grid_utm, cus_utm))

# Measure the distance from each cell grid centre to the nearest research institute
ccaf_grid_utm <- get.nearest.dist(institutes_utm, ccaf_grid_utm)

# Count records per cell
ccaf_grid_utm$nreg <-
  lengths(st_intersects(ccaf_grid_utm, records_utm))

# Extract dataframe
ccaf_grid_df <-
  st_drop_geometry(ccaf_grid_utm) %>%
  mutate(CU_presence = ifelse(CU == 1, "Present", "Absent")) %>%
  select(CU_presence, dist_inst, nreg)

# Run models -------------------------------------------------

# Fit Negative Binomial Generalized Linear Model
summary(
  glm_fitted <-
    glm.nb(
      nreg ~ dist_inst + CU_presence,
      data = ccaf_grid_df,
      na.action = "na.fail"
    )
)

# Test quality of fit -----------------------------------------
par(mar = c(1, 1, 1, 1))

# Conventional Residuals (fittedModel)
glm_simulation <- simulateResiduals(glm_fitted, plot = T)

# Detect possible misspecifications
plotResiduals(glm_simulation, ccaf_grid_df$dist_inst)
plotResiduals(glm_simulation, ccaf_grid_df$uc_pres)

# Test overdispersion
testDispersion(glm_simulation, alternative = "greater")

# Test if there are more zeros than expected
testZeroInflation(glm_simulation, alternative = "greater")

# Save --------------------------------------------------------

# Overdispersion plots
pdf(
  file = "../data/results/model-residual-plot-grid.pdf",
  width = 7.5,
  height = 5
)
plot(glm_simulation)
dev.off()

pdf(
  file = "../data/results/model-residual-variables-plot-grid.pdf",
  width = 8,
  height = 5
)
par(mfrow = c(1, 2))
plotResiduals(glm_simulation, ccaf_grid_df$dist_inst)
plotResiduals(glm_simulation, ccaf_grid_df$uc_pres)
dev.off()

pdf(
  file = "../data/results/model-overdispersion-plot-grid.pdf",
  width = 7.5,
  height = 5
)
testDispersion(glm_simulation, alternative = "greater")
dev.off()

pdf(
  file = "../data/results/model-zeroinflation-plot-grid.pdf",
  width = 7.5,
  height = 5
)
testZeroInflation(glm_simulation, alternative = "greater")
dev.off()

# Model output
glm_fitted_coef <-
  as.data.frame(coef(summary(glm_fitted))) %>%
  mutate(variable = row.names(.)) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  relocate(variable)

OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = glm_fitted_coef)
saveWorkbook(OUT, "../data/results/model-results.xlsx", overwrite = TRUE)
