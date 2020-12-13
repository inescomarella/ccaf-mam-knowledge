# File purpose: Do data analysis, run models, correlation test
# Data: 17/11/2020

# Load in libraries
x <-
  c(
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

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-scatter-plot.R")

utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load in data
record_pts_utm <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  st_transform(utm)

institute_pts_utm <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  ) %>%
  st_transform(utm)

# Get Brazil map
br_longlat <-
  brazilmaps::get_brmap(geo = "Brazil")%>% 
  st_as_sf() %>%
  st_transform(longlat)

ccaf_utm <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
) %>% 
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_transform(utm)

ccaf_longlat <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
) %>% 
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

cus_longlat <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf_longlat)

cus_utm <-
  cus_longlat %>%
  st_transform(utm)

# Pre-process map -----------------------------------------------------

# expected area = 100000m * 10000m = 1e+9 m2 = 1000 km2
cellarea <- 1000 * (1e+6)
cellsize <- 2 * sqrt(cellarea/((3*sqrt(3)/2))) * sqrt(3)/2

ccaf_grid_utm <- 
  ccaf_utm %>%
  st_make_grid(cellsize = cellsize, square = FALSE) %>%
  st_as_sf()

# Area
a <- st_area(ccaf_grid_utm)[1]
units(a) <- "km^2"
a

ccaf_grid_longlat <- 
  ccaf_grid_utm %>%
  st_transform(longlat)

ccaf_grid_utm$CU_presence <-
  ccaf_grid_utm %>%
  st_intersects(st_buffer(cus_utm, 0)) %>% 
  as.numeric() %>% 
  tibble()

ccaf_grid_utm <-
  ccaf_grid_utm %>%
  mutate(CU_presence = !is.na(CU_presence))

# Pre-process data -----------------------------------------------------------

# Measure the distance of each cell grid to the nearest research institute
ccaf_grid_utm <- get.nearest.dist(institute_pts_utm, ccaf_grid_utm)

# Count records per cell
ccaf_grid_utm$nreg <- lengths(st_intersects(ccaf_grid_utm, record_pts_utm))

# Extract dataframe
ccaf_grid_df <- 
  st_drop_geometry(ccaf_grid_utm) %>%
  select(CU_presence, dist_inst, nreg)

# Correlation test -----------------------------------------------------------

grid_cor <-
  cor(ccaf_grid_df, method = c("pearson", "kendall", "spearman"))

# Run models -----------------------------------------------------------------

# Fit Negative Binomial Generalized Linear Model
summary(
  glm_fitted <-
    glm.nb(
      nreg ~ dist_inst + CU_presence,
      data = ccaf_grid_df,
      na.action = "na.fail"
    )
)

# Rank by AIC
# Select best model to rerun the GLM
glm_ranked <- dredge(glm_fitted)

# Test quality of fit --------------------------------------------------------
par(mar = c(1,1,1,1))

# Conventional Residuals (fittedModel)
glm_simulation <- simulateResiduals(glm_fitted, plot = T)

# Detect possible misspecifications
plotResiduals(glm_simulation, ccaf_grid_df$dist_inst)
plotResiduals(glm_simulation, ccaf_grid_df$uc_pres)

# Test overdispersion
testDispersion(glm_simulation, alternative = "greater")

# Test if there are more zeros than expected
testZeroInflation(glm_simulation, alternative = "greater")

# For details on overdispersion test check:
# > Overdispersion, and how to deal with it in R and JAGS
# > DHARMa: residual diagnostics for hierarchical (multi-level/mixed)
# regression models

# Save -----------------------------------------------------------------------

# Correlation test plot
pdf(
  file = "../data/results/correlation-plot-grid.pdf",
  width = 5,
  height = 6
)
corrplot(
  grid_cor,
  type = "upper",
  method = "circle",
  tl.col = "black",
  tl.srt = 45,
  order = "original"
)
dev.off()

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
  mutate_if(is.numeric, ~ round(., 3))

glm_ranked_df <-
  as.data.frame(glm_ranked) %>%
  mutate_if(is.numeric, ~ round(., 3))

glm_refitted_coef <-
  as.data.frame(coef(summary(glm_fitted))) %>%
  mutate_if(is.numeric, ~ round(., 3))

OUT <- createWorkbook()

addWorksheet(OUT, "global-model-coef-grid")
addWorksheet(OUT, "model-selection-grid")
addWorksheet(OUT, "selected-model-coef-grid")

writeData(OUT, sheet = "global-model-coef-grid", x = glm_fitted_coef)
writeData(OUT, sheet = "model-selection-grid", x = glm_ranked_df)
writeData(OUT, sheet = "selected-model-coef-grid", x = glm_refitted_coef)

saveWorkbook(OUT, "../data/results/model-results.xlsx", overwrite = TRUE)
