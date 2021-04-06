
# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "fossil",
    "raster",
    "patchwork",
    "cowplot",
    "openxlsx",
    "recipes",
    "tidymodels",
    "dotwhisker",
    "geobgu",
    "stars",
    "fishualize",
    "ggpubr"
  )
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")
conflicted::conflict_prefer("raster_extract", "geobgu")
conflicted::conflict_prefer("get_legend", "cowplot")

source("./R-scripts/functions/funs-spatial-analyses.R")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")
utm <- CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load in data -----------
br_longlat <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  st_crop(
    xmax = -38.7,
    xmin = -41.87851,
    ymax = -13.00164,
    ymin = -21.30178
  )

rm(br_longlat)

forest <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(as(ccaf, "Spatial")) %>%
  mask(as(ccaf, "Spatial")) %in% 1:10

cus_longlat <-
  read_sf("../data/processed-data/CUs-map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf)

records_longlat <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  mutate(id = seq(1, nrow(.)))

institutes_utm <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Make grid ----
cellsize <- sqrt(3500) * 1000

# Make the hexagon grid with the expected area as simple feature
grid <-
  ccaf %>%
  st_make_grid(cellsize = 0.55, square = T, offset = c(st_bbox(ccaf)[c("xmin")]-0.22, st_bbox(ccaf)[c("ymin")]-0.45)) %>%
  st_as_sf()

grid %>% ggplot() + geom_sf() + 
  geom_sf(data = ccaf, fill = NA)

grid$grid_id <- seq(1, nrow(grid), 1)

# Area
grid$area <- grid %>% st_area()
a <- grid$area[1]
units(a) <- "km^2"
a
units(grid$area) <- "km^2"
max(grid$area)
min(grid$area)

units(grid$area) <- NULL



# Biological variables ----
record_grid <- st_intersection(records_longlat, grid)

for (i in 1:nrow(grid)) {
  
  grid$nrec[i] <- record_grid %>%
    filter(grid_id == i) %>%
    nrow()
  
  nrec <- record_grid %>%
    filter(grid_id == i) %>%
    filter(!is.na(eventDate)) %>%
    nrow()
  
  cell_df <- record_grid %>%
    filter(grid_id == i) %>%
    select(eventDate, species) %>%
    st_drop_geometry() %>%
    unique()
  
  grid$Sobs[i] <- length(unique(cell_df$species))
  
  if (nrec >= 25) {
    
    cell_df_expended <- cell_df %>%
      expand(eventDate, species)
    
    cell_df_expended_absent <- anti_join(cell_df_expended, cell_df)
    
    cell_df$State <- 1
    cell_df_expended_absent$State <- 0
    
    cell_df_State <- bind_rows(cell_df, cell_df_expended_absent)
    
    sp_by_saple_df <- cell_df_State %>%
      arrange(eventDate, species) %>%
      spread(key = eventDate, value = State)
    
    grid$Sest[i] <- chao2(sp_by_saple_df[, -1])
  } else {
    grid$Sest[i] <- NA
  }
  
}

grid_bio <- grid %>%
  mutate(c = Sobs/Sest)

proximity <- 
  research.institute.proximity(grid = st_join(records_longlat, grid), institutes_points = institutes_utm)

grid_bio <- merge(grid_bio, proximity, all = TRUE)

# Get environment data ----
# Get elevation data
elevation_data <- 
  getData("alt",
  country = "BRA",
  mask = TRUE,
  res = 2.5
)

# Get environment data
worldclim_data <- getData("worldclim", var = "bio", res = 2.5)

# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(1, 12)]]
names(worldclim_amt_ap) <-
  c("AMT", "AP")

# Extract environment data from sample points
ccaf_environmet <- mask(crop(worldclim_amt_ap, ccaf), ccaf)
ccaf_elevation <- mask(crop(elevation_data, ccaf), ccaf)

# Re-scale
ccaf_environmet_rescaled <- disaggregate(ccaf_environmet, fact = 6/2.5, fun = "bilinear")
ccaf_elevation_rescaled <- disaggregate(ccaf_elevation, fact = 6/2.5, fun = "bilinear")

# Calculate variables ----

grid_envi <- grid_bio %>% 
  mutate(
    forest_cov = raster_extract(
      x = st_as_stars(forest),
      y = grid_bio,
      fun = sum,
      na.rm = TRUE
    ),
    AMT = raster_extract(
      x = st_as_stars(ccaf_environmet_rescaled$AMT),
      y = grid_bio,
      fun = mean,
      na.rm = TRUE
    ),
    AP = raster_extract(
      x = st_as_stars(ccaf_environmet_rescaled$AP),
      y = grid_bio,
      fun = mean,
      na.rm = TRUE
    ),
    elev = raster_extract(
      x = st_as_stars(ccaf_elevation_rescaled),
      y = grid_bio,
      fun = mean,
      na.rm = TRUE
    )
  )

grid_envi$CU <-  grid_envi %>%
  st_intersects(cus_longlat) %>%
  lengths()

grid_data <-  grid_envi %>%
  group_by(grid_id) %>%
  mutate(
    KL = (c + nrec / max(grid_envi$nrec, na.rm = TRUE)) / 2
  ) %>%
  mutate(KL = ifelse(is.na(KL) | is.infinite(KL), 0, KL))

grid_data_processing <- grid_data %>%
  mutate(
    elevd = abs(elev - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$elev),
    AMTd = abs(AMT - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$AMT),
    APd = abs(AP - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$AP)
  )

grid_data_processed <- grid_data_processing %>%
  mutate(
    elevd = elevd / max(grid_data_processing$elevd, na.rm = TRUE),
    AMTd = AMTd / max(grid_data_processing$AMTd, na.rm = TRUE),
    APd = APd / max(grid_data_processing$APd, na.rm = TRUE),
    forestw = forest_cov / max(grid_data_processing$forest_cov, na.rm = TRUE)
  ) %>%
  mutate(KG = forestw * mean(c(elevd, AMTd, APd, (1 - KL))))

# Map variables ----

grid_data_classified <- grid_data_processed %>%
  filter(!is.nan(KG)) %>%
  mutate(KG_class = ifelse(
    KG > 0.8,
    "Very high",
    ifelse(
      KG > 0.6 & KG < 0.8,
      "High",
      ifelse(
        KG > 0.4 & KG < 0.6,
        "Medium",
        ifelse(
          KG > 0.2 & KG < 0.4,
          "Low",
          "Very low"
        )
      )
    )
  )) %>%
  mutate(KL_class = ifelse(
    KL > 0.8,
    "Very high",
    ifelse(
      KL > 0.6 & KL < 0.8,
      "High",
      ifelse(
        KL > 0.4 & KL < 0.6,
        "Medium",
        ifelse(
          KL > 0.2 & KL < 0.4,
          "Low",
          "Very low"
        )
      )
    )
  ))

levels <- c("Very high", "High", "Medium", "Low", "Very low")

grid_data_classified$KL_class <-
  factor(grid_data_classified$KL_class,
    levels = levels
  )
grid_data_classified$KG_class <-
  factor(grid_data_classified$KG_class,
    levels = levels
  )

KL_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(aes(fill = KL_class), color = NA) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  theme_light() +
  labs(fill = "Knowledge level")

KG_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(aes(fill = KG_class), color = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Study urgency level")

nrec_map <- grid_data_classified %>%
  filter(nrec > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = nrec)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(1, max(grid_data_classified$nrec)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), nrec, 0, 1),
    labels = break_5points(st_drop_geometry(grid_data_classified), nrec, 0, 1)
  ) +
  theme_light() +
  labs(fill = "Number of\nRecords")

c_map <- grid_data_classified %>%
  filter(!is.na(c)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = c)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(0, max(grid_data_classified$c, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), c, 1, 0),
    labels = break_5points(st_drop_geometry(grid_data_classified), c, 1, 0)
  ) +
  theme_light() +
  labs(fill = "Completeness")

CU_map <- grid_data_classified %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggplot() +
  geom_sf(color = NA, aes(fill = CU)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus", discrete = TRUE) +
  theme_light() +
  labs(fill = "Conservation\nUnit")

forest_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = forest_cov)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    breaks = c(0, max(grid_data_classified$forest_cov, na.rm = TRUE)),
    labels = c("Low", "High")
  ) +
  theme_light() +
  labs(fill = "Relative forest coverage")

Sobs_map <- grid_data_classified %>%
  filter(!is.na(Sobs)) %>%
  filter(Sobs > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sobs)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(1, max(grid_data_classified$Sobs, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), Sobs, 0, 1),
    labels = break_5points(st_drop_geometry(grid_data_classified), Sobs, 0, 1)
  ) +
  theme_light() +
  labs(fill = "Observed species\nrichness")

Sest_map <- grid_data_classified %>%
  filter(!is.na(Sest)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sest)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(grid_data_classified$Sest, na.rm = TRUE), 
               max(grid_data_classified$Sest, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), Sest, 0, min(grid_data_classified$Sest, na.rm = TRUE)),
    labels = break_5points(st_drop_geometry(grid_data_classified), Sest, 0, min(grid_data_classified$Sest, na.rm = TRUE))
  ) +
  theme_light() +
  labs(fill = "Estimated species\nrichness")

elev_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elev)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(round(min(grid_data_classified$elev, na.rm = TRUE), 0), 
               max(grid_data_classified$elev, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), elev, 0, round(min(grid_data_classified$elev, na.rm = TRUE), 0)),
    labels = break_5points(st_drop_geometry(grid_data_classified), elev, 0, round(min(grid_data_classified$elev, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Elevation (m)")

elev_distance_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elevd)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Elevd")

grid_data_classified <- grid_data_classified %>%
  mutate(AMT = AMT / 10)

AMT_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMT)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      round(min(grid_data_classified$AMT, na.rm = TRUE), 1),
      round(max(grid_data_classified$AMT, na.rm = TRUE), 1)
    ),
    breaks = break_5points(st_drop_geometry(grid_data_classified), AMT, 1, round(min(grid_data_classified$AMT, na.rm = TRUE), 1)),
    labels = break_5points(st_drop_geometry(grid_data_classified), AMT, 1, round(min(grid_data_classified$AMT, na.rm = TRUE), 1))
  ) +
  theme_light() +
  labs(fill = "Annual Mean\nTemperature (ºC)")

AMT_distance_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMTd)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

AP_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(round(min(grid_data_classified$AP, na.rm = TRUE), 0), 
               round(max(grid_data_classified$AP, na.rm = TRUE), 0)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), AP, 0, round(min(grid_data_classified$AP, na.rm = TRUE), 0)),
    labels = break_5points(st_drop_geometry(grid_data_classified), AP, 0, round(min(grid_data_classified$AP, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Annual precipitation (mm)")

AP_distance_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = APd)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

pri_prox_map <- grid_data_classified %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = proximity)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Proximity to collection")
# Graph nrec x variables ----

proximity_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "proximity", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Proximity to collection")

CU_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggscatter(
    x = "CU", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Conservation Unit")

forest_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "forestw", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Relative forest coverage")


Sest_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "Sest", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Chao2 species richness estimation")

c_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "c", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Completeness")

KG_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "KG", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Knowledge gap index")

KL_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "KL", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Knowledge level index")

elev_graph <- grid_data_classified %>%
  ggscatter(
    x = "elev", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Elevation (m)")

AMT_graph <- grid_data_classified %>%
  mutate(AMT = AMT / 10) %>%
  ggscatter(
    x = "AMT", y = "KL",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Annual Mean Temperature (ºC)")

AP_graph <- grid_data_classified %>%
  ggscatter(
    x = "AP", y = "KL",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Annual Precipitation (mm)")

# Fit model ----

# Pre-process
df_recipe <- grid_data_classified %>%
  st_drop_geometry() %>%
  recipe(nrec ~ proximity + CU + forest_cov + Sest) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_juice <- juice(df_recipe)

# Linear regression model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit model
lm_fit <-
  lm_mod %>%
  fit(nrec ~ proximity + CU + forest_cov + Sest, data = df_juice)

tidy(lm_fit)

# Predict ----

# New data
new_points_CUp_dist <- expand.grid(
  CU = 1,
  proximity = seq(-5, 5, 0.2),
  forest_cov = mean(df_juice$forest_cov, na.rm = TRUE),
  Sest = max(df_juice$Sest, na.rm = TRUE)
)
new_points_CUp_Sest <- expand.grid(
  CU = 1,
  proximity = mean(df_juice$proximity, na.rm = TRUE),
  forest_cov = mean(df_juice$forest_cov, na.rm = TRUE),
  Sest = seq(-5, 5, 0.2)
)
new_points_CUa_dist <- expand.grid(
  CU = 0,
  proximity = seq(-5, 5, 0.2),
  forest_cov = mean(df_juice$forest_cov, na.rm = TRUE),
  Sest = mean(df_juice$Sest, na.rm = TRUE)
)
new_points_CUa_Sest <- expand.grid(
  CU = 0,
  proximity = mean(df_juice$proximity, na.rm = TRUE),
  forest_cov = mean(df_juice$forest_cov, na.rm = TRUE),
  Sest = seq(-5, 5, 0.2)
)
# Predict mean
mean_pred_CUp_dist <- predict(lm_fit, new_data = new_points_CUp_dist)
mean_pred_CUp_Sest <- predict(lm_fit, new_data = new_points_CUp_Sest)
mean_pred_CUa_dist <- predict(lm_fit, new_data = new_points_CUa_dist)
mean_pred_CUa_Sest <- predict(lm_fit, new_data = new_points_CUa_Sest)

# Predict confidence interval
conf_int_pred_CUp_dist <- predict(lm_fit,
  new_data = new_points_CUp_dist,
  type = "conf_int"
)
conf_int_pred_CUp_Sest <- predict(lm_fit,
  new_data = new_points_CUp_Sest,
  type = "conf_int"
)
conf_int_pred_CUa_dist <- predict(lm_fit,
  new_data = new_points_CUa_dist,
  type = "conf_int"
)
conf_int_pred_CUa_Sest <- predict(lm_fit,
  new_data = new_points_CUa_Sest,
  type = "conf_int"
)

plot_data_CUp_dist <-
  new_points_CUp_dist %>%
  bind_cols(mean_pred_CUp_dist) %>%
  bind_cols(conf_int_pred_CUp_dist) %>%
  mutate(ID = "CU present")
plot_data_CUp_Sest <-
  new_points_CUp_Sest %>%
  bind_cols(mean_pred_CUp_Sest) %>%
  bind_cols(conf_int_pred_CUp_Sest) %>%
  mutate(ID = "CU present")
plot_data_CUa_dist <-
  new_points_CUa_dist %>%
  bind_cols(mean_pred_CUa_dist) %>%
  bind_cols(conf_int_pred_CUa_dist) %>%
  mutate(ID = "CU absent")
plot_data_CUa_Sest <-
  new_points_CUa_Sest %>%
  bind_cols(mean_pred_CUa_Sest) %>%
  bind_cols(conf_int_pred_CUa_Sest) %>%
  mutate(ID = "CU absent")

plot_data_dist <- bind_rows(plot_data_CUp_dist, plot_data_CUa_dist)
plot_data_Sest <- bind_rows(plot_data_CUp_Sest, plot_data_CUa_Sest)

# Plot prediction ----

# Model predictions
predict_collection <- ggplot(plot_data_dist, aes(x = distance, color = ID)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(
    ymin = .pred_lower,
    ymax = .pred_upper
  ),
  width = .2
  ) +
  labs(
    y = "Number of records",
    x = "Proximity to collection"
  ) +
  scale_color_discrete(name = element_blank()) +
  theme_light()

predict_SR <- ggplot(plot_data_Sest, aes(x = Sest, color = ID)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(
    ymin = .pred_lower,
    ymax = .pred_upper
  ),
  width = .2
  ) +
  labs(
    y = "Number of records",
    x = "Species Richness Estimated"
  ) +
  scale_color_discrete(name = element_blank()) +
  theme_light()

# Model coefficients
tidy(lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  ) + theme_light()

# Save results -------------
nrec_map + theme_void() 
ggsave("../data/results/07-nrec-map.pdf",
       width = 8,
       height = 6
)

Sobs_map + theme_void() + geom_sf(data = records_longlat, size = 0.2)
ggsave("../data/results/07-Sobs-map.pdf",
       width = 8,
       height = 6
)

Sest_map + theme_void()  + geom_sf(data = records_longlat, size = 0.2)
ggsave("../data/results/07-Sest-map.pdf",
       width = 8,
       height = 6
)

c_map + theme_void()  + geom_sf(data = records_longlat, size = 0.2)
ggsave("../data/results/07-c-map.pdf",
       width = 8,
       height = 6
)

KL_map + theme_void() 
ggsave("../data/results/07-KL-map.pdf",
       width = 8,
       height = 6
)

forest_map + theme_void() + labs(fill = "Relative forest\ncoverage")
ggsave("../data/results/07-forest-map.pdf",
       width = 8,
       height = 6
)

elev_map + theme_void()
ggsave("../data/results/07-elevation-map.pdf",
       width = 8,
       height = 6
)

AMT_map + theme_void()
ggsave("../data/results/07-AMT-map.pdf",
       width = 8,
       height = 6
)

AP_map + theme_void()
ggsave("../data/results/07-AMT-map.pdf",
       width = 8,
       height = 6
)

elev_distance_map + theme_void()
ggsave("../data/results/07-elevation-distance-map.pdf",
       width = 8,
       height = 6
)

AMT_distance_map + theme_void()
ggsave("../data/results/07-AMT-distance-map.pdf",
       width = 8,
       height = 6
)

AP_distance_map + theme_void()
ggsave("../data/results/07-AMT-distance-map.pdf",
       width = 8,
       height = 6
)

KG_map + theme_void()
ggsave("../data/results/07-KG-map.pdf",
       width = 8,
       height = 6
)

p1 <- KL_map  + theme_void() 
p2 <- nrec_map  + theme_void()
p3 <- c_map  + theme_void()
p4 <- Sobs_map  + theme_void()
p5 <- Sest_map  + theme_void()

p_grid <- plot_grid(p2, p3, p4, p5, ncol = 2, labels = c("B", "C", "D", "E"), label_size = 12)

plot_grid(p1, p_grid, labels = c("A", ""), label_size = 12)
ggsave("../data/results/07-bio-vars-map.pdf",
       width = 11.69,
       height = 8.27
)

p1 <- forest_map  + theme_void()+ labs(fill = "Relative forest\ncoverage")
p2 <- elev_map  + theme_void()
p3 <- AMT_map  + theme_void()
p4 <- AP_map  + theme_void() + labs(fill = "Annual Precipation (mm)")
p2d <- elev_distance_map  + theme_void()
p3d <- AMT_distance_map  + theme_void()
p4d <- AP_distance_map  + theme_void()
plot_grid(p2, p3, p4, p1, p2d, p3d, p4d, 
          ncol = 4, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          label_size = 12)

ggsave("../data/results/07-envi-vars-map.pdf",
       width = 11.69,
       height = 8.27
)

# Tables
OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = tidy(lm_fit))
saveWorkbook(OUT, "../data/results/07-model.xlsx", overwrite = TRUE)

# Save workspace ----
save.image("~/tcc-ccma/code/spatial-analyses.RData")
