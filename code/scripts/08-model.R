#Date: 07/04/2021

xfun::pkg_attach2(c("tidyverse", "openxlsx", "tidymodels", "recipes", "sf", "sp"))

source("./R-scripts/functions/funs-spatial-analyses.R")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data -------------------------
grid_data <- read_sf("../data/processed-data/grid-data.shp")

br <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br) %>%
  st_crop(
    xmax = -38.7,
    xmin = -41.87851,
    ymax = -13.00164,
    ymin = -21.30178
  )

records <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  mutate(id = seq(1, nrow(.)))

institutes <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Pre-process data --------------------------------
proximity <-
  research.institute.proximity(
    grid = st_join(records, grid_data),
    institutes_points = institutes
    )

grid_data <- merge(grid_data, proximity, all = TRUE)

df_recipe <- grid_data %>%
  st_drop_geometry() %>%
  recipe(nrec ~ proximity + CU + forest_cov + Sest) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_juice <- juice(df_recipe)

# Fit model ------------------

# Linear regression model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit model
lm_fit <-
  lm_mod %>%
  fit(nrec ~ proximity + CU + forest_cov + Sest, data = df_juice)

tidy(lm_fit)

# Predict ------------------

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

# Plot prediction ------------------
pri_prox_map <- grid_data %>%
  filter(!is.na(proximity)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = proximity)) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Proximity to collection")

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

# Export ----------------------------------------
# Table
OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = tidy(lm_fit))
saveWorkbook(OUT, "../data/results/08-model.xlsx", overwrite = TRUE)
