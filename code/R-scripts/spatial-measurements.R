# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "bdvis",
    "raster",
    "elevatr",
    "patchwork",
    "viridis",
    "cowplot",
    "dgof",
    "openxlsx",
    "animation",
    "recipes",
    "tidymodels",
    "dotwhisker",
    "geobgu",
    "stars"
  )
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")
conflicted::conflict_prefer("raster_extract", "geobgu")

source("./R-scripts/functions/funs-inventory-completeness.R")

research.institute.distance <- function(grid, institutes_points) {

  # Set crs
  institutes_points <- st_transform(institutes_points, longlat)
  grid <- st_transform(grid, longlat)

  grid_ids <- unique(grid$grid_id)

  # Get research institutes names
  institutes <- institutes_points$institution_name

  pri_df <- data.frame()

  for (i in 1:length(institutes)) {
    for (j in 1:length(grid_ids)) {

      # Total number of records per grid cell
      nrec_j <- grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j]
        ) %>%
        nrow()

      # Number of records from a certain research institute per grid cell
      nrec_ij <-
        grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j] &
            institutionCode == institutes[i]
        ) %>%
        nrow()

      grid_centroid <- grid %>%
        filter(grid_id == grid_ids[j]) %>%
        st_centroid() %>%
        head(1)

      # Proximity to research institute data.frame
      dist_ij <-
        st_distance(
          x = grid_centroid,
          y = institutes_points[i, ],
          which = "Great Circle"
        )

      # Relative contribution of a certain research institute
      rel_contrib_j <- nrec_ij / nrec_j

      # if(is.na(as.numeric(dist_ij)) | as.numeric(dist_ij) == 0){
      #  dist_ij <- 0.05
      # }

      pri_df[j, i] <- rel_contrib_j / as.numeric(dist_ij)
    }
  }

  pri_impact <- pri_df %>%
    # Remove columns with sum = 0
    select(which(!colSums(pri_df, na.rm = TRUE) %in% 0)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>%

    # Sum relative contribution * proximity of each institute per grid cell
    mutate(distance = sum(c_across(-c(grid_id)), na.rm = TRUE)) %>%
    select(grid_id, distance)


  merge(grid, pri_impact, by = "grid_id")
}

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data --------------------------------------------------------------
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

forest <-
  read_sf("../data/processed-data/forest-area.shp") %>%
  st_transform(longlat)

# Make grid ----
grid <-
  ccaf %>%
  st_make_grid(cellsize = 0.1) %>%
  st_as_sf()

grid$grid_id <- seq(1, nrow(grid), 1)

# Measure forest coverage ----
int <- as_tibble(st_intersection(forest, grid))
int$forest_area <- st_area(int$geometry)

int_grouped <- int %>%
  group_by(grid_id) %>%
  summarise(forest_area = as.numeric(sum(forest_area)))

grid_forest <- merge(grid, int_grouped, by = "grid_id")

# Get environment data ----
# Get elevation data
ccaf_elevation <-
  get_elev_raster(ccaf, z = 10, clip = "locations")

# Get environment data
worldclim_data <- getData("worldclim", var = "bio", res = 2.5)

# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(1, 12)]]
names(worldclim_amt_ap) <-
  c("AMT", "AP")

# Extract environment data from sample points
ccaf_environmet <- mask(crop(worldclim_amt_ap, ccaf), ccaf)

# Estimate completeness ----

# Get dataframe
records_df <-
  st_drop_geometry(records_longlat)

# bdvis dataframe standards
conf <-
  list(
    Latitude = "decimalLatitude",
    Longitude = "decimalLongitude",
    Date_collected = "eventDate",
    Scientific_name = "species"
  )

# Format dataframe to bdvis standards
records_bdvis <- format_bdvis(records_df, config = conf)

# Get cell id to estimate completeness
records_cell_id <- getcellid(records_bdvis)

# Estimate completeness based on Chao2 index of species richness
bdcompleted <-
  bdcomplete(
    records_cell_id,
    recs = 25,
    gridscale = 0.1
  )

# Merge completeness estimate and cell id
records_bdcomplete <- merge(records_cell_id, bdcompleted, all = T)

records_bdcomplete_longlat <-
  merge(records_longlat, records_bdcomplete)

# Measurements ----

grid_envi <- grid_forest %>% 
  mutate(
    AMT = raster_extract(
      x = st_as_stars(ccaf_environmet$AMT),
      y = grid_forest,
      fun = mean,
      na.rm = TRUE
    ),
    AP = raster_extract(
      x = st_as_stars(ccaf_environmet$AP),
      y = grid_forest,
      fun = mean,
      na.rm = TRUE
    ),
    elev = raster_extract(
      x = st_as_stars(ccaf_elevation),
      y = grid_forest,
      fun = mean,
      na.rm = TRUE
    )
  )

grid_envi_rec <-  st_join(grid_envi, records_bdcomplete_longlat) %>%
  group_by(grid_id) %>%
  mutate(nrec = n())

grid_envi_rec_ri <-
  research.institute.distance(grid_envi_rec, institutes_utm)

grid_envi_rec_ri$CU <-  grid_envi_rec_ri %>%
  st_intersects(cus_longlat) %>%
  lengths()

clean_data <-  grid_envi_rec_ri %>%
  group_by(grid_id) %>%
  mutate(
    distance = mean(distance, na.rm = TRUE),
    c = mean(c),
    Sest = mean(Sest),
    KM = (c + nrec / max(grid_envi_rec_ri$nrec, na.rm = TRUE)) / 2,
    forest_perc = forest_area / as.numeric(st_area(geometry))
  ) %>%
  mutate(KM = ifelse(is.na(KM) | is.infinite(KM), 0, KM)) %>%
  select(KM, nrec, c, CU, forest_area, forest_perc, Sest, elev, AMT, AP, distance, grid_id) %>%
  distinct(grid_id, .keep_all = TRUE)

processed_data <- clean_data %>%
  mutate(
    elev_weight = abs(elev - filter(
      test_data, KM == max(test_data$KM, na.rm = TRUE)
    )$elev),
    AMT_weight = abs(AMT - filter(
      test_data, KM == max(test_data$KM, na.rm = TRUE)
    )$AMT),
    AP_weight = abs(AP - filter(
      test_data, KM == max(test_data$KM, na.rm = TRUE)
    )$AP)
  )

processed_data <- processed_data %>%
  mutate(
    elev_weight = elev_weight / max(test_data$elev_weight, na.rm = TRUE),
    AMT_weight = AMT_weight / max(test_data$AMT_weight, na.rm = TRUE),
    AP_weight = AP_weight / max(test_data$AP_weight, na.rm = TRUE)
  ) %>%
  mutate(KG = forest_perc * mean(c(elev_weight, AMT_weight, AP_weight, (1 - KM))))


# Variable maps ----

KG_map <- processed_data %>%
  ggplot() +
  geom_sf(aes(fill = KG), size = NA) +
  scale_fill_viridis() +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Gap in knowledge level")

KM_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = KM)) +
  scale_fill_viridis() +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Knowledge level")

nrec_map <- processed_data %>%
  filter(nrec > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = nrec)) +
  scale_fill_viridis() +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Number of records")

c_map <- processed_data %>%
  filter(!is.na(c)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = c)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "c-value")

CU_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = CU)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Conservation Unit presence")

forest_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = forest_area)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Forest coverage")

Sest_map <- processed_data %>%
  filter(!is.na(Sest)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sest)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light()


elev_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elev)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Elevation")

AMT_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMT)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Annual Mean Temperature")

AP_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Annual Precipitation")

distance_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = distance)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme_light() +
  labs(fill = "Proximity to collection")

# Variables graphs ----

KG_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = KG, y = KM)) +
  geom_point() +
  geom_smooth()

nrec_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = nrec, y = KM)) +
  geom_point() +
  geom_smooth()

c_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = c, y = KM)) +
  geom_point() +
  geom_smooth()

CU_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = CU, y = KM)) +
  geom_point() +
  geom_smooth()

forest_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = forest_area, y = KM)) +
  geom_point() +
  geom_smooth()

Sest_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = Sest, y = KM)) +
  geom_point() +
  geom_smooth()

elev_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = elev, y = KM)) +
  geom_point()

AMT_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = AMT, y = KM)) +
  geom_point()

AP_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = AP, y = KM)) +
  geom_point()

distance_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggplot(aes(x = distance, y = KM)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Proximity to Collection")

# Fit model ----

# Pre-process
df_recipe <- processed_data %>%
  st_drop_geometry() %>%
  select(-forest_perc) %>%
  recipe(nrec ~ .) %>%
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
  fit(nrec ~ distance + CU + forest_area + Sest, data = df_juice)

tidy(lm_fit)

# Predict ----

# New data
new_points_CUp_dist <- expand.grid(
  CU = 1,
  distance = seq(-5, 5, 0.2),
  forest_area = mean(df_juice$forest_area, na.rm = TRUE),
  Sest = max(df_juice$Sest, na.rm = TRUE)
)
new_points_CUp_Sest <- expand.grid(
  CU = 1,
  distance = mean(df_juice$distance, na.rm = TRUE),
  forest_area = mean(df_juice$forest_area, na.rm = TRUE),
  Sest = seq(-5, 5, 0.2)
)
new_points_CUa_dist <- expand.grid(
  CU = 0,
  distance = seq(-5, 5, 0.2),
  forest_area = mean(df_juice$forest_area, na.rm = TRUE),
  Sest = mean(df_juice$Sest, na.rm = TRUE)
)
new_points_CUa_Sest <- expand.grid(
  CU = 0,
  distance = mean(df_juice$distance, na.rm = TRUE),
  forest_area = mean(df_juice$forest_area, na.rm = TRUE),
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
ggplot(plot_data_dist, aes(x = distance, color = ID)) +
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

ggplot(plot_data_Sest, aes(x = Sest, color = ID)) +
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



level_order <- records_longlat %>%
  st_drop_geometry() %>%
  mutate(Collection = ifelse(institutionCode == "UFES" | str_detect(institutionCode, "CEPLAC") | institutionCode == "MEL" | institutionCode == "UESC" | institutionCode == "MBML",
    "Local",
    ifelse(institutionCode == "",
      "Published data",
      ifelse(institutionCode == "KU" | institutionCode == "LACM" | institutionCode == "USNM" | institutionCode == "BNHM" | institutionCode == "MCZ" | institutionCode == "ROM" | institutionCode == "FMNH" | institutionCode == "CLO" | institutionCode == "UMMZ",
        "International",
        "Brazilian"
      )
    )
  )) %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Collection)

records_longlat %>%
  st_drop_geometry() %>%
  mutate(Collection = ifelse(institutionCode == "UFES" | str_detect(institutionCode, "CEPLAC") | institutionCode == "MEL" | institutionCode == "UESC" | institutionCode == "MBML",
    "Local",
    ifelse(institutionCode == "",
      "Published data",
      ifelse(institutionCode == "KU" | institutionCode == "LACM" | institutionCode == "USNM" | institutionCode == "BNHM" | institutionCode == "MCZ" | institutionCode == "ROM" | institutionCode == "FMNH" | institutionCode == "CLO" | institutionCode == "UMMZ",
        "International",
        "Brazilian"
      )
    )
  )) %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = factor(Collection, levels = level_order$Collection), y = n), stat = "identity") +
  labs(x = "Collections", y = "Number of records") +
  theme_light()

# Save workspace ----
save.image("~/tcc-ccma/code/spatial-measurements.RData")
