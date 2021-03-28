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

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

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
      clean_data, KM == max(clean_data$KM, na.rm = TRUE)
    )$elev),
    AMT_weight = abs(AMT - filter(
      clean_data, KM == max(clean_data$KM, na.rm = TRUE)
    )$AMT),
    AP_weight = abs(AP - filter(
      clean_data, KM == max(clean_data$KM, na.rm = TRUE)
    )$AP)
  )

processed_data <- processed_data %>%
  mutate(
    elev_weight = elev_weight / max(processed_data$elev_weight, na.rm = TRUE),
    AMT_weight = AMT_weight / max(processed_data$AMT_weight, na.rm = TRUE),
    AP_weight = AP_weight / max(processed_data$AP_weight, na.rm = TRUE)
  ) %>%
  mutate(KG = forest_perc * mean(c(elev_weight, AMT_weight, AP_weight, (1 - KM))))


# Variable maps ----

processed_data <- processed_data %>%
  mutate(KL = ifelse(
    KM > 0.8,
    "Very high",
    ifelse(
      KM > 0.6 & KM < 0.8,
      "High",
      ifelse(
        KM > 0.4 & KM < 0.6,
        "Medium",
        ifelse(
          KM > 0.2 & KM < 0.4,
          "Low",
          "Very low"
        )
      )
    )
  )) %>%
  mutate(GKL = ifelse(
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
  ))

levels <- c("Very high", "High", "Medium", "Low", "Very low")

processed_data$KL <-
  factor(processed_data$KL,
         levels = levels)
processed_data$GKL <-
  factor(processed_data$GKL,
         levels = levels)

KL_map <- processed_data %>%
  ggplot() +
  geom_sf(aes(fill = KL), color = NA) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus",
                  discrete = TRUE,
                  direction = -1) +
  theme_light() +
  labs(fill = "Knowledge level")

GKL_map <- processed_data %>%
  filter(!is.na(GKL)) %>%
  ggplot() +
  geom_sf(aes(fill = GKL), color = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus",
                  discrete = TRUE,
                  direction = -1) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Gap in knowledge level")

nrec_map <- processed_data %>%
  filter(nrec > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = nrec)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Number of records")

c_map <- processed_data %>%
  filter(!is.na(c)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = c)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "c-value")

CU_map <- processed_data %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggplot() +
  geom_sf(color = NA, aes(fill = CU)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus", discrete = TRUE) +
  theme_light() +
  labs(fill = "Conservation Unit")

forest_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = forest_area)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Forest coverage")

Sest_map <- processed_data %>%
  filter(!is.na(Sest)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sest)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

elev_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elev)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Elevation")

AMT_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMT)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Annual Mean \nTemperature")

AP_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Annual Precipitation")

distance_map <- processed_data %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = distance)) +
  geom_sf(data = cus_longlat, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Proximity to collection")

# Variables graphs ----

nrec_c_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggscatter(
    x = "nrec", y = "c",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light()

KG_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggscatter(
    x = "KG", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

nrec_graph <- processed_data %>%
  mutate(nrec = ifelse(is.na(nrec), 0, nrec)) %>%
  ggscatter(
    x = "nrec", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

c_graph <- processed_data %>%
  ggscatter(
    x = "c", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

CU_graph <- processed_data %>%
  ggscatter(
    x = "CU", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

forest_graph <- processed_data %>%
  ggscatter(
    x = "forest_area", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

Sest_graph <- processed_data %>%
  ggscatter(
    x = "Sest", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

elev_graph <- processed_data %>%
  ggscatter(
    x = "elev", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

AMT_graph <- processed_data %>%
  ggscatter(
    x = "AMT", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

AP_graph <- processed_data %>%
  ggscatter(
    x = "AP", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

distance_graph <- processed_data %>%
  ggscatter(
    x = "distance", y = "KM",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light()

# Fit model ----

# Pre-process
df_recipe <- processed_data %>%
  st_drop_geometry() %>%
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
  fit(nrec ~ distance + CU + forest_perc + Sest, data = df_juice)

tidy(lm_fit)

# Predict ----

# New data
new_points_CUp_dist <- expand.grid(
  CU = 1,
  distance = seq(-5, 5, 0.2),
  forest_perc = mean(df_juice$forest_perc, na.rm = TRUE),
  Sest = max(df_juice$Sest, na.rm = TRUE)
)
new_points_CUp_Sest <- expand.grid(
  CU = 1,
  distance = mean(df_juice$distance, na.rm = TRUE),
  forest_perc = mean(df_juice$forest_perc, na.rm = TRUE),
  Sest = seq(-5, 5, 0.2)
)
new_points_CUa_dist <- expand.grid(
  CU = 0,
  distance = seq(-5, 5, 0.2),
  forest_perc = mean(df_juice$forest_perc, na.rm = TRUE),
  Sest = mean(df_juice$Sest, na.rm = TRUE)
)
new_points_CUa_Sest <- expand.grid(
  CU = 0,
  distance = mean(df_juice$distance, na.rm = TRUE),
  forest_perc = mean(df_juice$forest_perc, na.rm = TRUE),
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


records_df <- records_longlat %>%
  st_drop_geometry() %>%
  mutate(Collection = ifelse(institutionCode == "UFES" | str_detect(institutionCode, "CEPLAC") | institutionCode == "MEL" | institutionCode == "UESC" | institutionCode == "MBML",
    "Local collections",
    ifelse(institutionCode == "",
      "Scientific literature",
      ifelse(institutionCode == "KU" | institutionCode == "LACM" | institutionCode == "USNM" | institutionCode == "BNHM" | institutionCode == "MCZ" | institutionCode == "ROM" | institutionCode == "FMNH" | institutionCode == "CLO" | institutionCode == "UMMZ",
        "International collections",
        ifelse(str_detect(institutionCode, "iNat"),
          "iNaturalist",
          "National collections"
        )
      )
    )
  ))

level_order <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Collection)

data <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  mutate(per = n / sum(n))

data$label <- scales::percent(data$per)
data %>%
  filter(per < 0.2) %>%
  select(per) %>% sum()

pie_chart <- data %>% 
  ggplot() +
  geom_bar(aes(
    x = "",
    y = n,
    fill = factor(Collection, levels = level_order$Collection)
  ),
  stat = "identity",
  width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = element_blank())

line_graph_collections <- records_df %>%
  filter(year != "NA") %>%
  mutate(fk_group = "a") %>%
  mutate(year = as.Date(year, "%Y")) %>%
  group_by(fk_group, Collection, year) %>%
  summarise(n = n()) %>%
  mutate(ncum = cumsum(n)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = ncum, color = factor(Collection, levels = level_order$Collection))) +
  theme_light() +
  labs(color = element_blank(),
       x = element_blank(),
       y = "Cumulative number of records")

# Save results -------------
pie_chart
ggsave("../data/results/spa-mea-pie-chart.pdf",
       width = 8,
       height = 6
)
line_graph_collections
ggsave("../data/results/spa-mea-line-graph-collections.pdf",
       width = 8,
       height = 6
)

KL_map  + theme_void() + (nrec_map  + theme_void() / c_map  + theme_void())
ggsave("../data/results/spa-mea-KL-nrec-c-map.pdf",
       width = 11.69,
       height = 8.27
)

GKL_map + theme_void() + (forest_map + theme_void() + elev_map + theme_void()) / (AMT_map + theme_void() + AP_map + theme_void())
ggsave("../data/results/spa-mea-environment-map.pdf",
       width = 11.69,
       height = 8.27
)
conflicted::conflict_prefer("get_legend", "cowplot")

legend <- get_legend(predict_collection)
predict_collection_nl <- predict_collection + theme(legend.position = "none")
predict_SR_nl <- predict_SR + theme(legend.position = "none")

predict_collection_nl + predict_SR_nl + legend
ggsave("../data/results/spa-mea-predict.pdf",
       width = 8,
       height = 4
)


# Tables
OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = tidy(lm_fit))
saveWorkbook(OUT, "../data/results/spa-mea-model.xlsx", overwrite = TRUE)

# Save workspace ----
save.image("~/tcc-ccma/code/spatial-measurements.RData")
