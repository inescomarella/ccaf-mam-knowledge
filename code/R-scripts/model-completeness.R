# Do tidymodels
# Inês M. Comarella
# Date: 02/02/2021

xfun::pkg_attach(c(
  "tidyverse",
  "tidymodels",
  "brazilmaps",
  "dotwhisker",
  "magrittr",
  "bdvis",
  "sf",
  "sp"
))

longlat <- CRS("+proj=longlat +datum=WGS84")
utm <-
  CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load in data ----------------------------------------------------------------
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
  arrange(order, species) %>%
  mutate(rcrd_id = seq(1, nrow(.))) %>%
  mutate(institutionCode = ifelse(str_detect(collectionCode, "UFES") |
    str_detect(collectionCode, "LABEQ"),
  "UFES",
  ifelse(str_detect(collectionCode, "UESC"),
    "UESC",
    ifelse(str_detect(collectionCode, "USP"),
      "USP",
      ifelse(str_detect(collectionCode, "UFRRJ"),
        "UFRRJ",
        ifelse(collectionCode == "MVZ",
          "BNHM",
          ifelse(collectionCode == "MEL",
            "MEL",
            institutionCode
          )
        )
      )
    )
  )
  ))

institutes_utm <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  ) %>%
  st_transform(utm) %>%
  filter(
    !str_detect(institution_name, "Alegre"),
    !str_detect(institution_name, "Mateus")
  ) %>%
  mutate(institution_name = ifelse(institution_name == "INMA", "MBML", institution_name)) %>%
  mutate(institution_name = ifelse(str_detect(institution_name, "UFES"), "UFES", institution_name))

# Estimate completeness -----------------------------------------------------

# Data dataframe
records_df <-
  st_drop_geometry(records_longlat)

# Estimate completeness

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
    indf = records_cell_id,
    recs = 10,
    gridscale = 0.1
  )

# Merge completeness estimate and cell id
records_bdcomplete <- merge(records_cell_id, bdcompleted, all = T)

records_bdcomplete_longlat <-
  merge(records_longlat, records_bdcomplete)

# Make grid
# Grid cell area = 1000 km²
grid <-
  ccaf %>%
  st_transform(utm) %>%
  st_make_grid(cellsize = c(sqrt(1) * sqrt(10) * 10000, sqrt(1) * sqrt(10) * 10000)) %>%
  st_as_sf() %>%
  st_transform(longlat)

# Identify cells
grid$grid_id <- seq(1, nrow(grid), 1)

bdcomplete_grid <-
  st_join(grid, records_bdcomplete_longlat)

bdcomplete_grid_clipped <- st_intersection(bdcomplete_grid, ccaf)

# Predictor variables -------------------------------------------

research.institute.impact <- function(grid, institutes_points) {
  utm <-
    CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Set crs
  institutes_points <- st_transform(institutes_points, utm)
  grid <- st_transform(grid, utm)

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
          grid_id == grid_ids[j],
          institutionCode == institutes[i]
        ) %>%
        nrow()
      
      # Proximity to research institute data.frame
      dist_ij <-
        grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j],
          institutionCode == institutes[i]
        ) %>%
        st_distance(
          x = st_geometry(filter(grid, grid_id == grid_ids[j]))[1],
          y = st_geometry(institutes_points)[i]
        )
      
      # Relative contribution of a certain research institute
      rel_contrib_j <- nrec_ij / nrec_j
      
      pri_df[j, i] <- rel_contrib_j / dist_ij
    }
  }

  pri_impact <- pri_df %>%
    set_colnames(unique(institutes_points$institution_name)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>%
    
    # Sum relative contribution * proximity of each institute per grid cell
    mutate(impact = sum(c_across(-grid_id))) %>%
    select(grid_id, impact)


  merge(grid, pri_impact, all = TRUE)
}

bdcomplete_grid_clipped <-
  research.institute.impact(bdcomplete_grid_clipped, institutes_utm)

bdcomplete_grid_clipped$CU <-
  st_as_sf(bdcomplete_grid_clipped) %>%
  st_transform(longlat) %>%
  st_intersects(cus_longlat) %>%
  lengths()

# Model data ----------------------------------------------------------------

df <- bdcomplete_grid_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    nrec = n(),
    CU = unique(CU),
    impact = mean(impact, na.rm = TRUE),
    c = mean(c, na.rm = TRUE)
  ) %>%
  filter(nrec > 1)

to_remove <- df %>%
  mutate(impact = as.numeric(impact)) %>%
  filter(is.na(impact) | is.infinite(impact))
df <- anti_join(df, to_remove) %>%
  select(-grid_id)

# Fit model -----------------------------------------------------------------

# Pre-process
df_recipe <- df %>%
  recipe(nrec ~ .) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_juice <- juice(df_recipe)

lm_mod <-
  linear_reg() %>%
  set_engine("lm")

lm_fit <-
  lm_mod %>%
  fit(nrec ~ impact * CU, data = df_juice)

tidy(lm_fit)

# Predict -----------------------------------------------------------

# New data
new_points_CUp <- expand.grid(
  CU = 1,
  impact = seq(0, 5, 0.25)
)
new_points_CUa <- expand.grid(
  CU = 0,
  impact = seq(0, 5, 0.25)
)

# Predict mean
mean_pred_CUp <- predict(lm_fit, new_data = new_points_CUp)
mean_pred_CUa <- predict(lm_fit, new_data = new_points_CUa)

# Predict confidence interval
conf_int_pred_CUp <- predict(lm_fit,
  new_data = new_points_CUp,
  type = "conf_int"
)
conf_int_pred_CUa <- predict(lm_fit,
  new_data = new_points_CUa,
  type = "conf_int"
)
plot_data_CUp <-
  new_points_CUp %>%
  bind_cols(mean_pred_CUp) %>%
  bind_cols(conf_int_pred_CUp) %>%
  mutate(ID = "CU present")
plot_data_CUa <-
  new_points_CUa %>%
  bind_cols(mean_pred_CUa) %>%
  bind_cols(conf_int_pred_CUa) %>%
  mutate(ID = "CU absent")

plot_data <- bind_rows(plot_data_CUp, plot_data_CUa)

# Plot ------------------------------------------------------------------
# Plot data
nrec_impact_plot <- ggplot(
  df,
  aes(impact, nrec)
) +
  geom_jitter() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    y = "Number of records",
    x = "Research Institute impact"
  ) +
  theme_light()

nrec_CU_plot <- ggplot(
  df,
  aes(CU, nrec)
) +
  geom_jitter() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    y = "Number of records",
    x = "Conservation Unit"
  ) +
  theme_light()

nrec_completeness_plot <- df %>%
  filter(!is.na(c)) %>%
  ggplot(aes(c, nrec)) +
  geom_jitter() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    y = "Number of records",
    x = "Completeness"
  ) +
  theme_light()

# Model coefficients
model_coef_graph <- tidy(lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  ) + theme_light()

# Model predictions
model_pred_graph <- ggplot(plot_data, aes(x = impact, color = ID)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(
    ymin = .pred_lower,
    ymax = .pred_upper
  ),
  width = .2
  ) +
  labs(
    y = "Number of records",
    x = "Research Institute impact"
  ) +
  scale_color_discrete(name = element_blank()) +
  theme_light()
