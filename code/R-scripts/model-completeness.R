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
            ifelse(str_detect(institutionCode, "UFRJ"),
                   "MNRJ",
                   institutionCode
            )
          )
        )
      )
    )
  )
  )) %>%
  filter(collectionCode != "LABEQ")

institutes_longlat <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Pre-process map -----------------------------------------------------

# Make grid
grid <-
  ccaf %>% 
  st_make_grid(cellsize = 0.1) %>%
  st_as_sf()

# Identify cells
grid$grid_id <- seq(1, nrow(grid), 1)

records_grid <-
  st_join(grid, records_longlat)

records_grid_clipped <- st_intersection(records_grid, ccaf)

# Predictor variables -------------------------------------------

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
        st_centroid() %>% head(1)

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

data_grid <-
  research.institute.distance(records_grid_clipped, institutes_longlat)

data_grid$area <-
  data_grid %>%
  st_area() %>%
  as.numeric()

data_grid$CU <-
  data_grid %>%
  st_intersects(cus_longlat) %>%
  lengths()

# Model data ----------------------------------------------------------------

df <- data_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    nrec = n(),
    CU = unique(CU),
    distance = mean(impact, na.rm = TRUE),
    area = unique(area)
  )


df <- df %>%
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
  fit(nrec ~ area + distance + CU, data = df_juice)

tidy(lm_fit)

# Predict -----------------------------------------------------------

# New data
new_points_CUp <- expand.grid(
  CU = 1,
  distance = seq(0, 5, 0.25),
  area = max(df_juice$area)
)
new_points_CUa <- expand.grid(
  CU = 0,
  distance = seq(0, 5, 0.25),
  area = max(df_juice$area)
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
  aes(distance, nrec)
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


# Model coefficients
model_coef_graph <- tidy(lm_fit) %>%
  dwplot(
    dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
  ) + theme_light()

# Model predictions
model_pred_graph <- ggplot(plot_data, aes(x = distance, color = ID)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(
    ymin = .pred_lower,
    ymax = .pred_upper
  ),
  width = .2
  ) +
  labs(
    y = "Number of records",
    x = "Research Institute distance"
  ) +
  scale_color_discrete(name = element_blank()) +
  theme_light()
