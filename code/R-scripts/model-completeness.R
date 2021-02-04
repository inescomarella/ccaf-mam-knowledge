# Do tidymodels
# Inês M. Comarella
# Date: 02/02/2021

xfun::pkg_attach(c(
  "tidyverse",
  "tidymodels",
  "brazilmaps",
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
                 institutionCode)
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
  mutate(institution_name = ifelse(institution_name ==  "INMA",  "MBML", institution_name)) %>%
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
grid_1t <-
  ccaf %>%
  st_transform(utm) %>%
  st_make_grid(cellsize = c(sqrt(1) * sqrt(10) * 10000, sqrt(1) * sqrt(10) * 10000)) %>%
  st_as_sf() %>%
  st_transform(longlat)

grid_2t <-
  ccaf %>%
  st_transform(utm) %>%
  st_make_grid(cellsize = c(sqrt(2) * sqrt(10) * 10000, sqrt(2) * sqrt(10) * 10000)) %>%
  st_as_sf() %>%
  st_transform(longlat)

grid_3t <-
  ccaf %>%
  st_transform(utm) %>%
  st_make_grid(cellsize = c(sqrt(3) * sqrt(10) * 10000, sqrt(3) * sqrt(10) * 10000)) %>%
  st_as_sf() %>%
  st_transform(longlat)

# Identify cells
grid_1t$grid_id <- seq(1, nrow(grid_1t), 1)
grid_2t$grid_id <- seq(1, nrow(grid_2t), 1)
grid_3t$grid_id <- seq(1, nrow(grid_3t), 1)

bdcomplete_grid_1t <-
  st_join(grid_1t, records_bdcomplete_longlat)

bdcomplete_grid_2t <-
  st_join(grid_2t, records_bdcomplete_longlat)

bdcomplete_grid_3t <-
  st_join(grid_3t, records_bdcomplete_longlat)

bdcomplete_grid_1t_clipped <- st_intersection(bdcomplete_grid_1t, ccaf)
bdcomplete_grid_2t_clipped <- st_intersection(bdcomplete_grid_2t, ccaf)
bdcomplete_grid_3t_clipped <- st_intersection(bdcomplete_grid_3t, ccaf)

# Predictor variables -------------------------------------------

research.institute.impact <- function(grid, institutes_points){
  utm <-
    CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Set crs
  institutes_points <- st_transform(institutes_points, utm)
  grid <- st_transform(grid, utm)
  
  grid_ids <- unique(grid$grid_id)

  # Get research institutes names
  institutes <- institutes_points$institution_name
  
  # Proximity to research institute data.frame
  pri_df <- data.frame()
  for (i in 1:length(institutes)) {
    
    for (j in 1:length(grid_ids)) {
      nrec_j <- grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j]
        ) %>%
        nrow()
      
      nrec_ij <-
        grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j],
          institutionCode == institutes[i]
        ) %>%
        nrow()
      
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
      
      rel_contrib_j <- nrec_ij / nrec_j
      
      pri_df[j,i] <- rel_contrib_j / dist_ij
    }
  }
  
  pri_impact <- pri_df %>% 
    set_colnames(unique(institutes_points$institution_name)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>% 
    mutate(impact = sum(c_across(-grid_id))) %>%
    select(grid_id, impact)
  
  
  merge(grid, pri_impact, all = TRUE)
}

bdcomplete_grid_1t_clipped <-
  research.institute.impact(bdcomplete_grid_1t_clipped, institutes_utm)
bdcomplete_grid_2t_clipped <-
  research.institute.impact(bdcomplete_grid_2t_clipped, institutes_utm)
bdcomplete_grid_3t_clipped <-
  research.institute.impact(bdcomplete_grid_3t_clipped, institutes_utm)

bdcomplete_grid_1t_clipped$CU <-
  st_as_sf(bdcomplete_grid_1t_clipped) %>%
  st_transform(longlat) %>%
  st_intersects(cus_longlat) %>%
  lengths()

bdcomplete_grid_2t_clipped$CU <-
  st_as_sf(bdcomplete_grid_2t_clipped) %>%
  st_transform(longlat) %>%
  st_intersects(cus_longlat) %>%
  lengths()

bdcomplete_grid_3t_clipped$CU <-
  st_as_sf(bdcomplete_grid_3t_clipped) %>%
  st_transform(longlat) %>%
  st_intersects(cus_longlat) %>%
  lengths()

# Model data ----------------------------------------------------------------

df_1t <-
  bdcomplete_grid_1t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n(),
            CU = unique(CU),
            impact = mean(impact, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>% 
  filter(nrec > 1)

df_2t <-
  bdcomplete_grid_2t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n(),
            CU = unique(CU),
            impact = mean(impact, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>% 
  filter(nrec > 1)

df_3t <-
  bdcomplete_grid_3t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n(),
            CU = unique(CU),
            impact = mean(impact, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>% 
  filter(nrec > 1)

to_remove <- df_1t %>%
  mutate(impact = as.numeric(impact)) %>%
  filter(is.na(impact) | is.infinite(impact))
df_1t <- anti_join(df_1t, to_remove) %>%
  select(-grid_id)

to_remove <- df_2t %>%
  mutate(impact = as.numeric(impact)) %>%
  filter(is.na(impact) | is.infinite(impact))
df_2t <- anti_join(df_2t, to_remove) %>%
  select(-grid_id)

to_remove <- df_3t %>%
  mutate(impact = as.numeric(impact)) %>%
  filter(is.na(impact) | is.infinite(impact))
df_3t <- anti_join(df_3t, to_remove) %>%
  select(-grid_id)

# Model ---------------------------------------------------------------------

# Pre-process
df_1t_recipe <- df_1t %>%
  recipe(nrec ~ .) %>% 
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
df_2t_recipe <- df_2t %>%
  recipe(nrec ~ .) %>% 
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
df_3t_recipe <- df_3t %>%
  recipe(nrec ~ .) %>% 
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_1t_juice <- juice(df_1t_recipe)
df_2t_juice <- juice(df_2t_recipe)
df_3t_juice <- juice(df_3t_recipe)

lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

lm_fit_1t <- 
  lm_mod %>% 
  fit(nrec ~ impact * CU, data = df_1t_juice)
lm_fit_2t <- 
  lm_mod %>% 
  fit(nrec ~ impact * CU, data = df_2t_juice)
lm_fit_3t <- 
  lm_mod %>% 
  fit(nrec ~ impact * CU, data = df_3t_juice)

tidy(lm_fit_1t)
tidy(lm_fit_2t)
tidy(lm_fit_3t)

# Predict -----------------------------------------------------------
new_points_CUp <- expand.grid(CU = 1, 
                          impact = seq(0, 5, 0.25))
new_points_CUa <- expand.grid(CU = 0, 
                              impact = seq(0, 5, 0.25))

mean_pred_CUp_1t <- predict(lm_fit_1t, new_data = new_points_CUp)
mean_pred_CUp_2t <- predict(lm_fit_2t, new_data = new_points_CUp)
mean_pred_CUp_3t <- predict(lm_fit_3t, new_data = new_points_CUp)

mean_pred_CUa_1t <- predict(lm_fit_1t, new_data = new_points_CUa)
mean_pred_CUa_2t <- predict(lm_fit_2t, new_data = new_points_CUa)
mean_pred_CUa_3t <- predict(lm_fit_3t, new_data = new_points_CUa)

conf_int_pred_CUp_1t <- predict(lm_fit_1t, 
                         new_data = new_points_CUp, 
                         type = "conf_int")
conf_int_pred_CUp_2t <- predict(lm_fit_2t, 
                             new_data = new_points_CUp, 
                             type = "conf_int")
conf_int_pred_CUp_3t <- predict(lm_fit_3t, 
                             new_data = new_points_CUp, 
                             type = "conf_int")

conf_int_pred_CUa_1t <- predict(lm_fit_1t, 
                                new_data = new_points_CUa, 
                                type = "conf_int")
conf_int_pred_CUa_2t <- predict(lm_fit_2t, 
                                new_data = new_points_CUa, 
                                type = "conf_int")
conf_int_pred_CUa_3t <- predict(lm_fit_3t, 
                                new_data = new_points_CUa, 
                                type = "conf_int")

plot_data_CUp_1t <- 
  new_points_CUp %>% 
  bind_cols(mean_pred_CUp_1t) %>% 
  bind_cols(conf_int_pred_CUp_1t) %>%
  mutate(ID = "CU present")
plot_data_CUp_2t <- 
  new_points_CUp %>% 
  bind_cols(mean_pred_CUp_2t) %>% 
  bind_cols(conf_int_pred_CUp_2t) %>%
  mutate(ID = "CU present")
plot_data_CUp_3t <- 
  new_points_CUp %>% 
  bind_cols(mean_pred_CUp_3t) %>% 
  bind_cols(conf_int_pred_CUp_3t) %>%
  mutate(ID = "CU present")

plot_data_CUa_1t <- 
  new_points_CUa %>% 
  bind_cols(mean_pred_CUa_1t) %>% 
  bind_cols(conf_int_pred_CUa_1t) %>%
  mutate(ID = "CU absent")
plot_data_CUa_2t <- 
  new_points_CUa %>% 
  bind_cols(mean_pred_CUa_2t) %>% 
  bind_cols(conf_int_pred_CUa_2t) %>%
  mutate(ID = "CU absent")
plot_data_CUa_3t <- 
  new_points_CUa %>% 
  bind_cols(mean_pred_CUa_3t) %>% 
  bind_cols(conf_int_pred_CUa_3t) %>%
  mutate(ID = "CU absent")

plot_data_1t <- bind_rows(plot_data_CUp_1t, plot_data_CUa_1t)
plot_data_2t <- bind_rows(plot_data_CUp_2t, plot_data_CUa_2t)
plot_data_3t <- bind_rows(plot_data_CUp_3t, plot_data_CUa_3t)

# Plot ------------------------------------------------------------------
# Plot data
ggplot(df_1t,
       aes(impact, nrec)) +      
  geom_jitter() +                       
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  theme_light()

ggplot(df_2t,
       aes(impact, nrec)) +      
  geom_jitter() +                       
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  theme_light()

ggplot(df_3t,
       aes(impact, nrec)) +      
  geom_jitter() +                       
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  theme_light()  


ggplot(df_1t,
       aes(CU, nrec)) +    
  geom_jitter() +                      
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Conservation Unit") +
  theme_light()

ggplot(df_2t,
       aes(CU, nrec)) +      
  geom_jitter() +                         
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Conservation Unit") +
  theme_light()

ggplot(df_3t,
       aes(CU, nrec)) +   
  geom_jitter() +                        
  geom_smooth(method = lm, se = TRUE)  + 
  labs(y = "Number of records",
       x = "Conservation Unit") +
  theme_light()


df_1t %>%
  filter(!is.na(c)) %>%
  ggplot(aes(c, nrec)) +    
  geom_jitter() +                      
  geom_smooth(method = lm, se = TRUE) + 
  labs(y = "Number of records",
       x = "Completeness") +
  theme_light()

df_2t %>%
  filter(!is.na(c)) %>%
  ggplot(aes(c, nrec)) +    
  geom_jitter() +                    
  geom_smooth(method = lm, se = TRUE) + 
  labs(y = "Number of records",
       x = "Completeness") +
  theme_light()

df_3t %>%
  filter(!is.na(c)) %>%
  ggplot(aes(c, nrec)) +    
  geom_jitter() +                         
  geom_smooth(method = lm, se = TRUE) + 
  labs(y = "Number of records",
       x = "Completeness") +
  theme_light()

# Model coefficients
model_coef_1t_graph <- tidy(lm_fit_1t) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +  theme_light()

model_coef_2t_graph <- tidy(lm_fit_2t) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +  theme_light()

model_coef_3t_graph <- tidy(lm_fit_3t) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +  theme_light()

# Model predictions
model_pred_1t_graph <- ggplot(plot_data_1t, aes(x = impact, color = ID)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  scale_color_discrete(name = element_blank()) +
  theme_light()

model_pred_2t_graph <- ggplot(plot_data_2t, aes(x = impact, color = ID)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  scale_color_discrete(name = element_blank()) +
  theme_light()

model_pred_3t_graph <- ggplot(plot_data_3t, aes(x = impact, color = ID)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Number of records",
       x = "Research Institute impact") +
  scale_color_discrete(name = element_blank()) +
  theme_light()
