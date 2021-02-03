# Do tidymodels
# Inês M. Comarella
# Date: 02/02/2021

xfun::pkg_attach(c(
  "tidyverse",
  "tidymodels",
  "brazilmaps",
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
  mutate(rcrd_id = seq(1, nrow(.)))

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
  )

# Pre-process data ------------------------------------------------------------

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

bdcomplete_grid_1t_clipped_frst <- st_intersection(bdcomplete_grid_1t, ccaf)
bdcomplete_grid_2t_clipped_frst <- st_intersection(bdcomplete_grid_2t, ccaf)
bdcomplete_grid_3t_clipped_frst <- st_intersection(bdcomplete_grid_3t, ccaf)

source("./R-scripts/functions/07-funs-scatter-graph.R")

bdcomplete_grid_1t_clipped <-
  get.nearest.dist(institutes_utm,
                   st_transform(bdcomplete_grid_1t_clipped_frst, utm))
bdcomplete_grid_2t_clipped <-
  get.nearest.dist(institutes_utm,
                   st_transform(bdcomplete_grid_2t_clipped_frst, utm))
bdcomplete_grid_3t_clipped <-
  get.nearest.dist(institutes_utm,
                   st_transform(bdcomplete_grid_3t_clipped_frst, utm))

st_geometry(bdcomplete_grid_1t_clipped) <- 
  st_geometry(bdcomplete_grid_1t_clipped_frst)

st_geometry(bdcomplete_grid_2t_clipped) <- 
  st_geometry(bdcomplete_grid_2t_clipped_frst)

st_geometry(bdcomplete_grid_3t_clipped) <- 
  st_geometry(bdcomplete_grid_3t_clipped_frst)

bdcomplete_grid_1t_clipped$CU <-
  st_as_sf(bdcomplete_grid_1t_clipped) %>%
  st_intersects(cus_longlat) %>%
  lengths()

bdcomplete_grid_2t_clipped$CU <-
  st_as_sf(bdcomplete_grid_2t_clipped) %>%
  st_intersects(cus_longlat) %>%
  lengths()

bdcomplete_grid_3t_clipped$CU <-
  st_as_sf(bdcomplete_grid_3t_clipped) %>%
  st_intersects(cus_longlat) %>%
  lengths()

# Dataframe -------------------------------------------------------------------
# c ~ UC + nrec + dist + area
# nrec ~ UC + dist + area

df_1t <-
  bdcomplete_grid_1t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(dist_inst = unique(dist_inst))

df_2t <-
  bdcomplete_grid_2t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(dist_inst = unique(dist_inst))
df_3t <-
  bdcomplete_grid_3t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(dist_inst = unique(dist_inst))

df_1t[, 3] <-
  bdcomplete_grid_1t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(CU = unique(CU)) %>%
  ungroup() %>%
  select(CU)
df_2t[, 3] <-
  bdcomplete_grid_2t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(CU = unique(CU)) %>%
  ungroup() %>%
  select(CU)
df_3t[, 3] <-
  bdcomplete_grid_3t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(CU = unique(CU)) %>%
  ungroup() %>%
  select(CU)

df_1t[, 4] <-
  bdcomplete_grid_1t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n()) %>%
  ungroup() %>%
  select(nrec)
df_2t[, 4] <-
  bdcomplete_grid_2t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n()) %>%
  ungroup() %>%
  select(nrec)
df_3t[, 4] <-
  bdcomplete_grid_3t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(nrec = n()) %>%
  ungroup() %>%
  select(nrec)

df_1t[, 5] <-
  bdcomplete_grid_1t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>% 
  summarise(c = mean(c, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c)
df_2t[, 5] <-
  bdcomplete_grid_2t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>% 
  summarise(c = mean(c, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c)
df_3t[, 5] <-
  bdcomplete_grid_3t_clipped %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>% 
  summarise(c = mean(c, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c)

df_1t <-
  df_1t %>% filter(nrec > 1)
df_2t <-
  df_2t %>% filter(nrec > 1)
df_3t <-
  df_3t %>% filter(nrec > 1)

# Plot -----------------------------------------------------------------------

df_3t %>%
  ggplot(aes(x = nrec, y = c)) +
  geom_point()
df_3t %>%
  ggplot(aes(x = CU, y = c)) +
  geom_point()
df_3t %>%
  ggplot(aes(x = dist_inst, y = c)) +
  geom_point()

df_3t %>%
  ggplot(aes(x = c, y = nrec)) +
  geom_point()
df_3t %>%
  ggplot(aes(x = CU, y = nrec)) +
  geom_point()
df_3t %>%
  ggplot(aes(x = dist_inst, y = nrec, color = CU)) +
  geom_point()

# Model -----------------------------------------------------------------------
df_3t_split <- initial_split(df_3t)

df_3t_recipe <- training(df_3t_split) %>%
  recipe(nrec ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_3t_training <- juice(df_3t_recipe)

iris_rf <- 
  rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") %>%
  fit(nrec ~ ., data = df_3t_training)
