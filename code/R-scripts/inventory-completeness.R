# File purpose: Evaluate the coverage of inventory completeness along spatial, environmental, and temporal gradients
# Date: 15/01/2021
# Inês M. Comarella

########################################
# To do:
# - Model
# - Research institutes in maps (if model work)
########################################

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
    "dotwhisker"
  )
)
conflicted::conflict_scout()
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")

source("./R-scripts/functions/funs-inventory-completeness.R")

# Set projections
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
  mutate(id = seq(1, nrow(.))) %>%
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

# Pre-process data ------------------------------------------------------------

# Get dataframe
records_df <-
  st_drop_geometry(records_longlat)

# Remove orders with too few records
orders <-
  records_df %>%
  select(order) %>%
  unique() %>%
  filter(
    order != "Sirenia",
    order != "Perissodactyla",
    order != "Artiodactyla",
    order != "Lagomorpha"
  )

# Separate orders
order_records_longlat <- list()
for (i in 1:nrow(orders)) {
  order_records_longlat[[i]] <-
    records_longlat %>%
    filter(order == orders$order[i])
}
names(order_records_longlat) <- as.character(orders$order)

# Separate by 5 years
j <- 1
year_records_longlat <- list()
for (i in seq(as.numeric(min(records_df$year, na.rm = FALSE)), 2015, 5)) {
  year_records_longlat[[j]] <-
    records_longlat %>%
    filter(year <= (i + 5))
  j <- j + 1
}

# Get dataframes
order_records_df <-
  lapply(order_records_longlat, st_drop_geometry)

year_records_df <-
  lapply(year_records_longlat, st_drop_geometry)

# Estimate completeness -------------------------------------------------------

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
order_records_bdvis <-
  lapply(order_records_df, format_bdvis, config = conf)
year_records_bdvis <-
  lapply(year_records_df, format_bdvis, config = conf)

# Get cell id to estimate completeness
records_cell_id <- getcellid(records_bdvis)
order_records_cell_id <- lapply(order_records_bdvis, getcellid)
year_records_cell_id <- lapply(year_records_bdvis, getcellid)

# Estimate completeness based on Chao2 index of species richness
bdcompleted <-
  bdcomplete(
   records_cell_id,
    recs = 10,
    gridscale = 0.1
  )
order_bdcompleted <-
  lapply(order_records_cell_id,
    bdcomplete,
    recs = 10,
    gridscale = 0.1
  )
year_bdcompleted <-
  lapply(year_records_cell_id,
    bdcomplete,
    recs = 10,
    gridscale = 0.1
  )

# Merge completeness estimate and cell id
records_bdcomplete <- merge(records_cell_id, bdcompleted, all = T)

order_records_bdcomplete <- list()
for (i in 1:length(order_records_cell_id)) {
  order_records_bdcomplete[[i]] <-
    merge(order_records_cell_id[[i]], order_bdcompleted[[i]], all = TRUE)
}
names(order_records_bdcomplete) <- names(order_records_cell_id)

year_records_bdcomplete <- list()
for (i in 1:length(year_records_cell_id)) {
  year_records_bdcomplete[[i]] <-
    merge(year_records_cell_id[[i]], year_bdcompleted[[i]], all = TRUE)
}

records_bdcomplete_longlat <-
  merge(records_longlat, records_bdcomplete)

order_records_bdcomplete_longlat <- list()
for (i in 1:length(order_records_bdcomplete)) {
  order_records_bdcomplete_longlat[[i]] <-
    merge(order_records_longlat[[i]], order_records_bdcomplete[[i]])
}

year_records_bdcomplete_longlat <- list()
for (i in 1:length(year_records_bdcomplete)) {
  year_records_bdcomplete_longlat[[i]] <-
    merge(year_records_longlat[[i]], year_records_bdcomplete[[i]])
}

# Make grid -------------------------------------------------------------------
grid <-
  ccaf %>%
  st_make_grid(cellsize = 0.1) %>%
  st_as_sf()

grid$grid_id <- seq(1, nrow(grid), 1)

bdcomplete_grid <-
  st_join(grid, records_bdcomplete_longlat)

order_bdcomplete_grid <- list()
for (i in 1:length(order_records_bdcomplete_longlat)) {
  order_bdcomplete_grid[[i]] <-
    st_join(grid, order_records_bdcomplete_longlat[[i]])
}
names(order_bdcomplete_grid) <-
  names(order_records_bdcomplete)

year_bdcomplete_grid <- list()
for (i in 1:length(year_records_bdcomplete_longlat)) {
  year_bdcomplete_grid[[i]] <-
    st_join(grid, year_records_bdcomplete_longlat[[i]])
}

# Environment data ------------------------------------------------------------

# Get environment data
worldclim_data <- getData("worldclim", var = "bio", res = 10)

# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(1, 12)]]
names(worldclim_amt_ap) <-
  c("AMT", "AP")

# Remove points outside study are
bdcomplete_grid_clipped <-
  st_intersection(bdcomplete_grid, ccaf)

# Get elevation data
elevation_bdcomplete_grid_clipped <-
  get_elev_point(st_centroid(bdcomplete_grid_clipped), src = "aws")

# Sample points
sample <-
  elevation_bdcomplete_grid_clipped %>%
  st_centroid() %>%
  st_coordinates()

# Extract environment data from sample points
pts_amt_ap <- extract(worldclim_amt_ap, sample)

# Convert grid to data.frame
envi_df <-
  cbind.data.frame(pts_amt_ap, elevation_bdcomplete_grid_clipped)

# Completeness statistics -----------------------------------

# Spatial: long, lat
long_x <- envi_df %>%
  filter(c >= 0.6) %>%
  select(Longitude)
long_y <- envi_df %>%
  select(Longitude)

lat_x <- envi_df %>%
  filter(c >= 0.6) %>%
  select(Latitude)
lat_y <- envi_df %>%
  select(Latitude)

# Temporal: year
year_x <- envi_df %>%
  filter(c >= 0.6, !is.na(year), year != "NA") %>%
  select(year)
year_y <- envi_df %>%
  filter(!is.na(year), year != "NA") %>%
  select(year)

# Environmental: elevation, AMT, AP
ele_x <- envi_df %>%
  filter(c >= 0.6) %>%
  select(elevation)
ele_y <- envi_df %>%
  select(elevation)

amt_x <- envi_df %>%
  filter(c >= 0.6) %>%
  select(AMT)
amt_y <- envi_df %>%
  select(AMT)

ap_x <- envi_df %>%
  filter(c >= 0.6) %>%
  select(AP)
ap_y <- envi_df %>%
  select(AP)

long_ks <-
  ks.test(as.numeric(long_x$Longitude), as.numeric(long_y$Longitude))
lat_ks <-
  ks.test(as.numeric(lat_x$Latitude), as.numeric(lat_y$Latitude))
year_ks <-
  ks.test(as.numeric(year_x$year), as.numeric(year_y$year))
ele_ks <-
  ks.test(as.numeric(ele_x$elevation), as.numeric(ele_y$elevation))
amt_ks <-
  ks.test(
    as.numeric(amt_x$AMT),
    as.numeric(amt_y$AMT)
  )
ap_ks <-
  ks.test(
    as.numeric(ap_x$AP),
    as.numeric(ap_y$AP)
  )

ks_statistics <-
  data.frame(
    "Variable" = c("Year", "Longitude", "Latitude", "elevation", "AMT", "AP"),
    "D-statistics" = rbind(
      year_ks$statistic,
      long_ks$statistic,
      lat_ks$statistic,
      ele_ks$statistic,
      amt_ks$statistic,
      ap_ks$statistic
    )
  )

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
          grid_id == grid_ids[j] &
            institutionCode == institutes[i]
        ) %>%
        nrow()
      
      # Proximity to research institute data.frame
      dist_ij <-
        st_distance(
          x = st_geometry(filter(grid, grid_id == grid_ids[j]))[1],
          y = st_geometry(institutes_points)[i]
        )
      
      # Relative contribution of a certain research institute
      rel_contrib_j <- nrec_ij / nrec_j
      
      if(is.na(as.numeric(dist_ij)) | as.numeric(dist_ij) == 0){
        dist_ij <- 1
      }
      
      pri_df[j, i] <- rel_contrib_j / as.numeric(dist_ij)
    }
  }
  
  pri_impact <- pri_df %>%
    # Remove columns with sum = 0
    select(which(!colSums(pri_df, na.rm = TRUE) %in% 0)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>%
    
    # Sum relative contribution * proximity of each institute per grid cell
    mutate(impact = sum(c_across(-c(grid_id)), na.rm = TRUE)) %>%
    select(grid_id, impact)
  
  
  merge(grid, pri_impact, by = "grid_id")
}

bdcomplete_grid_data <-
  research.institute.impact(bdcomplete_grid_clipped, institutes_utm)

bdcomplete_grid_data$CU <-
  st_as_sf(bdcomplete_grid_data) %>%
  st_transform(longlat) %>%
  st_intersects(cus_longlat) %>%
  lengths()

# Model data ----------------------------------------------------------------

model_df <- bdcomplete_grid_data %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    nrec = n(),
    CU = unique(CU),
    impact = mean(impact, na.rm = TRUE),
    c = mean(c, na.rm = TRUE)
  ) %>%
  filter(nrec > 1)

to_remove <- model_df %>%
  mutate(impact = as.numeric(impact)) %>%
  filter(is.na(impact) | is.infinite(impact))
model_df <- anti_join(model_df, to_remove) %>%
  select(-c(grid_id, c))

# Fit model -----------------------------------------------------------------

# Pre-process
df_recipe <- model_df %>%
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
  model_df,
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
  model_df,
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

# Plot orders completeness
all_m_inv_comp_plot <- plot.inventory.completeness(bdcomplete_grid)

m_inv_comp_plot <-
  lapply(order_bdcomplete_grid, plot.inventory.completeness)

names(m_inv_comp_plot) <- names(order_bdcomplete_grid)

## Add title
for (i in 1:length(m_inv_comp_plot)) {
  m_inv_comp_plot[[i]] <-
    m_inv_comp_plot[[i]] +
    annotate(
      geom = "text",
      size = 3,
      x = -39.5,
      y = -12,
      label = names(m_inv_comp_plot)[i]
    ) +
    coord_sf(clip = "off") +
    theme(legend.position = "none", axis.title = element_blank())
}

legend <- get_legend(
  all_m_inv_comp_plot +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

m_inv_comp_plot$All <-
  all_m_inv_comp_plot +
  annotate(
    geom = "text",
    size = 3,
    x = -39.5,
    y = -12,
    label = "All mammals"
  ) +
  coord_sf(clip = "off") +
  theme(legend.position = "none", axis.title = element_blank())

## Reorder maps
m_inv_comp_plot <- m_inv_comp_plot[order(names(m_inv_comp_plot))]

## Plot
orders_completeness_maps <-
  wrap_plots(
    m_inv_comp_plot,
    nrow = 2,
    ncol = 4,
    widths = unit(3, "cm")
  ) / legend +
  plot_layout(heights = c(1, .1))

# Environmental graphs
ap_elev <-
  envi_df %>%
  mutate(c = c * 100) %>%
  ggplot() +
  geom_point(aes(x = elevation, y = AP, color = c)) +
  scale_color_viridis(
    limits = c(0.5, 100),
    breaks = c(0.5, 20, 40, 60, 80, 100),
    labels = c(0.5, 20, 40, 60, 80, 100)
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA),
    legend.position = "none"
  ) +
  labs(color = element_blank()) +
  xlab("Elevation") +
  ylab("Annual Precipitation")

amt_elev <-
  envi_df %>%
  mutate(c = c * 100) %>%
  ggplot() +
  geom_point(aes(x = elevation, y = AMT, color = c)) +
  scale_color_viridis(
    limits = c(0.5, 100),
    breaks = c(0.5, 20, 40, 60, 80, 100),
    labels = c(0.5, 20, 40, 60, 80, 100)
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA),
    legend.position = "none"
  ) +
  labs(color = element_blank()) +
  xlab("Elevation") +
  ylab("Annual Mean Temperature")

environmental_plot <-
  (ap_elev + amt_elev) / legend +
  plot_layout(heights = c(1, .1), nrow = 2)

# Temporal completeness

# Number of cell with at least 10 records and c >= 0.6
minimal_completeness_years_graph <-
  envi_df %>%
  filter(year != "NA", !is.na(year), c >= 0.6, c != "", !is.na(c)) %>%
  arrange(year) %>%
  mutate(year = as.Date(year, "%Y")) %>%
  group_by(year) %>%
  summarise(completeness_y = n_distinct(grid_id)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = completeness_y)) +
  ylab("Number of grid cells") +
  xlab("Years") +
  theme_light()

nrec_y_graph <-
  envi_df %>%
  filter(year != "NA", !is.na(year)) %>%
  arrange(year) %>%
  mutate(year = as.Date(year, "%Y")) %>%
  group_by(year) %>%
  summarise(nrec_y = n_distinct(id)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = nrec_y)) +
  ylab("Number of records cells") +
  xlab("Years") +
  theme_light()

minimal_completeness_cum_graph <-
  envi_df %>%
  filter(year != "NA", !is.na(year), c >= 0.6, c != "", !is.na(c)) %>%
  arrange(year) %>%
  mutate(
    year = as.Date(year, "%Y"),
    fk_group = "a"
  ) %>%
  group_by(year) %>%
  mutate(completeness_y = n_distinct(grid_id)) %>%
  select(year, completeness_y, fk_group) %>%
  unique() %>%
  group_by(fk_group) %>%
  mutate(completeness_cum = cumsum(completeness_y)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = completeness_cum)) +
  ylab("Number of grid cells") +
  xlab("Years") +
  theme_light()


nrec_cum_graph <-
  envi_df %>%
  filter(year != "NA", !is.na(year)) %>%
  arrange(year) %>%
  mutate(
    year = as.Date(year, "%Y"),
    fk_group = "a"
  ) %>%
  group_by(year) %>%
  mutate(nrec_y = n_distinct(id)) %>%
  select(year, nrec_y, fk_group) %>%
  unique() %>%
  group_by(fk_group) %>%
  mutate(nrec_cum = cumsum(nrec_y)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = nrec_cum)) +
  ylab("Number of records") +
  xlab("Years") +
  theme_light()

mean_completeness_cum_graph <-
  envi_df %>%
  filter(year != "NA", !is.na(year), c != "", !is.na(c)) %>%
  arrange(year) %>%
  mutate(
    year = as.Date(year, "%Y"),
    fk_group = "a"
  ) %>%
  group_by(year) %>%
  mutate(mean_c = mean(c)) %>%
  select(year, mean_c, fk_group) %>%
  unique() %>%
  group_by(fk_group) %>%
  mutate(mean_c_cum = cummean(mean_c)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = mean_c_cum)) +
  ylab("Mean completeness") +
  xlab("Years") +
  theme_light()


nrec_cum_envi_df <-
  envi_df %>%
  filter(year != "NA", !is.na(year)) %>%
  arrange(year) %>%
  mutate(
    year = as.Date(year, "%Y"),
    fk_group = "a"
  ) %>%
  group_by(year) %>%
  mutate(nrec_y = n_distinct(id)) %>%
  select(year, nrec_y, fk_group) %>%
  unique() %>%
  group_by(fk_group) %>%
  mutate(nrec_cum = cumsum(nrec_y))

mean_c_cum_envi_df <-
  envi_df %>%
  filter(year != "NA", !is.na(year), c != "", !is.na(c)) %>%
  arrange(year) %>%
  mutate(
    year = as.Date(year, "%Y"),
    fk_group = "a"
  ) %>%
  group_by(year) %>%
  mutate(mean_c = mean(c)) %>%
  select(year, mean_c, fk_group) %>%
  unique() %>%
  group_by(fk_group) %>%
  mutate(mean_c_cum = cummean(mean_c))

mean_c_cum_df_slct <- mean_c_cum_envi_df %>% select(year, mean_c_cum)
nrec_cum_df_slct <- nrec_cum_envi_df %>% select(year, nrec_cum)

cum_df_mrg <- merge(mean_c_cum_df_slct, nrec_cum_df_slct)

mean_completeness_cum_rec_graph <-
  ggplot(cum_df_mrg) +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = mean_c_cum * 10000), color = "red") +
  geom_line(aes(x = year, y = nrec_cum)) +
  scale_y_continuous( # Features of the first axis
    name = "Cumulative number of records",

    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~ . / 10000, name = "Mean completeness")
  ) +
  theme_light()

# Animation -------------------------------------------------------------------

nrec_max <-
  year_bdcomplete_grid[[length(year_bdcomplete_grid)]] %>%
  st_drop_geometry() %>%
  filter(!is.na(c)) %>%
  group_by(grid_id) %>%
  summarise(nrec_count = n()) %>%
  select(nrec_count) %>%
  max()

spatial_temporal_completeness_map_animation <- list()
for (i in 1:length(year_bdcomplete_grid)) {
  comp_plot <-
    year_bdcomplete_grid[[i]] %>%
    filter(!is.na(c)) %>%
    mutate(c = c * 100) %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = c)) +
    geom_sf(data = ccaf, fill = NA) +
    geom_sf(
      data = cus_longlat,
      fill = NA,
      size = 0.2
    ) +
    scale_fill_viridis(
      limits = c(0.5, 100),
      breaks = c(0.5, 20, 40, 60, 80, 100),
      labels = c(0.5, 20, 40, 60, 80, 100)
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(fill = "Completeness")

  nrec_plot <-
    year_bdcomplete_grid[[i]] %>%
    group_by(grid_id) %>%
    summarise(nrec_count = n()) %>%
    filter(nrec_count > 1) %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = nrec_count)) +
    geom_sf(data = ccaf, fill = NA) +
    geom_sf(
      data = cus_longlat,
      fill = NA,
      size = 0.2
    ) +
    scale_fill_viridis(
      limits = c(10, nrec_max),
      breaks = c(
        10,
        round((nrec_max - 10) / 5, 0),
        round((nrec_max - 10) * 2 / 5, 0),
        round((nrec_max - 10) * 3 / 5, 0),
        round((nrec_max - 10) * 4 / 5, 0),
        nrec_max
      ),
      labels = c(
        10,
        round((nrec_max - 10) / 5, 0),
        round((nrec_max - 10) * 2 / 5, 0),
        round((nrec_max - 10) * 3 / 5, 0),
        round((nrec_max - 10) * 4 / 5, 0),
        nrec_max
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(fill = "Number of records")

  plot_row <- plot_grid(comp_plot, nrec_plot)

  y <- as.numeric(min(year_bdcomplete_grid[[i]]$year, na.rm = T)) + (i * 5)

  title <- ggdraw() +
    draw_label(
      paste0(
        as.character(min(year_bdcomplete_grid[[i]]$year, na.rm = T)),
        " - ",
        as.character(y)
      ),
      fontface = "bold",
      hjust = 1,
      size = 15
    )

  spatial_temporal_completeness_map_animation[[i]] <-
    plot_grid(title, plot_row,
      ncol = 1,
      rel_heights = c(.1, 1)
    )
}

# Save ------------------------------------------------------------------------

# Temporal map animation
saveGIF(
  print(spatial_temporal_completeness_map_animation),
  movie.name = "spatial_temporal_completeness_map_animation.gif",
  img.name = "Rplot",
  convert = "magick",
  ani.width = 800,
  ani.height = 600
)

# Map
spatial_temporal_completeness_map_animation[[length(spatial_temporal_completeness_map_animation)]]

ggsave(
  filename = "../data/results/spatial_temporal_completeness_map.pdf",
  width = 8,
  height = 6
)
ggsave(
  filename = "../data/results/spatial_temporal_completeness_map.png",
  width = 8,
  height = 6
)

# Order map
orders_completeness_maps

ggsave(
  filename = "../data/results/orders_completeness_maps.pdf",
  width = 8,
  height = 6
)
ggsave(
  filename = "../data/results/orders_completeness_maps.png",
  width = 8,
  height = 6
)

# Temporal completeness graph
mean_completeness_cum_rec_graph
ggsave(
  filename = "../data/results/mean_completeness_cum_rec_graph.pdf",
  width = 8,
  height = 6
)

minimal_completeness_years_graph
ggsave(
  filename = "../data/results/minimal_completeness_years_graph.pdf",
  width = 8,
  height = 6
)

nrec_y_graph
ggsave(
  filename = "../data/results/nrec_y_graph.pdf",
  width = 8,
  height = 6
)

nrec_cum_graph
ggsave(
  filename = "../data/results/nrec_cum_graph.pdf",
  width = 8,
  height = 6
)

minimal_completeness_cum_graph
ggsave(
  filename = "../data/results/minimal_completeness_cum_graph.pdf",
  width = 8,
  height = 6
)

mean_completeness_cum_graph
ggsave(
  filename = "../data/results/mean_completeness_cum_graph.pdf",
  width = 8,
  height = 6
)

# Environmental graphs
environmental_plot
ggsave(
  filename = "../data/results/environmental_plot.pdf",
  width = 8,
  height = 6
)

# Completeness statistics
OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = ks_statistics)


saveWorkbook(OUT, "../data/results/ks_statistics.xlsx", overwrite = TRUE)
