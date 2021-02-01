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
    "animation"
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
  mutate(id = seq(1, nrow(.)))

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
    indf = records_cell_id,
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
df <-
  cbind.data.frame(pts_amt_ap, elevation_bdcomplete_grid_clipped)

# Completeness statistics -----------------------------------

# Spatial: long, lat
long_x <- df %>%
  filter(c >= 0.6) %>%
  select(Longitude)
long_y <- df %>%
  select(Longitude)

lat_x <- df %>%
  filter(c >= 0.6) %>%
  select(Latitude)
lat_y <- df %>%
  select(Latitude)

# Temporal: year
year_x <- df %>%
  filter(c >= 0.6, !is.na(year), year != "NA") %>%
  select(year)
year_y <- df %>%
  filter(!is.na(year), year != "NA") %>%
  select(year)

# Environmental: elevation, AMT, AP
ele_x <- df %>%
  filter(c >= 0.6) %>%
  select(elevation)
ele_y <- df %>%
  select(elevation)

amt_x <- df %>%
  filter(c >= 0.6) %>%
  select(AMT)
amt_y <- df %>%
  select(AMT)

ap_x <- df %>%
  filter(c >= 0.6) %>%
  select(AP)
ap_y <- df %>%
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

# Model -----------------------------------------------------------------------
# c ~ UC + nrec + dist + area
# nrec ~ UC + dist + area

bdcomplete_grid_clipped <- st_intersection(bdcomplete_grid, ccaf)

bdcomplete_grid_clipped <-
  get.nearest.dist(institutes_utm, st_transform(bdcomplete_grid_clipped, utm))

st_geometry(bdcomplete_grid_clipped) <- st_geometry(st_intersection(bdcomplete_grid, ccaf))

bdcomplete_grid_clipped$CU <-
  st_as_sf(bdcomplete_grid_clipped) %>%
  st_intersects(cus_longlat) %>%
  lengths()

df_model <-
  bdcomplete_grid_clipped %>%
  group_by(grid_id) %>%
  select(dist_inst) %>%
  st_drop_geometry()

df_model[, 2] <-
  bdcomplete_grid_clipped %>%
  group_by(grid_id) %>%
  select(CU) %>%
  st_drop_geometry()

df_model[, 3] <-
  bdcomplete_grid_clipped %>%
  group_by(grid_id) %>%
  mutate(nrec = n()) %>%
  select(nrec) %>%
  st_drop_geometry()

df_model[, 4] <-
  bdcomplete_grid_clipped %>%
  group_by(grid_id) %>%
  select(c) %>%
  st_drop_geometry() %>%
  mutate(c = ifelse(is.na(c), 0, c))

df_model <-
  df_model %>% filter(nrec > 1)

summary(
  glm_fitted_c <-
    MASS::glm.nb(
      c ~ dist_inst + CU + nrec,
      data = df_model,
    )
)

summary(
  glm_fitted_nrec <-
    MASS::glm.nb(
      nrec ~ dist_inst + CU,
      data = df_model,
    )
)

summary(
  glm_fitted_c <-
    glm(
      c ~ dist_inst + CU + nrec,
      data = df_model,
    )
)

summary(
  glm_fitted_nrec <-
    glm(
      nrec ~ dist_inst + CU,
      data = df_model,
    )
)


library(DHARMa)
par(mar = c(1, 1, 1, 1))

# Conventional Residuals (fittedModel)
glm_simulation_c <- simulateResiduals(glm_fitted_c, plot = T)
glm_simulation_nrec <- simulateResiduals(glm_fitted_nrec, plot = T)

# Detect possible misspecifications
plotResiduals(glm_simulation_c, df_model$dist_inst)
plotResiduals(glm_simulation_c, df_model$CU)
plotResiduals(glm_simulation_c, df_model$nrec)

# Test overdispersion
testDispersion(glm_simulation_nrec, alternative = "greater")

# Test if there are more zeros than expected
testZeroInflation(glm_simulation_nrec, alternative = "greater")

# Plot ------------------------------------------------------------------------

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
  df %>%
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
  df %>%
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
  df %>%
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
  df %>%
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
  df %>%
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
  df %>%
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
  df %>%
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


nrec_cum_df <-
  df %>%
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

mean_c_cum_df <-
  df %>%
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

mean_c_cum_df_slct <- mean_c_cum_df %>% select(year, mean_c_cum)
nrec_cum_df_slct <- nrec_cum_df %>% select(year, nrec_cum)

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
