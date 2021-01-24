# File purpose: Evaluate the coverage of inventory completeness along spatial, environmental, and temporal gradients
# Date: 15/01/2021
# Inês M. Comarella

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
    "dgof"
  )
)

conflicted::conflict_scout()
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ---------------------------------------------------
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

# Pre-process data ---------------------------------------------------------

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

# Estimate completeness ---------------------------------------------------

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
  bdcomplete(indf = records_cell_id,
             recs = 10,
             gridscale = 0.1)
order_bdcompleted <-
  lapply(order_records_cell_id,
         bdcomplete,
         recs = 10,
         gridscale = 0.1)
year_bdcompleted <-
  lapply(year_records_cell_id,
         bdcomplete,
         recs = 10,
         gridscale = 0.1)

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

# Classify inventory completeness ---------------------------------------
# Troia & McManamay (2016)
records_bdcomplete_longlat <-
  merge(records_longlat, records_bdcomplete) %>%
  mutate(
    completeness = ifelse(
      test = nrec >= 10 & c >= 0.6,
      yes = "Well-surveyed (low threshold)",
      no = ifelse(
        test = nrec >= 25 & c >= 0.7,
        yes = "Well-surveyed (medium threshold)",
        no = ifelse(
          test = nrec >= 50 & c >= 0.8,
          yes = "Well-surveyed (high threshold)",
          no = "Not well-surveyed"
        )
      )
    )
  )

order_records_bdcomplete_longlat <- list()
for (i in 1:length(order_records_bdcomplete)) {
  order_records_bdcomplete_longlat[[i]] <-
    merge(order_records_longlat[[i]], order_records_bdcomplete[[i]]) %>%
    mutate(
      completeness = ifelse(
        test = nrec >= 10 & c >= 0.6,
        yes = "Well-surveyed (low threshold)",
        no = ifelse(
          test = nrec >= 25 & c >= 0.7,
          yes = "Well-surveyed (medium threshold)",
          no = ifelse(
            test = nrec >= 50 & c >= 0.8,
            yes = "Well-surveyed (high threshold)",
            no = "Not well-surveyed"
          )
        )
      )
    )
}

year_records_bdcomplete_longlat <- list()
for (i in 1:length(year_records_bdcomplete)) {
  year_records_bdcomplete_longlat[[i]] <-
    merge(year_records_longlat[[i]], year_records_bdcomplete[[i]]) %>%
    mutate(
      completeness = ifelse(
        test = nrec >= 10 & c >= 0.6,
        yes = "Well-surveyed (low threshold)",
        no = ifelse(
          test = nrec >= 25 & c >= 0.7,
          yes = "Well-surveyed (medium threshold)",
          no = ifelse(
            test = nrec >= 50 & c >= 0.8,
            yes = "Well-surveyed (high threshold)",
            no = "Not well-surveyed"
          )
        )
      )
    )
}

# Make grid ---------------------------------------------------------------
grid <-
  ccaf %>%
  st_make_grid(cellsize = 0.1) %>%
  st_as_sf()

bdcomplete_grid <-
  st_join(grid, records_bdcomplete_longlat) %>%
  mutate(completeness = ifelse(
    test = is.na(completeness),
    yes = "Not surveyed",
    no = completeness
  ))

order_bdcomplete_grid <- list()
for (i in 1:length(order_records_bdcomplete_longlat)) {
  order_bdcomplete_grid[[i]] <-
    st_join(grid, order_records_bdcomplete_longlat[[i]]) %>%
    mutate(completeness = ifelse(
      test = is.na(completeness),
      yes = "Not surveyed",
      no = completeness
    ))
}
names(order_bdcomplete_grid) <-
  names(order_records_bdcomplete)

year_bdcomplete_grid <- list()
for (i in 1:length(year_records_bdcomplete_longlat)) {
  year_bdcomplete_grid[[i]] <-
    st_join(grid, year_records_bdcomplete_longlat[[i]]) %>%
    mutate(completeness = ifelse(
      test = is.na(completeness),
      yes = "Not surveyed",
      no = completeness
    ))
}

# Environment ------------------------------------------------------------

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

# Statistics -----------------------------------

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
  ks.test(as.numeric(ap_x$AP),
          as.numeric(ap_y$AP))

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

# Temporal completeness ----------------------------

# Number of cell with at least 10 records and c >= 0.6
temporal_completeness_plot <-
  df %>%
  filter(year != "NA", !is.na(year), c >= 0.6) %>%
  arrange(year) %>%
  mutate(year = as.Date(year, "%Y")) %>%
  group_by(year) %>%
  summarise(nrec = n_distinct(id)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = nrec)) +
  ylab("Number of grid cells") +
  xlab("Years") + 
  theme_light()
  

# Plot --------------------------------------------------------------

plot.inventory.completeness <- function(sf_obj) {
  sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    mutate(c = c * 100) %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = c)) +
    geom_sf(data = ccaf, fill = NA) +
    geom_sf(data = cus_longlat,
            fill = NA,
            size = 0.2) +
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
    labs(fill = element_blank())
}

# Plot orders completeness
all_m_inv_comp_plot <-  plot.inventory.completeness(bdcomplete_grid)

m_inv_comp_plot <-
  lapply(order_bdcomplete_grid, plot.inventory.completeness)

names(m_inv_comp_plot) <- names(order_bdcomplete_grid)

# Add title
for (i in 1:length(m_inv_comp_plot)) {
  m_inv_comp_plot[[i]] <-
    m_inv_comp_plot[[i]] +
    annotate(
      geom = "text",
      size = 3,
      x = -39.5,
      y = -12,
      family = "Lato",
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
    family = "Lato",
    label = "All mammals"
  ) +
  coord_sf(clip = "off") +
  theme(legend.position = "none", axis.title = element_blank())

# Reorder maps
m_inv_comp_plot <- m_inv_comp_plot[order(names(m_inv_comp_plot))]

# Plot
orders_completeness_plot <-
  wrap_plots(
    m_inv_comp_plot,
    nrow = 2,
    ncol = 4,
    widths = unit(3, 'cm')
  ) / legend +
  plot_layout(heights = c(1, .1))

ap_elev <- 
  df %>%
  mutate(c = c * 100) %>%
  ggplot() +
  geom_point(aes(x = elevation, y = Annual.Precipitation, color = c)) +
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
  geom_point(aes(x = elevation, y = Annual.Mean.Temperature, color = c)) +
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
  (ap_elev + amt_elev) / legend  +
  plot_layout(heights = c(1, .1), nrow = 2)


plot.nrec <- function(sf_obj) {
  nmax <-
    sf_obj  %>%
    st_drop_geometry() %>%
    select(nrec) %>%
    max(na.rm = T)
  
  sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = nrec)) +
    geom_sf(data = ccaf, fill = NA) +
    scale_fill_viridis(
      limits = c(1, nmax),
      breaks = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      ),
      labels = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

plot.Sobs <- function(sf_obj) {
  nmax <-
    sf_obj  %>%
    st_drop_geometry() %>%
    select(Sobs) %>%
    max(na.rm = T)
  
  sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = Sobs)) +
    geom_sf(data = ccaf, fill = NA) +
    scale_fill_viridis(
      limits = c(1, nmax),
      breaks = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      ),
      labels = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

plot.Sest <- function(sf_obj) {
  nmax <-
    sf_obj %>%
    st_drop_geometry() %>%
    select(Sest) %>%
    max(na.rm = T)
  
  sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = Sest)) +
    geom_sf(data = ccaf, fill = NA) +
    scale_fill_viridis(
      limits = c(1, nmax),
      breaks = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      ),
      labels = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

year_plots <-
  lapply(year_bdcomplete_grid, plot.inventory.completeness)
nrec_year_plots <- lapply(year_bdcomplete_grid, plot.nrec)
Sobs_year_plots <- lapply(year_bdcomplete_grid, plot.Sobs)
Sest_year_plots <- lapply(year_bdcomplete_grid, plot.Sest)
Sest_year_plots[[40]]

year_bdcomplete_grid[[40]] %>% st_drop_geometry() %>% filter(Sest > 700) %>% select(order.y) %>% unique()


year_plot_list <- list()
for (i in 1:length(year_plots)) {
  year_plot_list[[i]] <-
    wrap_plots(nrec_year_plots[[i]],
               year_plots[[i]],
               nrow = 1)
}
year_plot_list[[40]]
animation::saveGIF(
  print(year_plots),
  movie.name = "animation-nsp.gif",
  img.name = "Rplot",
  convert = "magick"
)
animation::saveGIF(
  print(year_plot_list),
  movie.name = "animation-nsp.gif",
  img.name = "Rplot",
  convert = "magick"
)
year_plot_list[[41]]

bdcomplete_grid %>%
  filter(completeness != "Not surveyed") %>%
  ggplot() +
  geom_sf(size = 0, aes(fill = c)) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_viridis() +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
bdcomplete_grid %>% st_drop_geometry() %>% filter(Sest > 300) %>% select(stateProvince.x, order.y, species) %>% unique

bdcomplete_grid %>% nrow
