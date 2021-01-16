# File purpose: Evaluate the coverage of inventory completeness along spatial, environmental, and temporal gradients
# Date: 15/01/2021
# Inês M. Comarella

# Load libraries
xfun::pkg_attach2(c("tidyverse", "sf", "sp", "bdvis", "raster", "elevatr"))

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

# Process data ---------------------------------------------------------
records_df <-
  st_drop_geometry(records_longlat)

# All recorded orders with a minimum number of records
orders <-
  records_df %>%
  select(order) %>%
  unique() %>%
  filter(order != "Sirenia", order != "Perissodactyla")

order_records_longlat <- list()
for (i in 1:nrow(orders)) {
  order_records_longlat[[i]] <-
    records_longlat %>%
    filter(order == orders$order[i])
}
names(order_records_longlat) <- as.character(orders$order)

j <- 1
records_year_longlat <- list()
for (i in seq(as.numeric(min(records_df$year, na.rm = FALSE)), 2015, 5)) {
  records_year_longlat[[j]] <-
    records_longlat %>%
    filter(year <= (i + 5))
  j <- j + 1
}

order_records_df <-
  lapply(order_records_longlat, st_drop_geometry)

records_year_df <-
  lapply(records_year_longlat, st_drop_geometry)

# bdvis data.frame standards
conf <-
  list(
    Latitude = "decimalLatitude",
    Longitude = "decimalLongitude",
    Date_collected = "eventDate",
    Scientific_name = "species"
  )

records_bdvis <- format_bdvis(records_df, config = conf)
order_records_bdvis <-
  lapply(order_records_df, format_bdvis, config = conf)
records_year_bdvis <-
  lapply(records_year_df, format_bdvis, config = conf)

records_cell_id <- getcellid(records_bdvis)
order_records_cell_id <- lapply(order_records_bdvis, getcellid)
records_year_cell_id <- lapply(records_year_bdvis, getcellid)

# Estimate completeness based on Chao2 index of species richness
bdcompleted <-
  bdcomplete(
    indf = records_cell_id,
    recs = 5,
    gridscale = 0.1
  )
order_bdcompleted <-
  lapply(order_records_cell_id,
    bdcomplete,
    recs = 5,
    gridscale = 0.1
  )
year_bdcompleted <-
  lapply(records_year_cell_id,
    bdcomplete,
    recs = 5,
    gridscale = 0.1
  )

records_bdcomplete <- merge(records_cell_id, bdcompleted, all = T)

order_records_bdcomplete <- list()
for (i in 1:length(order_records_cell_id)) {
  order_records_bdcomplete[[i]] <-
    merge(order_records_cell_id[[i]], order_bdcompleted[[i]], all = TRUE)
}
names(order_records_bdcomplete) <- names(order_records_cell_id)

records_year_bdcomplete <- list()
for (i in 1:length(records_year_cell_id)) {
  records_year_bdcomplete[[i]] <-
    merge(records_year_cell_id[[i]], year_bdcompleted[[i]], all = TRUE)
}

# Classify inventory completeness ---------------------------------------
records_bdcomplete_longlat <-
  merge(records_longlat, records_bdcomplete, by = "id") %>%
  mutate(
    completeness = ifelse(
      test = nrec >= 10 & c >= 0.5,
      yes = "Well-surveyed (low threshold)",
      no = ifelse(
        test = nrec >= 25 & c >= 0.65,
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
    merge(order_records_longlat[[i]], order_records_bdcomplete[[i]], by = "id") %>%
    mutate(
      completeness = ifelse(
        test = nrec >= 10 & c >= 0.5,
        yes = "Well-surveyed (low threshold)",
        no = ifelse(
          test = nrec >= 25 & c >= 0.65,
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

records_year_bdcomplete_longlat <- list()
for (i in 1:length(records_year_bdcomplete)) {
  records_year_bdcomplete_longlat[[i]] <-
    merge(records_year_longlat[[i]], records_year_bdcomplete[[i]], by = "id") %>%
    mutate(
      completeness = ifelse(
        test = nrec >= 10 & c >= 0.5,
        yes = "Well-surveyed (low threshold)",
        no = ifelse(
          test = nrec >= 25 & c >= 0.65,
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
for (i in 1:length(records_year_bdcomplete_longlat)) {
  year_bdcomplete_grid[[i]] <-
    st_join(grid, records_year_bdcomplete_longlat[[i]]) %>%
    mutate(completeness = ifelse(
      test = is.na(completeness),
      yes = "Not surveyed",
      no = completeness
    ))
}

# Environment ------------------------------------------------------------

# Get enviroment data
worldclim_data <- getData("worldclim", var = "bio", res = 10)

# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(1, 12)]]
names(worldclim_amt_ap) <-
  c("Annual Mean Temperature", "Annual Precipitation")

# Remove points outside study are
bdcomplete_grid_clipped <-
  st_intersection(bdcomplete_grid, ccaf)

order_bdcomplete_grid_clipped <- list()
for (i in 1:length(order_bdcomplete_grid)) {
  order_bdcomplete_grid_clipped[[i]] <-
    st_intersection(order_bdcomplete_grid[[i]], ccaf)
}

# Get elevation data
elevation_bdcomplete_grid_clipped <-
  get_elev_point(st_centroid(bdcomplete_grid_clipped), src = "aws")

order_elevation_bdcomplete_grid_clipped <- list()
for (i in 1:length(order_bdcomplete_grid_clipped)) {
  order_elevation_bdcomplete_grid_clipped[[i]] <-
    get_elev_point(st_centroid(order_bdcomplete_grid_clipped[[i]]), src = "aws")
}

# Sample points
sample <-
  elevation_bdcomplete_grid_clipped %>%
  st_centroid() %>%
  st_coordinates()

order_sample <- list()
for (i in 1:length(order_elevation_bdcomplete_grid_clipped)) {
  order_sample[[i]] <-
    order_elevation_bdcomplete_grid_clipped[[i]] %>%
    st_centroid() %>%
    st_coordinates()
}

# Extract environment data from sample points
pts_amt_ap <- extract(worldclim_amt_ap, sample)

order_pts_amt_ap <- list()
for (i in 1:length(order_sample)) {
  order_pts_amt_ap[[i]] <- extract(worldclim_amt_ap, order_sample[[i]])
}

# Convert grid to data.frame
df <- cbind.data.frame(pts_amt_ap, elevation_bdcomplete_grid_clipped)

order_df <- list()
for (i in 1:length(order_pts_amt_ap)) {
  order_df[[i]] <-
    cbind.data.frame(order_pts_amt_ap[[i]], order_elevation_bdcomplete_grid_clipped[[i]])
}
names(order_df) <- names(order_bdcompleted)


# Plot --------------------------------------------------------------
library(cowplot)

plot.inventory.completeness <- function(sf_obj) {
 sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = completeness)) +
    geom_sf(data = ccaf, fill = NA) +
    scale_fill_manual(values = c("grey", "yellow")) +
    theme_nothing()
}
all_m_inv_comp_plot <- plot.inventory.completeness(bdcomplete_grid)

order_m_inv_comp_plot <- lapply(order_bdcomplete_grid, plot.inventory.completeness)
names(order_m_inv_comp_plot) <- names(order_bdcomplete_grid)
order_m_inv_comp_plot$Artiodactyla

final_plot_together <-
  plot_grid(
    all_m_inv_comp_plot,
    order_m_inv_comp_plot[[1]],
    order_m_inv_comp_plot[[2]],
    order_m_inv_comp_plot[[3]],
    order_m_inv_comp_plot[[4]],
    order_m_inv_comp_plot[[5]],
    order_m_inv_comp_plot[[6]],
    order_m_inv_comp_plot[[7]],
    order_m_inv_comp_plot[[8]],
    order_m_inv_comp_plot[[9]],
    nrow = 2
  )

year_bdcomplete_grid[[41]] %>%
  filter(completeness != "Not surveyed") %>%
  ggplot() +
  geom_sf(size = 0, aes(fill = completeness)) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_manual(values = c("grey", "yellow")) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA)
  )


ggplot(df) +
  geom_point(aes(x = Annual.Mean.Temperature, y = Annual.Precipitation, color = completeness)) +
  scale_color_manual(values = c("black", "grey", "yellow")) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA)
  )

ggplot(df) +
  geom_point(aes(x = Annual.Mean.Temperature, y = elevation, color = completeness)) +
  scale_color_manual(values = c("black", "grey", "yellow")) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA)
  )

order_df$Primates %>%
  ggplot() +
  geom_point(aes(x = Annual.Mean.Temperature, y = Annual.Precipitation, color = completeness)) +
  scale_color_manual(values = c("black", "grey", "yellow")) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA)
  )
