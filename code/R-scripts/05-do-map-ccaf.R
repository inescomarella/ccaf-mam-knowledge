# File purpose: Make CCAF first map
# Date: 13/12/2020

# Load libraries
xfun::pkg_attach(c(
  "tidyverse",
  "sf",
  "brazilmaps",
  "raster",
  "ggspatial",
  "cowplot",
  "rworldmap",
  "grid"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Set projection
longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load data --------------------------------------------------
br_longlat <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf_spatial <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br_longlat) %>%
  st_crop(xmax = -38.7, xmin = -41.87851, ymax = -13.00164, ymin = -21.30178) %>%
  as(., "Spatial")

land_use_spdf <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(ccaf_spatial) %>%
  mask(ccaf_spatial) %>%
  as(., "SpatialPixelsDataFrame") %>%
  as.data.frame()

land_use_table_df <-
  read.csv(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-area.csv"
  )

ccaf_all_area_sf <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat)

cus_sf <-
  read_sf("../data/processed-data/CUs-map-full.shp") %>%
  st_transform(longlat)

institute_sf <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Pre-process data -------------------------------------------

land_use_table_df <-
  land_use_table_df %>%
  mutate(id = ifelse(class == 0,
    "Others",
    ifelse(
      class < 10,
      "Forest",
      ifelse(
        class > 10 & class < 14 | class == 29 | class == 32,
        "Others",
        ifelse(
          class == 15,
          "Pasture",
          ifelse(
            class > 16 & class < 21 | class == 41 | class == 39 | class == 36,
            "Agriculture",
            ifelse(
              class == 21,
              "Mosaic of Agriculture \nand Pasture",
              ifelse(
                class > 22 & class < 26 | class == 30,
                "Others",
                ifelse(class == 31 | class == 33,
                  "Water",
                  ""
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  mutate(value = class) %>%
  select(value, id, class_name)

colnames(land_use_spdf) <- c("value", "x", "y")

land_use_spdf <-
  merge(land_use_spdf, land_use_table_df)

land_use_spdf$id <-
  factor(
    land_use_spdf$id,
    levels = c(
      "Forest",
      "Agriculture",
      "Pasture",
      "Mosaic of Agriculture \nand Pasture",
      "Water",
      "Others"
    )
  )

brazil_cropped <-
  get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat) %>%
  st_crop(st_bbox(ccaf_all_area_sf))

ccaf_marine_area_sf <-
  st_difference(ccaf_all_area_sf, st_as_sf(ccaf_spatial))

south_america_sf <-
  getMap()[-which(getMap()$ADMIN == "Antarctica"), ] %>%
  spTransform(longlat) %>%
  st_as_sf() %>%
  filter(Stern == "South America")

BA_ES_sf <-
  get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat) %>%
  filter(State == "32" | State == "29")

brazil_states_sf <-
  get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat)

# Plot maps ---------------------------------------------------
cbPalette <- c(
  "#009E73", #green
  "#D55E00", #red
  "#F0E442", #yellow
  "#CC79A7", #pink
  "#0072B2", #blue
  "#999999" #grey
)
zoom_in_map <-
  ggplot() +
  geom_sf(data = brazil_cropped) +
  geom_raster(data = land_use_spdf, aes(x = x, y = y, fill = id)) +
  scale_fill_manual(
    values = cbPalette,
    name = "Land use") +
  geom_sf(data = ccaf_marine_area_sf, fill = "#0072B2", size = NA) +
  geom_sf(
    data = cus_sf,
    aes(color = "black"),
    fill = NA,
    size = 0.3
  ) +
  scale_color_identity(
    guide = "legend",
    name = element_blank(),
    labels = "Conservation Unit"
  ) +
  geom_sf(
    data = institute_sf,
    size = 1,
    color = "white",
    pch = 17
  ) +
  theme_light() +
  # Scale bar in the bottom right
  annotation_scale(location = "br", width_hint = 0.2) +
  # North arrow in the bottom right above scale bar
  annotation_north_arrow(
    location = "br",
    style = north_arrow_fancy_orienteering(text_size = 12),
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_y = unit(0.5, "in")
  ) +
  background_grid("none") +
  theme(
    axis.text.y = element_text(angle = 90),
    axis.title = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.key.size = unit(0.65, "cm"),
    legend.key = element_rect(size = 0.5)
  ) +
  coord_sf(
    # Plot axis y in the right
    label_graticule = "SE"
  ) 

zoom_out_map <-
  ggplot() +
  geom_sf(data = south_america_sf, fill = NA, size = 0.2) +
  geom_sf(data = brazil_states_sf, size = 0.2) +
  geom_sf(
    data = st_as_sf(ccaf_spatial),
    fill = "red",
    size = 0
  ) +
  geom_sf(data = ccaf_all_area_sf, fill = "purple", size = NA) +
  geom_sf(data = BA_ES_sf, fill = NA, size = 0.4) +
  geom_rect(
    data = ccaf_all_area_sf,
    xmin = -41.87851,
    xmax = -37.27757,
    ymin = -21.30999,
    ymax = -13.00164,
    fill = NA,
    size = 0.2,
    color = "#999999"
  ) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  #theme_nothing() +
  background_grid("none")

legend_map <- get_legend(zoom_in_map)

zoom_in_map <- 
  zoom_in_map + 
  theme(legend.position = "none")
  
final_plot <-
  ggdraw() +
  draw_plot(zoom_in_map, hjust = -0.1) +
  draw_plot(zoom_out_map,
    scale = 0.5,
    hjust = 0.24,
    vjust = -0.25
  ) +
  draw_plot(
    legend_map,
    hjust = 0.24,
    vjust = 0.23
  ) +
  draw_grob(
    rectGrob(gp = gpar(fill = NA, col = "#999999"),
                  width = 0.23, 
                  height = 0.43),
    hjust = 0.24,
    vjust = 0.23)

# Save map ---------------------------------------------------
save_plot("../data/results/ccaf-map.pdf",
  final_plot,
  base_height = 6,
  base_width = 8
)
