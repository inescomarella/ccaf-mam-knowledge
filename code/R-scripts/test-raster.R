# File purpose: Make CCAF first map
# Date: 13/12/2020

# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(brazilmaps)
library(ggspatial)
library(rworldmap)
library(cowplot)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Set projection
longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load data ---------------------------------------------------

br_sf <-
  get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)

ccaf_spatial <-
  st_read(
    dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
    layer = "corredores_ppg7",
    check_ring_dir = TRUE
  ) %>%
  st_set_crs(longlat) %>%
  # Only Central Corridor of Atlantic Forest
  filter(str_detect(NOME1, "Mata")) %>%
  # Only terrestrial area
  st_intersection(br_sf) %>%
  # Fix name
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
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

cus_sf <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(st_as_sf(ccaf_spatial))

institute_sf <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

ccaf_all_area_sf <-
  st_read(
    dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
    layer = "corredores_ppg7",
    check_ring_dir = TRUE
  ) %>%
  st_set_crs(longlat) %>%
  # Only Central Corridor of Atlantic Forest
  filter(str_detect(NOME1, "Mata")) %>%
  # Fix name
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_as_sf()

# Pre-process data --------------------------------------------

land_use_table_df <-
  land_use_table_df %>%
  mutate(id = ifelse(class == 0,
    "Non observed",
    ifelse(
      class < 10,
      "Forest",
      ifelse(
        class > 10 & class < 14 | class == 29 | class == 32,
        "Non Forest Natural Formation",
        ifelse(
          class > 14 & class < 22 | class == 41 | class == 39 | class == 36,
          "Farming",
          ifelse(
            class > 22 & class < 26 | class == 30,
            "Non vegetated area",
            ifelse(class == 31 | class == 33,
              "Water",
              ""
            )
          )
        )
      )
    )
  )) %>%
  mutate(value = class) %>%
  select(value, id, class_name)

color_table <-
  tibble(
    id = c(
      "Forest",
      "Non Forest Natural Formation",
      "Farming",
      "Non vegetated area",
      "Water",
      "Non observed"
    ),
    color = c(
      # dark green
      "#06bd00",
      # light green
      "#2bff00",
      # yellow
      "#ffe100",
      # magenta
      "#ff00cc",
      # blue
      "#0000FF",
      # grey
      "#a6a6a6"
    )
  )

colnames(land_use_spdf) <- c("value", "x", "y")

land_use_spdf <-
  merge(land_use_spdf, land_use_table_df)

land_use_spdf$id <-
  factor(
    land_use_spdf$id,
    levels = c(
      "Forest",
      "Non Forest Natural Formation",
      "Farming",
      "Non vegetated area",
      "Water",
      "Non observed"
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

zoom_in_map <-
  ggplot() +
  geom_sf(data = brazil_cropped) +
  geom_raster(data = land_use_spdf, aes(x = x, y = y, fill = id)) +
  scale_fill_manual(values = color_table$color) +
  geom_sf(
    data = ccaf_all_area_sf,
    fill = NA,
    size = 0.2
  ) +
  geom_sf(data = ccaf_marine_area_sf, fill = "#0000FF", size = 0.2) +
  geom_sf(
    data = cus_sf,
    fill = NA,
    size = 0.3,
    color = "black"
  ) +
  geom_sf(
    data = institute_sf,
    size = 0.7,
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
    axis.text.y = element_text(angle = 90, hjust = 0.3),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm")
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
    size = 0.2
  ) +
  geom_sf(data = ccaf_all_area_sf, fill = "red", size = 0.2) +
  geom_sf(data = BA_ES_sf, fill = NA, size = 0.4) +
  geom_rect(
    data = ccaf_all_area_sf,
    xmin = -41.87851,
    xmax = -37.27757,
    ymin = -21.30999,
    ymax = -13.00164,
    fill = NA,
    size = 0.2,
    colour = "black"
  ) +
  background_grid("none") +
  theme_nothing()

final_plot <-
  ggdraw() +
  draw_plot(zoom_in_map, hjust = -0.1) +
  draw_plot(zoom_out_map,
    scale = 0.5,
    hjust = 0.38,
    vjust = -0.25
  )

# Save map ---------------------------------------------------
save_plot("../data/results/ccaf-map.pdf",
  final_plot,
  base_height = 6,
  base_width = 8
)
