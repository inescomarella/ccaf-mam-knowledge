library(tidyverse)
library(sf)
library(raster)


# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")

land_use_table <-
  read.csv(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-area.csv"
  )

land_use_af_mapbiomas <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  )

corridors <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
) %>%
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

br_longlat <-
  brazilmaps::get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)

corridors <-
  st_intersection(corridors, br_longlat)

corridors <- as(corridors, "Spatial")

land_use_af_mapbiomas <- crop(land_use_af_mapbiomas, corridors)
land_use_af_mapbiomas <- mask(land_use_af_mapbiomas, corridors)

land_use_af_mapbiomas <-
  as(land_use_af_mapbiomas, "SpatialPixelsDataFrame")

land_use_af_mapbiomas <- as.data.frame(land_use_af_mapbiomas)

colnames(land_use_af_mapbiomas) <- c("value", "x", "y")

land_use_table <-
  land_use_table %>%
  mutate(id = ifelse(
    class == 0,
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
  dplyr::select(value, id, class_name)

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
      "#06bd00", #dark green
      "#2bff00", #light green
      "#ffe100", #yellow
      "#ff00cc", #magenta
      "#0000FF", #blue
      "#a6a6a6" #grey
    )
  )


land_use_af_mapbiomas <-
  merge(land_use_af_mapbiomas, land_use_table)

cus_geom <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map") %>%
  st_transform(sp::CRS("+proj=longlat +datum=WGS84"))

institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

library(ggspatial)


ccaf_longlat <-
  st_read(
    dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
    layer = "corredores_ppg7",
    check_ring_dir = TRUE
  ) %>%
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

brazil_cropped <-
  brazilmaps::get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat) %>%
  st_crop(st_bbox(ccaf_longlat))

ccaf_longlat <- st_difference(ccaf_longlat, st_as_sf(corridors))

land_use_af_mapbiomas$id <- factor(land_use_af_mapbiomas$id, levels = c("Forest", "Non Forest Natural Formation", "Farming", "Non vegetated area", "Water", "Non observed"))



ccaf_blah <-
  st_read(
    dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
    layer = "corredores_ppg7",
    check_ring_dir = TRUE
  ) %>%
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

zoom_in_map <-
  ggplot() +
  geom_sf(data = brazil_cropped) +
  geom_raster(data = land_use_af_mapbiomas, aes(x = x, y = y, fill = id)) +
  scale_fill_manual(values = color_table$color) +
  geom_sf(data = st_as_sf(corridors), fill = NA, size = 0.2) +
  geom_sf(data = ccaf_longlat, fill = "#0000FF", size = 0.2) +
  geom_sf(data = cus_geom, fill = NA, size = 0.3, color = "black") +
  geom_sf(data = institute_pts, size = 0.7, color = "white", pch = 17) +
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
  cowplot::background_grid("none") +
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


library(rworldmap)
south_america_sf <- getMap()[-which(getMap()$ADMIN == "Antarctica"), ] %>%
  spTransform(longlat) %>%
  st_as_sf() %>%
  filter(Stern == "South America")

BA_ES_sf <-
  brazilmaps::get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat) %>%
  filter(State == "32" | State == "29")

brazil_states_sf <-
  brazilmaps::get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat)

zoom_out_map <-
  ggplot() +
  geom_sf(data = south_america_sf, fill = NA, size = 0.2) +
  geom_sf(data = brazil_states_sf, size = 0.2) +
  geom_sf(data = st_as_sf(corridors), fill = 'red', size = 0.2) +
  geom_sf(data = ccaf_longlat, fill = "red", size = 0.2) +
  geom_sf(data = BA_ES_sf, fill = NA, size = 0.4) +   
  geom_rect(
    data = ccaf_longlat,
    xmin = -41.87851,
    xmax = -37.27757,
    ymin = -21.30999,
    ymax = -13.00164,
    fill = NA,
    size = 0.2,
    colour = "black"
  ) +
  cowplot::background_grid("none") +
  cowplot::theme_nothing()

library(cowplot)
cowplot::ggdraw() +
  cowplot::draw_plot(zoom_in_map, hjust = -0.1) +
  cowplot::draw_plot(zoom_out_map, scale = 0.5, hjust = 0.38, vjust = -0.25)

rm(arrowB)  
