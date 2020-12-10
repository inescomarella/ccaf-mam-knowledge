
# Load in libraries
library(tidyverse)
library(sf)
library(brazilmaps)
library(raster)
library(rworldmap)
library(cowplot)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-scatter-plot.R")

# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")

# Load data ------------------------------------------------
record_pts <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )

institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

cus_sf <-
  st_read(dsn = "../data/processed-data/", layer = "CUs-map")

corridors <- st_read(
  dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
  layer = "corredores_ppg7",
  check_ring_dir = TRUE
)

# Get Brazil map
br_longlat <-
  brazilmaps::get_brmap(geo = "Brazil") %>%
  st_as_sf() %>%
  st_transform(longlat)


# Pre-process maps ------------------------------------------

# Get Brazil map
brazil_sf <-
  brazilmaps::get_brmap(geo = "State") %>%
  st_as_sf() %>%
  st_transform(longlat)

# Keep only CCAF
ccaf_longlat <-
  corridors %>%
  filter(str_detect(NOME1, "Mata")) %>%
  st_set_crs(longlat)

# Clip
# Ignore the warnings
ccaf_clipped <-
  st_intersection(ccaf_longlat, brazil_sf)

# Map ----------------------------------------------------------------
# Import world map from rworldmap package
# Remove Antarctica, cause it's an open polygon, and it's very problematic
sPDF <- getMap()[-which(getMap()$ADMIN == "Antarctica"), ]
south_america_sf <- 
  sPDF %>%
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

zoom_out_map <-
  ggplot() +
  geom_sf(data = south_america_sf, fill = NA, size = 0.3) +
  geom_sf(data = brazil_sf, size = 0.3) +
  geom_sf(data = brazil_states_sf, fill = NA, size = 0.2) +
  geom_sf(data = ccaf_clipped, fill = 'red', size = 0.3) +
  geom_sf(data = BA_ES_sf, fill = NA, size = 0.6) + 
  background_grid("none") +
  theme_nothing()

# Zoom in map
land_use_table <- read.csv("../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-bahia-area.csv")

land_use_table_classified <-
  land_use_table %>%
  mutate(id = ifelse(
    class == 0,
    "Non observed",
    ifelse(
      class < 10 | class == 36 | class == 39 | class == 41,
      "Forest",
      ifelse(
        class > 10 & class < 14 | class == 29 | class == 32,
        "Non Forest Natural Formation",
        ifelse(
          class > 14 & class < 22,
          "Farming",
          ifelse(
            class > 22 & class < 26 | class == 30,
            "Non vegetated area",
            ifelse(
              class == 31 | class == 33,
              "Water",
              ""
            )
          )
        )
      )
    )
  )) %>%
  mutate(value = class) %>%
  select(value, id)

color_table <-
  tibble(
    id = c(
      "Forest",
      "Non Forest Natural Formation",
      "Farming",
      "Non vegetated area",
      "Water",
      "Non observed"),
    color = c("129912",
              "32CD32",
              "#FF8C00",
              "EA9999",
              "0000FF",
              "D5D5E5")
  )

land_use_table_classified <- merge(land_use_af_mapbiomas_df, land_use_table_classified)


###############################
land_use_af_mapbiomas <- raster::raster("../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif")

library(viridis)
library(raster)
library(tidyverse)

ccaf_clipped <-
  ccaf_clipped %>%
  filter(State == 32 | State == 29) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")

ccaf_clipped <- as(ccaf_clipped, "Spatial")

land_use_af_mapbiomas <- mask(land_use_af_mapbiomas, ccaf_clipped)

land_use_af_mapbiomas <- as(land_use_af_mapbiomas, "SpatialPixelsDataFrame")
land_use_af_mapbiomas_df <- as.data.frame(land_use_af_mapbiomas)
colnames(land_use_af_mapbiomas_df) <- c("value", "x", "y")


land_use_table <- read.csv("../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-bahia-area.csv")

land_use_table_classified <-
  land_use_table %>%
  mutate(id = ifelse(
    class == 0,
    "Non observed",
    ifelse(
      class < 10 | class == 36 | class == 39 | class == 41,
      "Forest",
      ifelse(
        class > 10 & class < 14 | class == 29 | class == 32,
        "Non Forest Natural Formation",
        ifelse(
          class > 14 & class < 22,
          "Farming",
          ifelse(
            class > 22 & class < 26 | class == 30,
            "Non vegetated area",
            ifelse(
              class == 31 | class == 33,
              "Water",
              ""
            )
          )
        )
      )
    )
  )) %>%
  mutate(value = class) %>%
  select(value, id)

land_use_table_classified <- 
  merge(land_use_af_mapbiomas_df, land_use_table_classified)

library(ggplot2)
ggplot(land_use_table_classified) +
  geom_tile(aes(x = x, y = y, fill = id))
