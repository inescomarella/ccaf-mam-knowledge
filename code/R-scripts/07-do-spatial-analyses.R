
# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "fossil",
    "raster",
    "patchwork",
    "cowplot",
    "dotwhisker",
    "geobgu",
    "stars",
    "fishualize",
    "ggpubr"
  )
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")
conflicted::conflict_prefer("raster_extract", "geobgu")
conflicted::conflict_prefer("get_legend", "cowplot")

source("./R-scripts/functions/funs-spatial-analyses.R")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data -----------
br <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf <-
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat) %>%
  st_intersection(br) %>%
  st_crop(
    xmax = -38.7,
    xmin = -41.87851,
    ymax = -13.00164,
    ymin = -21.30178
  )

rm(br)

forest <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(as(ccaf, "Spatial")) %>%
  mask(as(ccaf, "Spatial")) %in% 1:10

environment <- brick("../data/processed-data/worldclim-amt-ap.grd") 

elevation <- raster("../data/processed-data/elevation.tif") 

cus <-
  read_sf("../data/processed-data/CUs-map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf)

records <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  mutate(id = seq(1, nrow(.)))

institutes <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Make grid ----
grid <-
  ccaf %>%
  st_make_grid(cellsize = 0.1) %>%
  st_as_sf()

grid$grid_id <- seq(1, nrow(grid), 1)

# Area
grid$area <- grid %>% st_area()
units(grid$area) <- "km^2"
max(grid$area)
min(grid$area)

units(grid$area) <- NULL

# Pre-process environment data ----

# Create raster from grid
reqGridBbox <- st_bbox(grid)

# calculate number of rows/columns
nCols <- (reqGridBbox[3] - reqGridBbox[1]) / 0.1
nRows <- (reqGridBbox[4] - reqGridBbox[2]) / 0.1

grid_raster <- raster(ncol = nCols, nrow = nRows)
values(grid_raster) <- 1:ncell(grid_raster)
extent(grid_raster) <- extent(ccaf)
res(grid_raster) <- 0.1

# Re-scale
environmet_resampled <-
  resample(x = environment, y = grid_raster, method = "bilinear")
elevation_resampled <-
  resample(x = elevation, y = grid_raster, method = "bilinear")

# Extract environment data ---------------------

grid_centroids <- grid %>%
  st_centroid()

elev <- extract(elevation_resampled, grid_centroids)
envi <- extract(environmet_resampled, grid_centroids)

grid_envi <- grid
grid_envi$elev <- elev
grid_envi$AMT <- envi[, 1]
grid_envi$AP <- envi[, 2]

grid_envi <- grid_envi %>% 
  mutate(
    forest_cov = raster_extract(
      x = st_as_stars(forest),
      y = grid_envi,
      fun = sum,
      na.rm = TRUE
    )
  )

grid_envi$CU <-  grid_envi %>%
  st_intersects(cus) %>%
  lengths()

# Calculate variables ----
grid_bio <- grid_envi

record_grid <- st_intersection(records, grid_bio)

for (i in 1:nrow(grid)) {
  
  grid_bio$nrec[i] <- record_grid %>%
    filter(grid_id == i) %>%
    nrow()
  
  nrec <- record_grid %>%
    filter(grid_id == i) %>%
    filter(!is.na(eventDate)) %>%
    nrow()
  
  cell_df <- record_grid %>%
    filter(grid_id == i) %>%
    select(eventDate, species) %>%
    st_drop_geometry() %>%
    unique()
  
  grid_bio$Sobs[i] <- length(unique(cell_df$species))
  
  if (nrec >= 25) {
    
    cell_df_expended <- cell_df %>%
      expand(eventDate, species)
    
    cell_df_expended_absent <- anti_join(cell_df_expended, cell_df)
    
    cell_df$State <- 1
    cell_df_expended_absent$State <- 0
    
    cell_df_State <- bind_rows(cell_df, cell_df_expended_absent)
    
    sp_by_saple_df <- cell_df_State %>%
      arrange(eventDate, species) %>%
      spread(key = eventDate, value = State)
    
    grid_bio$Sest[i] <- chao2(sp_by_saple_df[, -1])
  } else {
    grid_bio$Sest[i] <- NA
  }
  
}

grid_bio <- grid_bio %>%
  mutate(c = Sobs/Sest)

grid_data <-  grid_bio %>%
  group_by(grid_id) %>%
  mutate(
    KL = (c + nrec / max(grid_envi$nrec, na.rm = TRUE)) / 2
  ) %>%
  mutate(KL = ifelse(is.na(KL) | is.infinite(KL), 0, KL))

grid_data_processing <- grid_data %>%
  mutate(
    elevd = abs(elev - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$elev),
    AMTd = abs(AMT - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$AMT),
    APd = abs(AP - filter(
      grid_data, nrec == max(grid_data$nrec, na.rm = TRUE)
    )$AP)
  )

grid_data_processed <- grid_data_processing %>%
  mutate(
    elevd = elevd / max(grid_data_processing$elevd, na.rm = TRUE),
    AMTd = AMTd / max(grid_data_processing$AMTd, na.rm = TRUE),
    APd = APd / max(grid_data_processing$APd, na.rm = TRUE),
    forestw = forest_cov / max(grid_data_processing$forest_cov, na.rm = TRUE)
  ) %>%
  mutate(KG = forestw * mean(c(elevd, AMTd, APd, (1 - KL))))

# Map variables ----

grid_data_classified <- grid_data_processed %>%
  ungroup() %>%
  filter(!is.nan(KG)) %>%
  mutate(grid_id = as.character(grid_id)) %>%
  mutate(KG_class = ifelse(
    KG > 0.8,
    "Very high",
    ifelse(
      KG > 0.6 & KG <= 0.8,
      "High",
      ifelse(
        KG > 0.4 & KG <= 0.6,
        "Medium",
        ifelse(
          KG > 0.2 & KG <= 0.4,
          "Low",
          "Very low"
        )
      )
    )
  )) %>%
  mutate(KL_class = ifelse(
    KL > 0.8,
    "Very high",
    ifelse(
      KL > 0.6 & KL <= 0.8,
      "High",
      ifelse(
        KL > 0.4 & KL <= 0.6,
        "Medium",
        ifelse(
          KL > 0.2 & KL <= 0.4,
          "Low",
          "Very low"
        )
      )
    )
  ))

levels <- c("Very high", "High", "Medium", "Low", "Very low")

grid_data_classified$KL_class <-
  factor(grid_data_classified$KL_class,
    levels = levels
  )
grid_data_classified$KG_class <-
  factor(grid_data_classified$KG_class,
    levels = levels
  )

KL_map <- grid_data_classified %>%
  filter(!is.na(elev)) %>%
  ggplot() +
  geom_sf(aes(fill = KL_class), color = NA) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  theme_light() +
  labs(fill = "Knowledge level")

KG_map <- grid_data_classified %>%
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(aes(fill = KG_class), color = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Study urgency level")

nrec_map <- grid_data_classified %>%
  filter(nrec > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = nrec)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(1, max(grid_data_classified$nrec)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), nrec, 0, 1),
    labels = break_5points(st_drop_geometry(grid_data_classified), nrec, 0, 1)
  ) +
  theme_light() +
  labs(fill = "Number of\nRecords")

c_map <- grid_data_classified %>%
  filter(!is.na(c)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = c)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(0, max(grid_data_classified$c, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), c, 1, 0),
    labels = break_5points(st_drop_geometry(grid_data_classified), c, 1, 0)
  ) +
  theme_light() +
  labs(fill = "Completeness")

CU_map <- grid_data_classified %>%
  filter(!is.na(AMT)) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggplot() +
  geom_sf(color = NA, aes(fill = CU)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus", discrete = TRUE) +
  theme_light() +
  labs(fill = "Conservation\nUnit")

forest_map <- grid_data_classified %>%
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = forest_cov)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(grid_data_classified$forest_cov, na.rm = TRUE), max(grid_data_classified$forest_cov, na.rm = TRUE)),
    breaks = c(min(grid_data_classified$forest_cov, na.rm = TRUE), max(grid_data_classified$forest_cov, na.rm = TRUE)),
    labels = c("Low", "High")
  ) +
  theme_light() +
  labs(fill = "Relative forest coverage")

Sobs_map <- grid_data_classified %>%
  filter(!is.na(Sobs)) %>%
  filter(Sobs > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sobs)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(1, max(grid_data_classified$Sobs, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), Sobs, 0, 1),
    labels = break_5points(st_drop_geometry(grid_data_classified), Sobs, 0, 1)
  ) +
  theme_light() +
  labs(fill = "Species richness\nobserved")

Sest_map <- grid_data_classified %>%
  filter(!is.na(Sest)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sest)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(grid_data_classified$Sest, na.rm = TRUE), 
               max(grid_data_classified$Sest, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), Sest, 0, min(grid_data_classified$Sest, na.rm = TRUE)),
    labels = break_5points(st_drop_geometry(grid_data_classified), Sest, 0, min(grid_data_classified$Sest, na.rm = TRUE))
  ) +
  theme_light() +
  labs(fill = "Species richness\nestimated")

elev_map <- grid_data_classified %>%
  filter(!is.na(elev)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elev)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(round(min(grid_data_classified$elev, na.rm = TRUE), 0), 
               max(grid_data_classified$elev, na.rm = TRUE)),
    breaks = break_5points(st_drop_geometry(grid_data_classified), elev, 0, round(min(grid_data_classified$elev, na.rm = TRUE), 0)),
    labels = break_5points(st_drop_geometry(grid_data_classified), elev, 0, round(min(grid_data_classified$elev, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Elevation (m)")

elev_distance_map <- grid_data_classified %>%
  filter(!is.na(elev)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elevd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Elevd")

grid_data_classified <- grid_data_classified %>%
  mutate(AMT = AMT / 10)

AMT_map <- grid_data_classified %>%
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMT)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      round(min(grid_data_classified$AMT, na.rm = TRUE), 1),
      round(max(grid_data_classified$AMT, na.rm = TRUE), 1)
    ),
    breaks = break_5points(st_drop_geometry(grid_data_classified), AMT, 1, round(min(grid_data_classified$AMT, na.rm = TRUE), 1)),
    labels = break_5points(st_drop_geometry(grid_data_classified), AMT, 1, round(min(grid_data_classified$AMT, na.rm = TRUE), 1))
  ) +
  theme_light() +
  labs(fill = "Annual Mean\nTemperature (ºC)")

AMT_distance_map <- grid_data_classified %>%
  filter(!is.na(AMTd)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMTd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

AP_map <- grid_data_classified %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      round(min(grid_data_classified$AP, na.rm = TRUE), 0),
      round(max(grid_data_classified$AP, na.rm = TRUE), 0)
    ),
    breaks = break_5points(st_drop_geometry(grid_data_classified), AP, 0, round(min(grid_data_classified$AP, na.rm = TRUE), 0)),
    labels = break_5points(st_drop_geometry(grid_data_classified), AP, 0, round(min(grid_data_classified$AP, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Annual precipitation (mm)")

AP_distance_map <- grid_data_classified %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = APd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

pri_prox_map <- grid_data_classified %>%
  filter(!is.na(proximity)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = proximity)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Proximity to collection")

# Graph nrec x variables ----

proximity_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "proximity", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Proximity to collection")

CU_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggscatter(
    x = "CU", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Conservation Unit")

forest_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "forestw", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Relative forest coverage")


Sest_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "Sest", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Chao2 species richness estimation")

c_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "c", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) + 
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Completeness")

KG_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "KG", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Knowledge gap index")

KL_graph <- grid_data_classified %>%
  filter(!is.na(nrec)) %>%
  ggscatter(
    x = "KL", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Knowledge level index")

elev_graph <- grid_data_classified %>%
  ggscatter(
    x = "elev", y = "nrec",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Elevation (m)")

AMT_graph <- grid_data_classified %>%
  mutate(AMT = AMT / 10) %>%
  ggscatter(
    x = "AMT", y = "KL",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Annual Mean Temperature (ºC)")

AP_graph <- grid_data_classified %>%
  ggscatter(
    x = "AP", y = "KL",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE
  ) +
  stat_cor(method = "pearson") +
  theme_light() +
  labs(y = "Number of records",
       x = "Annual Precipitation (mm)")

# Save results -------------
write_sf(grid_data_classified, "../data/processed-data/grid-data.shp")

nrec_map + theme_void() 
ggsave("../data/results/07-nrec-map.pdf",
       width = 8,
       height = 6
)

Sobs_map + theme_void() 
ggsave("../data/results/07-Sobs-map.pdf",
       width = 8,
       height = 6
)

Sest_map + theme_void()
ggsave("../data/results/07-Sest-map.pdf",
       width = 8,
       height = 6
)

c_map + theme_void()
ggsave("../data/results/07-c-map.pdf",
       width = 8,
       height = 6
)

KL_map + theme_void()
ggsave("../data/results/07-KL-map.pdf",
       width = 8,
       height = 6
)

forest_map + theme_void() + labs(fill = "Relative forest\ncoverage")
ggsave("../data/results/07-forest-map.pdf",
       width = 8,
       height = 6
)

elev_map + theme_void()
ggsave("../data/results/07-elevation-map.pdf",
       width = 8,
       height = 6
)

AMT_map + theme_void()
ggsave("../data/results/07-AMT-map.pdf",
       width = 8,
       height = 6
)

AP_map + theme_void()
ggsave("../data/results/07-AP-map.pdf",
       width = 8,
       height = 6
)

elev_distance_map + theme_void()
ggsave("../data/results/07-elevation-distance-map.pdf",
       width = 8,
       height = 6
)

AMT_distance_map + theme_void()
ggsave("../data/results/07-AMT-distance-map.pdf",
       width = 8,
       height = 6
)

AP_distance_map + theme_void() +
  labs(fill = "Annual Precipitation\n(mm)")
ggsave("../data/results/07-AMT-distance-map.pdf",
       width = 8,
       height = 6
)

KG_map + theme_void()
ggsave("../data/results/07-KG-map.pdf",
       width = 8,
       height = 6
)

p1 <- KL_map  + theme_void() 
p2 <- nrec_map  + theme_void()
p3 <- c_map  + theme_void()
p4 <- Sobs_map  + theme_void()
p5 <- Sest_map  + theme_void()

p_grid <- plot_grid(p2, p3, p4, p5, ncol = 2, labels = c("B", "C", "D", "E"), label_size = 12)

plot_grid(p1, p_grid, labels = c("A", ""), label_size = 12)
ggsave("../data/results/07-bio-vars-map.pdf",
       width = 11.69,
       height = 8.27
)

p1 <- forest_map  + theme_void() + 
  labs(fill = "Relative forest\ncoverage")
p2 <- elev_map  + theme_void()
p3 <- AMT_map  + theme_void() +
  labs(fill = "Annual Mean\nTemperature (ºC)")
p4 <- AP_map  + theme_void() + 
  labs(fill = "Annual Precipation\n(mm)")
plot_grid(p1, p2, p3, p4, 
          ncol = 4, 
          labels = c("A", "B", "C", "D"), 
          label_size = 12)

ggsave("../data/results/07-envi-vars-map.pdf",
       width = 13,
       height = 5
)

# Save workspace ----
save.image("~/tcc-ccma/code/spatial-analyses.RData")
