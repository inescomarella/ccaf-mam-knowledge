
# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "fossil",
    "raster",
    "geobgu",
    "stars"
  )
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")
conflicted::conflict_prefer("raster_extract", "geobgu")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data -----------
ccaf <- read_sf("data/processed/maps/ccaf_map.shp")

environment <- brick("data/processed/maps/worldclim_amt_ap.grd") 

elevation <- raster("data/processed/maps/elevation.tif") 

cus <-
  read_sf("data/processed/maps/CUs_map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf)

forest <-
  raster(
    "data/raw/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(as(ccaf, "Spatial")) %>%
  mask(as(ccaf, "Spatial")) %in% 1:8

records <-
  st_read(
    dsn = "data/processed/clean_database.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  mutate(id = seq(1, nrow(.)))

institutes <-
  st_read(
    dsn = "data/raw/research_institutes.csv",
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
    KL = (c + nrec / max(grid_bio$nrec, na.rm = TRUE)) / 2
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

# Export grid ---------------------------
write_sf(grid_data_classified, "./data/processed/maps/grid_data.shp")

# Save workspace ----
save.image("~/tcc-ccma/code/workspaces/spatial_analyses.RData")
