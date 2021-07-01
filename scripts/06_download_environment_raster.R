# Date: 07/04/2021

xfun::pkg_attach2(c("raster", "sf", "sp", "tidyverse"))

longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ----------------------------
ccaf <- read_sf("data/processed/maps/ccaf_map.shp")

# Get raster ----------------------------

# Get environment data
worldclim_data <- getData("worldclim", var = "bio", res = 2.5)

# Crop raster ----------------------------
environment_cropped <- mask(crop(worldclim_data, ccaf), ccaf)

# Export raster ----------------------------

writeRaster(environment_cropped,
  "data/processed/maps/worldclim_ccaf.grd",
  bandorder = "BIL",
  overwrite = TRUE
)

