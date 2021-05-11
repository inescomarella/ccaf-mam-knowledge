# Date: 07/04/2021

xfun::pkg_attach2(c("raster", "sf", "sp", "tidyverse"))

longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ----------------------------
ccaf <- read_sf("data/processed/maps/ccaf_map.shp")

# Get raster ----------------------------
# Get elevation data
elevation_data <-
  getData("alt",
    country = "BRA",
    mask = TRUE,
    res = 2.5
  )

# Get environment data
worldclim_data <- getData("worldclim", var = "bio", res = 2.5)

# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(5, 6, 12)]]
names(worldclim_amt_ap) <-
  c("MTWM", "MTCM", "AP")

# Crop raster ----------------------------
environment_cropped <- mask(crop(worldclim_amt_ap, ccaf), ccaf)

elevation_cropped <- mask(crop(elevation_data, ccaf), ccaf)

# Export raster ----------------------------

writeRaster(environment_cropped,
  "data/processed/maps/worldclim_amt_ap.grd",
  bandorder = "BIL",
  overwrite = TRUE
)

writeRaster(elevation_cropped,
  "data/processed/maps/elevation.tif",
  overwrite = TRUE
)
