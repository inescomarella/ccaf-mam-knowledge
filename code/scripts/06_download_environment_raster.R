# Date: 07/04/2021

xfun::pkg_attach2(c("raster", "sf", "sp", "tidyverse"))

longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ----------------------------
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

# BIO1 = Annual Mean Temperature
# BIO12 = Annual Precipitation
worldclim_amt_ap <- worldclim_data[[c(1, 12)]]
names(worldclim_amt_ap) <-
  c("AMT", "AP")

# Crop raster ----------------------------
environment_cropped <- mask(crop(worldclim_amt_ap, ccaf), ccaf)

elevation_cropped <- mask(crop(elevation_data, ccaf), ccaf)

# Export raster ----------------------------

writeRaster(
  environment_cropped,
  "../data/processed-data/worldclim-amt-ap.grd",
  bandorder = "BIL",
  overwrite = TRUE
)

writeRaster(elevation_cropped,
  "../data/processed-data/elevation.tif",
  overwrite = TRUE
)
