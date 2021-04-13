
xfun::pkg_attach2(c("raster", "sf", "sp", "tidyverse"))

longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ----------------------------
br <-
  read_sf("data/raw/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf <-
  read_sf("data/raw/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
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

# Export shapefile -------------------------------
if (!dir.exists("data/processed/maps")) {dir.create("data/processed/maps")}

st_write(
  ccaf,
  paste0("data/processed/maps/", "/", "ccaf_map.shp"),
  delete_layer = TRUE
)
