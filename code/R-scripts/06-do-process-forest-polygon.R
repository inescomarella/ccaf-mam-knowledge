# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "raster",
    "geobgu",
    "stars"
  )
)
conflicted::conflict_scout()
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("extract", "raster")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data --------------------------------------------------------------
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

rm(br_longlat)

# Forest ---------------------------------
forest_polygon <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(as(ccaf, "Spatial")) %>%
  mask(as(ccaf, "Spatial")) %in% 1:10
  
r.to.poly <- 
  forest_polygon %>%
  st_as_stars() %>%
  st_as_sf(
    as_points = FALSE,
    merge = TRUE
  )

clean_poly <- r.to.poly %>%
  filter(layer == 1) %>%
  st_make_valid()

st_write(
  clean_poly,
  paste0("../data/processed-data/", "/", "forest-area.shp"),
  delete_layer = TRUE
)

st_is_valid(filter(r.to.poly, layer == 1))

# value < 10 == Forest



