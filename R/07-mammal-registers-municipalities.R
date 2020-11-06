setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'GISTools', 'FNN', 'vegan', 'corrplot', 'ggplot2')
lapply(x, library, character.only = TRUE)

conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'select', winner = 'dplyr')

source('functions.R')

municipios <-
  st_read("../outputs/backup/municipios_joined.shp",
          crs = CRS("+proj=longlat +datum=WGS84"))
data <-
  st_read(
    '../data/mamm-data-clean.csv',
    options = c(
      'X_POSSIBLE_NAMES=decimalLongitude',
      'Y_POSSIBLE_NAMES=decimalLatitude'
    ),
    crs = CRS("+proj=longlat +datum=WGS84")
  )

municipios$n_reg <- lengths(st_intersects(municipios, data))

municipios_cropped <-
  st_crop(
    municipios,
    xmin = -41.8798,
    xmax = -38,
    ymin = -21.30178,
    ymax = -13.00164
  )
ggplot(municipios_cropped) + geom_sf(aes(fill = n_reg), size = 0.25)
