x <- c('conflicted', 'dplyr', 'raster', 'sf', 'rgdal', 'GISTools', 'FNN', 'vegan', 'corrplot', 'ggplot2')
lapply(x, library, character.only = TRUE)

conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'select', winner = 'dplyr')

load('functions.R')

municipios <- st_read("./outputs/backup/municipios_joined.shp")
data <- read.csv('./data/data-all-clean.csv')

to_remove <- data %>% filter(is.na(decimalLongitude))
data <- anti_join(data, to_remove)

data_layer <- data %>% select(decimalLongitude, decimalLatitude, eventYear, order, family, species)

data_st <- st_as_sf(data_layer, coords = 1:2)
data_st_crs <- st_set_crs(data_st, CRS("+proj=longlat +datum=WGS84"))

municipios_crs <- st_transform(municipios, crs = st_crs(data_st_crs))

data_clipped <- st_intersection(data_st_crs, municipios_crs)


municipios_crs$n_reg <- lengths(st_intersects(municipios_crs, data_clipped))

municipios_cropped <- st_crop(municipios_crs, xmin = -41.8798, xmax = -38,
                          ymin = -21.30178, ymax = -13.00164)
ggplot(municipios_cropped) + geom_sf(aes(fill = n_reg), size = 0.25)
