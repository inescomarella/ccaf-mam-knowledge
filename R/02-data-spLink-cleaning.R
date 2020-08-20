x <- c("rgdal", "raster", "rgeos", "tidyverse", "spatialEco", "rocc")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Get municipalities names
municipios_ccma <- readOGR(dsn = "../outputs", layer = "municipios-ccma")
municipios_names <- municipios_ccma@data$NM_MUNICIP

# Get occurence data from speciesLink
rspeciesLink(filename = 'data-spLink',
             stateProvince = c('Espirito Santo', 'Espírito Santo', 'ES', 'Bahia', 'BA'),
             Scope = 'animals',
             Synonyms = 'species2000')

# Input
data_downloaded <- read.csv('./results/data-spLink.csv')

# Apenas dados de mamiferos
data_mammal_raw <- data_downloaded %>% filter(class == 'Mammalia')

# Removendo dados sem coordenada geografica
data_mammal <- data_mammal_raw %>% filter(!is.na(decimalLatitude))

# Output
write.csv(data_mammal, './data-spLink.csv')
