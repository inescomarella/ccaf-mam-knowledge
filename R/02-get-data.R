setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c("tidyverse", "rocc", "rgbif", "sf") 
lapply(x, library, character.only = TRUE)

source('functions.R')

ccma <-
  st_read(dsn = '../outputs',
          layer = 'ccma-clipped',
          check_ring_dir = TRUE)

# Get occurence data from speciesLink
spLink_animals_down <- rspeciesLink(
  dir = '../data/',
  filename = 'spLink-mamm-raw',
  stateProvince = c('Espirito Santo', 'Espírito Santo', 'ES', 'Bahia', 'BA'),
  Coordinates = 'Yes',
  Scope = 'animals',
  Synonyms = 'species2000'
)

# Just mammal data
spLink_mamm_filtered <- spLink_animals_down %>% filter(class == 'Mammalia')

# Spin up a download request for GBIF occurrence data
gbif_mamm_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 359),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1500), #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)

# Get download from GBIF
gbif_mamm_occ_get <-
  occ_download_get(key = '0103075-200613084148143',
                   path = '../data',
                   overwrite = TRUE)

# Import downloaded file from GBIF
gbif_mamm_occ_imported <-
  occ_download_import(gbif_mamm_occ_get, path = '../data')

# Remove fossil record and iNaturalist registers
gbif_mamm_filtered <- remove.fossil.iNaturalist(gbif_mamm_occ_imported)

# Remove point outside CCMA
spLink_mamm_clipped <- clip.ccma(spLink_mamm_filtered)
gbif_mamm_clipped <- clip.ccma(gbif_mamm_filtered)

# Output
write.csv(spLink_mamm_clipped, '../data/spLink-mamm-clipped.csv')
write.csv(gbif_mamm_clipped, '../data/gbif-mamm-clipped.csv')
