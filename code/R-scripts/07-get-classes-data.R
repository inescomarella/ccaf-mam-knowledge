setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <-
  c("tidyverse", "rocc", "rgbif", "plyr", "sf", 'dplyr', 'raster')
lapply(x, library, character.only = TRUE)

source('functions.R')

ccma <-
  st_read(dsn = '../outputs',
          layer = 'ccma-clipped',
          check_ring_dir = TRUE)

# Get occurrence data from speciesLink -----
spLink_animals_down <- rspeciesLink(
  dir = '../data/',
  filename = 'spLink-animals-raw',
  stateProvince = c('Espirito Santo', 'Espírito Santo', 'ES', 'Bahia', 'BA'),
  Coordinates = 'Yes',
  Scope = 'animals',
  Synonyms = 'species2000'
)

# Spin up a download request for GBIF occurrence data ----
aves_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 212),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1500), #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)
amph_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 131),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1500), #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)
rept_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 358),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1500), #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)
inse_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 216),
  #insea
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1500),  #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)
arac_occ_down <- occ_download(
  user = 'inescomarella',
  pwd = '********',
  email = 'inesmottacomarella@gmail.com',
  format = "SIMPLE_CSV",
  pred('country', 'BR'),
  pred('taxonKey', 367),
  pred_gte("year", 1500), #greaterThanOrEquals
  pred_lte("year", 2020) #lessThanOrEquals
)

# Get download from GBIF ----
aves_occ_get <-
  occ_download_get(key = '0101910-200613084148143',
                   path = '../data',
                   overwrite = TRUE)
amph_occ_get <-
  occ_download_get(key = '0101858-200613084148143', 
                   path = '../data', 
                   overwrite = TRUE)
rept_occ_get <-
  occ_download_get(key = '0101880-200613084148143', 
                   path = '../data', 
                   overwrite = TRUE) 
inse_occ_get <-
  occ_download_get(key = '0101888-200613084148143', 
                   path = '../data', 
                   overwrite = TRUE)
arac_occ_get <-
  occ_download_get('0101907-200613084148143', 
                   path = '../data', 
                   overwrite = TRUE)

# Import downloaded file from GBIF ----
aves_occ_imported <-
  occ_download_import(aves_occ_get, path = '../data')
amph_occ_imported <-
  occ_download_import(amph_occ_get, path = '../data')
rept_occ_imported <-
  occ_download_import(rept_occ_get, path = '../data')
inse_occ_imported <-
  occ_download_import(inse_occ_get, path = '../data')
arac_occ_imported <-
  occ_download_import(arac_occ_get, path = '../data')


# Remove fossil record and iNaturalist registers -----
aves_occ_clean <- remove.fossil.iNaturalist(aves_occ_imported)
amph_occ_clean <- remove.fossil.iNaturalist(amph_occ_imported)
rept_occ_clean <- remove.fossil.iNaturalist(rept_occ_imported)
inse_occ_clean <- remove.fossil.iNaturalist(inse_occ_imported)
arac_occ_clean <- remove.fossil.iNaturalist(arac_occ_imported)

# Remove point outside CCMA ----
amph_clipped <- clip.ccma(amph_occ_clean)
rept_clipped <- clip.ccma(rept_occ_clean)
inse_clipped <- clip.ccma(inse_occ_clean)
arac_clipped <- clip.ccma(arac_occ_clean)
spLink_animals_clipped <- clip.ccma(spLink_animals_down)

# Export data
write.csv(amph_clipped, '../data/gbif-amph-clipped.csv')
write.csv(rept_clipped, '../data/gbif-rept-clipped.csv')
write.csv(inse_clipped, '../data/gbif-inse-clipped.csv')
write.csv(arac_clipped, '../data/gbif-arac-clipped.csv')
write.csv(aves_occ_clean, '../data/gbif-aves-to-clip.csv')
write.csv(spLink_animals_clipped, '../data/spLink-animals-clipped.csv')
