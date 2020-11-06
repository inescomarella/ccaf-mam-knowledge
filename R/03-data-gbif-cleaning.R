setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c("tidyverse", "rgbif", "sf")
lapply(x, library, character.only = TRUE)

source('functions.R')

# Spin up a download request for GBIF occurrence data
mamm_occ_down <- occ_download(
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
mamm_occ_get <-
  occ_download_get(key = '0103075-200613084148143',
                   path = '../data',
                   overwrite = TRUE)

# Import downloaded file from GBIF
mamm_occ_imported <-
  occ_download_import(mamm_occ_get, path = '../data')

# Remove fossil record and iNaturalist registers
mamm_occ_clean <- remove.fossil.iNaturalist(mamm_occ_imported)

# Remove point outside CCMA
mamm_clipped <- clip.ccma(mamm_occ_clean)

# Output
write.csv(mamm_clipped, '../data/gbif-mamm-clipped.csv')
