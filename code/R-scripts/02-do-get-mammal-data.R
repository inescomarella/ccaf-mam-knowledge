# File purpose: Get GBIF and speciesLink mammal data of ES and BA states
# Date: 16/11/2020

# Load in libraries
x <- c("tidyverse", "rocc", "rgbif", "sf") 
lapply(x, library, character.only = TRUE)

# Source functions
source("./R-scripts/functions/01-funs-get-mammal-data.R")

# Get occurrence data from speciesLink
# Takes 244.321s to run
spLink_animals_down <-
  rspeciesLink(
    dir = "../data/processed-data/",
    filename = "raw-spLink-animals-data",
    stateProvince = c("Espirito Santo", "Espírito Santo", "ES", "Bahia", "BA"),
    Coordinates = "Yes",
    Scope = "animals",
    Synonyms = "species2000"
  )

# There is come bug in the rocc::rspeciesLink() download, so just use the object
file.remove("../data/processed-data/raw-spLink-animals-data.csv")

# Just mammal data
spLink_mamm_filtered <- 
  spLink_animals_down %>% 
  filter(class == "Mammalia")

# Spin up a download request for GBIF occurrence data
# This might take a while
gbif_mamm_occ_down <- 
  occ_download(
  user = "inescomarella",
  pwd = "********",
  email = "inesmottacomarella@gmail.com",
  format = "SIMPLE_CSV",
  pred("country", "BR"),
  pred("taxonKey", 359),
  pred("hasCoordinate", TRUE),
  #greaterThanOrEquals
  pred_gte("year", 1500),
  #lessThanOrEquals
  pred_lte("year", 2020)
)

# Get download from GBIF
gbif_mamm_occ_get <-
  occ_download_get(key = "0103075-200613084148143",
                   path = "../data/processed-data/",
                   overwrite = TRUE)

# Import downloaded file from GBIF
gbif_mamm_occ_imported <-
  occ_download_import(gbif_mamm_occ_get, path = "../data/processed-data/")

# Remove fossil record and iNaturalist registers
gbif_mamm_filtered <- remove_fossil_iNaturalist(gbif_mamm_occ_imported)

# Export clean data.frames
write.csv(spLink_mamm_filtered, "../data/processed-data/raw-spLink-mammal-data.csv")
write.csv(gbif_mamm_filtered, "../data/processed-data/raw-gbif-mammal-data.csv")
