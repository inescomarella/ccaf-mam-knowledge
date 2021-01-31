# File purpose: Get GBIF and speciesLink mammal data of ES and BA states
# Date: 16/11/2020

# Load in libraries
xfun::pkg_attach(c(
  "tidyverse",
  "rocc",
  "rgbif",
  "plyr"
))

conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# GBIF data ---------------------------------------------------

# Set GBIF profile
options(gbif_user = "inescomarella")
options(gbif_pwd = "***")
options(gbif_email = "inesmottacomarella@gmail.com")

# Spin up a download request for GBIF occurrence data
# This might take a while
gbif_mamm_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 359),
    pred("hasCoordinate", TRUE),
    # greaterThanOrEquals
    pred_gte("year", 1500),
    # lessThanOrEquals
    pred_lte("year", 2020)
  )

# Get download from GBIF
gbif_mamm_occ_get <-
  occ_download_get(
    key = "0103075-200613084148143",
    path = "../data/processed-data/",
    overwrite = TRUE
  )

# Import downloaded file from GBIF
gbif_mamm_occ_imported <-
  occ_download_import(gbif_mamm_occ_get, path = "../data/processed-data/")

# speciesLink data --------------------------------------------

# Get occurrence data from speciesLink
# Takes 244.321s to run
spLink_animals_down <-
  rspeciesLink(
    dir = "../data/processed-data/",
    filename = "broken-spLink-animals-data",
    stateProvince = c("Espirito Santo", "Espírito Santo", "ES", "Bahia", "BA", "BAHIA", "ESPIRITO SANTO"),
    Coordinates = "Yes",
    Scope = "animals",
    Synonyms = "species2000"
  )

# There is come bug in the rocc::rspeciesLink() download, so just use the object
file.remove("../data/processed-data/broken-spLink-animals-data.csv")

# Just mammal data
spLink_mamm_filtered <-
  spLink_animals_down %>%
  filter(class == "Mammalia")

# Save data ---------------------------------------------------
gbif_mamm_occ_imported <-
  gbif_mamm_occ_imported %>%
  mutate(scientificName = species)

mamm_binded <- rbind.fill(spLink_mamm_filtered, gbif_mamm_occ_imported)

write.csv(mamm_binded, "../data/processed-data/downloaded-data.csv")
