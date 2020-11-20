# File purpose: Get animals data from GBIF and speciesLink
# Pay attention: Aves are in a separate script due to the dataset size.
# Date: 18/11/2020

# Load in libraries
library(rocc)
library(rgbif)

# GBIF data -----------------------------------------------------------------

# Set GBIF profile
options(gbif_user = "inescomarella")
options(gbif_pwd = "********")
options(gbif_email = "inesmottacomarella@gmail.com")

# Spin up a download request for GBIF occurrence data
# This takes a while
# You just need to do it once, after that you can just download the files using the
# key
mamm_occ_down <- 
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 359),
    pred("hasCoordinate", TRUE),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020)
  )
amph_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 131),
    pred("hasCoordinate", TRUE),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020)
  )
rept_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 358),
    pred("hasCoordinate", TRUE),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020) 
  )
inse_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 216),
    pred("hasCoordinate", TRUE),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020)
  )
arac_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 367),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020)
  )

# Get download from GBIF
 amph_occ_get <-
  occ_download_get(key = "0101858-200613084148143",
                   path = "../data/processed-data",
                   overwrite = TRUE)
rept_occ_get <-
  occ_download_get(key = "0101880-200613084148143",
                   path = "../data/processed-data",
                   overwrite = TRUE)
inse_occ_get <-
  occ_download_get(key = "0101888-200613084148143",
                   path = "../data/processed-data",
                   overwrite = TRUE)
arac_occ_get <-
  occ_download_get("0101907-200613084148143",
                   path = "../data/processed-data",
                   overwrite = TRUE)

# Import downloaded file from GBIF
# Unzip and read files
# Takes 189.560s to read the zip files
path_name <- "../data/processed-data"

gbif_occ_imported_amph <- occ_download_import(amph_occ_get, path_name)
gbif_occ_imported_rept <- occ_download_import(rept_occ_get, path_name)
gbif_occ_imported_inse <- occ_download_import(inse_occ_get, path_name)
gbif_occ_imported_arac <- occ_download_import(arac_occ_get, path_name)

# speciesLink data -----------------------------------------------------------

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

# There is some bug in the rocc::rspeciesLink() download
file.remove("../data/processed-data/raw-spLink-animals-data.csv")

# Save data -----------------------------------------------------------------
write.csv(gbif_occ_imported_amph,
          "../data/processed-data/raw-gbif-amph.csv")
write.csv(gbif_occ_imported_rept,
          "../data/processed-data/raw-gbif-rept.csv")
write.csv(gbif_occ_imported_inse,
          "../data/processed-data/raw-gbif-inse.csv")
write.csv(gbif_occ_imported_arac,
          "../data/processed-data/raw-gbif-arac.csv")
write.csv(spLink_animals_down,
          "../data/processed-data/raw-spLink-animals.csv")

