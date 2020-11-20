# File purpose: Get and clean aves data from GBIF, as the dataset is too large I keep
# using just one object as an strategy to save memory
# Date: 20/11/2020

# Load in library
# Don't load all libraries to save memory
library(rgbif)
#library(sf)
#library(sp)

# Source functions
source("./R-scripts/functions/05-funs-get-classes-data.R")

# Set GBIF profile
options(gbif_user = "inescomarella")
options(gbif_pwd = "********")
options(gbif_email = "inesmottacomarella@gmail.com")

ccma <- sf::st_read(dsn = "../data/processed-data",
                layer = "ccma-clipped",
                check_ring_dir = TRUE)
ccma <- sf::st_transform(ccma, crs = sp::CRS("+proj=longlat +datum=WGS84"))

# Spin up a download request for GBIF occurrence data
# This takes a while
aves_occ_down <-
  occ_download(
    format = "SIMPLE_CSV",
    pred("country", "BR"),
    pred("taxonKey", 212),
    pred("hasCoordinate", TRUE),
    #greaterThanOrEquals
    pred_gte("year", 1500),
    #lessThanOrEquals
    pred_lte("year", 2020)
  )

# Get download from GBIF
# Takes 2.5min to run
aves_occ_data <-
  occ_download_get(key = "0101910-200613084148143",
                   path = "../data/processed-data",
                   overwrite = TRUE)

# Import downloaded file from GBIF
# Unzip and read file
# Takes ~2min to run
aves_occ_data <- occ_download_import(aves_occ_data, "../data/processed-data")

# Remove some columns to make the object easier to handle
aves_occ_data <- 
  aves_occ_data %>% 
  select(
    class,
    order,
    family,
    species,
    scientificName,
    locality,
    stateProvince,
    decimalLatitude,
    decimalLongitude,
    year,
    basisOfRecord,
    institutionCode,
    collectionCode,
    catalogNumber,
    recordNumber,
    recordedBy
  )

# Remove fossil record and iNaturalist registers
# Takes 6.277s to run
aves_occ_data <- remove.fossil.iNaturalist(aves_occ_data)

# Remove records outside CCMA
# Takes 1h to run
aves_occ_data <- clip.ccma(aves_occ_data)

# Save data
write.csv(aves_occ_data,
          "../data/processed-data/gbif-aves-clipped.csv")

