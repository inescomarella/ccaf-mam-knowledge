# File purpose: Functions useful to clean data from GBIF, when dealing with
# different classes, easy to use lapply()
# Date: 20/11/2020

# Load in libraries
library(dplyr)
library(stringr)
library(conflicted)
library(sf)
library(sp)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("intersect", "dplyr")

remove.fossil.iNaturalist <- function(dataset) {
  # Remove fossil and iNaturalist records
  #
  # Arg:
  #   dataset: GBIF data containing basisOfRecord and institutionCode columns
  #
  # Putput: GBIF data.frame
  
  to_remove <-
    dataset %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             str_detect(institutionCode, "iNaturalist"))
  dataset <- anti_join(dataset, to_remove)
  return(dataset)
}

clip.ccma <- function(pts) {
  # Use st_intersection() to remove points outside ccma layer (sf obj)
  #
  # Args:
  #   pts: dataframe with decimalLongitude and decimalLatitude columns
  #   specifying coordinates
  
  to_remove <-
    pts %>%
    filter(
      is.na(decimalLongitude) |
        decimalLongitude == "" |
        is.na(decimalLatitude) |
        decimalLatitude == ""
    )
  pts <- anti_join(pts, to_remove)
  pts <-
    st_as_sf(pts, coords = c('decimalLongitude', 'decimalLatitude'))
  pts <-
    st_set_crs(pts, CRS("+proj=longlat +datum=WGS84"))
  pts <- st_intersection(pts, ccma)
  coords <- as.data.frame(st_coordinates(pts))
  
  pts <- st_set_geometry(pts, NULL)
  pts <- pts[1:(ncol(pts) - 2)]
  
  pts$decimalLongitude <- coords$X
  pts$decimalLatitude <- coords$Y
  return(pts)
}

select.gbif.columns <- function(dataset) {
  # Select columns of interest of GBIF data, that's as they use to be large objects
  #
  # Args:
  #   dataset: GBIF data
  
  dataset <-
    dataset %>%
    select(
      class,
      order,
      family,
      species,
      scientificName,
      verbatimScientificName,
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
  return(dataset)
}

only.indetified.sp <- function(dataset) {
  # Keep only identified species in GBIF data
  #
  # Args:
  #   dataset: GBIF data
  
  to_remove_scientificName <-
    dataset %>% filter(is.na(scientificName) |
                         str_detect(scientificName, " ") == FALSE)
  to_remove_verbatimSc <-
    dataset %>% filter(is.na(verbatimScientificName) |
                         str_detect(verbatimScientificName, " ") == FALSE)
  to_remove <-
    intersect(to_remove_verbatimSc, to_remove_scientificName)
  dataset <- anti_join(dataset, to_remove_scientificName)
  to_return_scientificName <-
    anti_join(to_remove_scientificName, to_remove)
  to_return_scientificName$scientificName <-
    to_return_scientificName$verbatimSc
  dataset <- bind_rows(dataset, to_return_scientificName)
  return(dataset)
}
