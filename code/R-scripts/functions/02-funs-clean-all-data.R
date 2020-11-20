# File purpose: functions useful to clean mammal data from papers, GBIF and
# speciesLink
# Using minimal number of objects to save memory and make (try to) it run faster
# 
# Date: 16/11/2020

library(dplyr)
library(sf)
library(sp)
library(conflicted)

conflict_prefer("summarise", "dplyr")

rl.synonyms <- function(x) {
  # Manipulate rl_synonyms() S4 object to get "results" and "name" as a 
  # dataframe
  # Make the rl_synonyms() applicable to a list of species
  # Args:
  #   x: species name as character
  
  synym <- rl_synonyms(x, key = rlkey)
  result <- synym$result
  scientificName <- synym$name
  results <- bind_cols(scientificName, result)
  return(results)
}


name.backbone <- function(x) {
  # Add a scientificName column containing the species name as written in the
  # input
  # Arg: 
  #   x: species names as character
  
  bckbn <- name_backbone(x)
  bckbn$scientificName <- x
  return(bckbn)
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
    st_as_sf(pts, coords = c("decimalLongitude", "decimalLatitude"))
  pts <-
    st_set_crs(pts, CRS("+proj=longlat +datum=WGS84"))
  ccma <- st_transform(ccma, crs = CRS("+proj=longlat +datum=WGS84"))
  pts <- st_intersection(pts, ccma)
  coords <- as.data.frame(st_coordinates(pts))
  
  pts <- st_set_geometry(pts, NULL)
  pts <- pts[1:(ncol(pts) - 2)]
  
  pts$decimalLongitude <- coords$X
  pts$decimalLatitude <- coords$Y
  return(pts)
}
