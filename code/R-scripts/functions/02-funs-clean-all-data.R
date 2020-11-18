# File purpose: functions useful to get species taxonomy
# Date: 16/11/2020

library(dplyr)
library(sf)
library(sp)
library(conflicted)

conflict_prefer("summarise", "dplyr")

rl.synonyms <- function(x) {
  # Manipulate rl_synonyms() S4 object to get 'results' and 'name' as a 
  # dataframe
  # Make the rl_synonyms() applicable to a list of species
  # Args:
  #   x: species name as character
  
  synym <- rl_synonyms(x, key = rlkey)
  result <- synym$result
  scientificName <- synym$name
  results <- cbind(scientificName, result)
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
  data_st <-
    st_as_sf(pts, coords = c('decimalLongitude', 'decimalLatitude'))
  data_crs <-
    st_set_crs(data_st, CRS("+proj=longlat +datum=WGS84"))
  ccma_crs <- st_transform(ccma, crs = st_crs(data_crs))
  data_clipped <- st_intersection(data_crs, ccma_crs)
  coords <- as.data.frame(st_coordinates(data_clipped))
  
  data_clipped_df <- st_set_geometry(data_clipped, NULL)
  data_clipped_df <- data_clipped_df[1:(ncol(data_clipped_df) - 2)]
  
  data_clipped_df$decimalLongitude <- coords$X
  data_clipped_df$decimalLatitude <- coords$Y
  return(data_clipped_df)
}
