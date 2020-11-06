library(dplyr)
library(sf)

count.sp.in.polygons <- function(pts, polygons){
  countPts = c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i, ]
      pts2 <- st_intersection(pts, polySelect)
      countPts[i] = length(unique(pts2$species))
      
    }
  })
  
  return(cbind(polygons,countPts))
}

count.order.in.polygons <- function(pts, polygons, order_name){
  countPts = c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i,]
      pts2 <- st_intersection(pts, polySelect)
      obj <- pts2 %>% filter(order == as.character(order_name))
      countPts[i] = nrow(obj)
    }
    
  })
  
  return(cbind(polygons,countPts))
}

count.class.in.polygons <- function(pts, polygons, class_name){
  countPts = c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i,]
      pts2 <- st_intersection(pts, polySelect)
      obj <- pts2 %>% filter(class == as.character(class_name))
      countPts[i] = nrow(obj)
    }
    
  })
  
  return(cbind(polygons,countPts))
}

clip.ccma <- function(pts) {
  to_remove <- pts %>% dplyr::filter(
    is.na(decimalLongitude) |
      decimalLongitude == "" |
      is.na(decimalLatitude) |
      decimalLatitude == ""
  )
  pts <- anti_join(pts, to_remove)
  data_st <-
    st_as_sf(pts, coords = c('decimalLongitude', 'decimalLatitude'))
  data_crs <- st_set_crs(data_st, sp::CRS("+proj=longlat +datum=WGS84"))
  ccma_crs <- st_transform(ccma, crs = st_crs(data_crs))
  data_clipped <- st_intersection(data_crs, ccma_crs)
  coords <- as.data.frame(st_coordinates(data_clipped))
  
  data_clipped_df <- st_set_geometry(data_clipped, NULL)
  data_clipped_df <- data_clipped_df[1:(ncol(data_clipped_df) - 2)]
  
  data_clipped_df$decimalLongitude <- coords$X
  data_clipped_df$decimalLatitude <- coords$Y
  return(data_clipped_df)
}

count.scientificName.in.polygons <- function(pts, polygons){
  countPts = c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i, ]
      pts2 <- st_intersection(pts, polySelect)
      countPts[i] = length(unique(pts2$scientificName))
      
    }
  })
  
  return(cbind(polygons,countPts))
}

remove.fossil.iNaturalist <- function(dataframe) {
  to_remove <-
    dataframe %>% 
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             str_detect(institutionCode, "iNaturalist"))
  df_clean <- anti_join(dataframe, to_remove)
  return(df_clean)
}

only.indetified.sp <- function(dataframe) {
  to_remove_scientificName <-
    dataframe %>% filter(
      is.na(scientificName) |
        str_detect(scientificName, " ") == FALSE
    )
  to_remove_verbatimSc <-
    dataframe %>% filter(
      is.na(verbatimScientificName) |
        str_detect(verbatimScientificName, " ") == FALSE
    )
  to_remove <- dplyr::intersect(to_remove_verbatimSc, to_remove_scientificName)
  data <- anti_join(dataframe, to_remove_scientificName)
  to_return_scientificName <- anti_join(to_remove_scientificName, to_remove)
  to_return_scientificName$scientificName <- to_return_scientificName$verbatimSc
  data <- bind_rows(data, to_return_scientificName)
  return(data)
}

count.orders.list.in.polygons <- function(pts, polygons, order_list){
  for (j in 1:length(order_list)) {
    countPts = c()
    suppressMessages({
      for (i in 1:nrow(polygons)) {
        polySelect <- polygons[i, ]
        pts2 <- st_intersection(pts, polySelect)
        obj <- pts2 %>% filter(order == as.character(order_list[j]))
        countPts[i] = nrow(obj)
      }
      
    })
    polygons[, (ncol(polygons) + 1)] <- countPts
  }
  colnames(polygons)[(ncol(polygons)-length(order_list)+1):ncol(polygons)] <- order_list
  return(polygons)
}