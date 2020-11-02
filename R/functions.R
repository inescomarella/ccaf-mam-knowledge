library(dplyr)
library(sf)

count.sp.in.polygons <- function(pts, polygons){
  countPts = c()
  for (i in 1:nrow(polygons)) {
    polySelect <- polygons[i,]
    pts2 <- st_intersection(pts, polySelect)
    countPts[i] = length(unique(pts2$species))
    
  }
  
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
