# File purpose: functions useful to prepare data to plot maps of classes data
# Date: 20/11/2020

# Load in libraries
library(dplyr)
library(sf)
library(dplyr)
library(conflicted)

conflict_prefer('filter', 'dplyr')

count.class.in.polygons <- function(pts, polygons, class_name) {
  # Count number of records of a certain class in each polygon
  # Args:
  #   pts: sf point object containing a "class" column
  #   polygons: sf multipolygon object
  #   class_name: character object containing the class name
  #
  # Output:
  #   polygons sf with the record count of each order in columns named as the
  #   order name
  
  countPts <- c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i,]
      pts2 <- st_intersection(pts, polySelect)
      obj <-
        pts2 %>%
        filter(class == as.character(class_name))
      countPts[i] <- nrow(obj)
    }
    
  })
  
  return(bind_cols(polygons, countPts))
}

count.sp.in.polygons <- function(pts, polygons) {
  # Count number of species in each polygon
  # Args:
  #   pts: sf point object containing a "species" columns
  #   polygons: sf multipolygon object
  # Output:
  #   polygons sf with an extra column called "nsp" containing the number
  #   of species
  
  nsp <- c()
  suppressMessages({
    for (i in 1:nrow(polygons)) {
      polySelect <- polygons[i, ]
      pts2 <- st_intersection(pts, polySelect)
      nsp[i] <- length(unique(pts2$species))
    }
  })
  polygons <- bind_cols(polygons, nsp)
  colnames(polygons)[82] <- "nsp"
  
  return(polygons)
}
