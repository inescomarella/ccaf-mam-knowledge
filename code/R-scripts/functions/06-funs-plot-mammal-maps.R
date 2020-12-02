# File purpose: Count record number of mammal orders and species
# Date: 17/11/2020

library(sf)
library(FNN)
library(dplyr)
library(conflicted)

conflict_prefer(name = "filter", winner = "dplyr")

count.orders.recs.in.polygons <-
  function(pts, polygons, order_list) {
    # Count number of records of each order in each polygon
    # Args:
    #   pts: sf point object containing a "order" column
    #   polygons: sf multipolygon object
    #   order_list: list of characters containing order names
    #
    # Output:
    #   polygons sf with the record count of each order in columns named as the
    #   order name

    for (j in 1:length(order_list)) {
      countPts <- c()
      suppressMessages({
        for (i in 1:nrow(polygons)) {
          polySelect <- polygons[i, ]
          pts2 <- st_intersection(pts, polySelect)
          obj <-
            pts2 %>%
            filter(order == as.character(order_list[j]))
          countPts[i] <- nrow(obj)
        }
      })
      polygons[, (ncol(polygons) + 1)] <- countPts
    }
    x <- ncol(polygons) - length(order_list) + 1
    colnames(polygons)[x:ncol(polygons)] <- order_list
    
    polygons
  }

count.sp.in.polygons <- function(pts, polygons) {
  # Count number of species in each polygon
  # Args:
  #   pts: sf point object containing a "species" columns
  #   polygons: sf multipolygon object
  #
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

  cbind(polygons, nsp)
}
