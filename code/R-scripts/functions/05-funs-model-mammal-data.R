# File purpose: functions useful to do measures necessary to model
# Date: 17/11/2020

library(sf)
library(FNN)

get.nearest.dist <- function(pts, polygon) {
  # Get distance from the centre point of each polygon in a multipolygon sf to
  # the nearest point in a point sf
  # Args:
  #   pts: sf point object
  #   polygons: sf multipolygon object
  # Output:
  #   polygon sf with an extra column called "dist_inst" containing the distance
  #   to the nearest research institute

  # Get centroid points to measure distance
  centroid <- coordinates(as(polygon, "Spatial"))
  pts_coords <- st_coordinates(pts)

  # Centre point distance to the nearest research institute
  dist <- get.knnx(pts_coords, centroid, k = 1)

  # Add distance to polygon attribute table
  polygon$dist_inst <- as.data.frame(dist)$nn.dist
  
  polygon
}
