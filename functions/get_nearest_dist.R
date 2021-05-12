library(FNN)

get_nearest_dist <- function(pts, polygon) {
  # Get distance from the central point of each polygon in a multipolygon sf to
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
  
  # Central point distance to the nearest research institute
  dist <- get.knnx(pts_coords, centroid, k = 1)
  
  # Add distance to polygon attribute table
  polygon$dist_inst <- as.data.frame(dist)$nn.dist
  
  st_as_sf(polygon)
}
