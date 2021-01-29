plot.inventory.completeness <- function(sf_obj) {
  sf_obj %>%
    filter(completeness != "Not surveyed") %>%
    mutate(c = c * 100) %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = c)) +
    geom_sf(data = ccaf, fill = NA) +
    geom_sf(data = cus_longlat,
            fill = NA,
            size = 0.2) +
    scale_fill_viridis(
      limits = c(0.5, 100),
      breaks = c(0.5, 20, 40, 60, 80, 100),
      labels = c(0.5, 20, 40, 60, 80, 100)
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(fill = element_blank())
}

get.nearest.dist <- function(pts, polygon) {
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
