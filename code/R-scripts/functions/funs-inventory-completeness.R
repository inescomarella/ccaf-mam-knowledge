plot.inventory.completeness <- function(sf_obj) {
  sf_obj %>%
    filter(!is.na(c)) %>%
    mutate(c = c * 100) %>%
    ggplot() +
    geom_sf(size = 0, aes(fill = c)) +
    geom_sf(data = ccaf, fill = NA) +
    geom_sf(data = cus_longlat,
            fill = NA,
            size = 0.2) +
    scale_fill_viridis(
      limits = c(0, 100),
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c(0, 20, 40, 60, 80, 100)
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


research.institute.impact <- function(grid, institutes_points) {
  utm <-
    CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Set crs
  institutes_points <- st_transform(institutes_points, utm)
  grid <- st_transform(grid, utm)
  
  grid_ids <- unique(grid$grid_id)
  
  # Get research institutes names
  institutes <- institutes_points$institution_name
  
  pri_df <- data.frame()
  for (i in 1:length(institutes)) {
    for (j in 1:length(grid_ids)) {
      
      # Total number of records per grid cell
      nrec_j <- grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j]
        ) %>%
        nrow()
      
      # Number of records from a certain research institute per grid cell
      nrec_ij <-
        grid %>%
        group_by(grid_id) %>%
        filter(
          grid_id == grid_ids[j] &
            institutionCode == institutes[i]
        ) %>%
        nrow()
      
      # Proximity to research institute data.frame
      dist_ij <-
        st_distance(
          x = st_geometry(filter(grid, grid_id == grid_ids[j]))[1],
          y = st_geometry(institutes_points)[i]
        )
      
      # Relative contribution of a certain research institute
      rel_contrib_j <- nrec_ij / nrec_j
      
      if(is.na(as.numeric(dist_ij)) | as.numeric(dist_ij) == 0){
        dist_ij <- 1
      }
      
      pri_df[j, i] <- rel_contrib_j / as.numeric(dist_ij)
    }
  }
  
  pri_impact <- pri_df %>%
    # Remove columns with sum = 0
    select(which(!colSums(pri_df, na.rm = TRUE) %in% 0)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>%
    
    # Sum relative contribution * proximity of each institute per grid cell
    mutate(impact = sum(c_across(-c(grid_id)), na.rm = TRUE)) %>%
    select(grid_id, impact)
  
  
  merge(grid, pri_impact, by = "grid_id")
}
