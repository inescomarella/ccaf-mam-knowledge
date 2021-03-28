research.institute.distance <- function(grid, institutes_points) {
  
  # Set crs
  institutes_points <- st_transform(institutes_points, longlat)
  grid <- st_transform(grid, longlat)
  
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
      
      grid_centroid <- grid %>%
        filter(grid_id == grid_ids[j]) %>%
        st_centroid() %>%
        head(1)
      
      # Proximity to research institute data.frame
      dist_ij <-
        st_distance(
          x = grid_centroid,
          y = institutes_points[i, ],
          which = "Great Circle"
        )
      
      # Relative contribution of a certain research institute
      rel_contrib_j <- nrec_ij / nrec_j
      
      # if(is.na(as.numeric(dist_ij)) | as.numeric(dist_ij) == 0){
      #  dist_ij <- 0.05
      # }
      
      pri_df[j, i] <- rel_contrib_j / as.numeric(dist_ij)
    }
  }
  
  pri_impact <- pri_df %>%
    # Remove columns with sum = 0
    select(which(!colSums(pri_df, na.rm = TRUE) %in% 0)) %>%
    mutate(grid_id = grid_ids) %>%
    rowwise() %>%
    
    # Sum relative contribution * proximity of each institute per grid cell
    mutate(distance = sum(c_across(-c(grid_id)), na.rm = TRUE)) %>%
    select(grid_id, distance)
  
  
  merge(grid, pri_impact, by = "grid_id")
}
