# File purpose: Functions to process data and plot maps to make animation
# Date: 30/11/2020

xfun::pkg_attach(c(
  "rlang",
  "sf",
  "dplyr",
  "ggspatial",
  "cowplot",
  "ggplot2",
  "viridis"
))

conflicted::conflict_prefer("filter", "dplyr")

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

nrec.along.years <- function(pts, map_geom) {
  # Create table with count of records along years
  #
  # Args:
  #   pts: sf point map with column "year"
  #   map_gom: sf multipolygon map

  # Remove year with "NA" (in this case is "NA", not NA)
  pts <-
    pts %>%
    filter(year != "NA") %>%
    mutate(year = as.character(year))

  nrec <- 0
  suppressMessages({
    for (i in 1:length(unique(pts$year))) {
      pts_filtered_year <-
        pts %>%
        filter(year == sort(unique(pts$year))[i])

      nrecy <- lengths(st_intersects(map_geom, pts_filtered_year))

      # Make it cumulative through years
      nrec <- nrec + nrecy
      map_geom <- bind_cols(map_geom, nrec)
    }
  })

  # Column names as number is very problematic, so add an letter (without space)
  # Identify columns with counts for each year
  name_list <- sub(" ", "", paste("y", sort(unique(pts$year))))
  colnames(map_geom)[3:(ncol(map_geom) - 1)] <- name_list

  map_geom
}

nsp.along.years <- function(pts, map_geom) {
  # Create table with count of species recorded along years
  #
  # Args:
  #   pts: sf point map with column "year"
  #   map_gom: sf multipolygon map
  
  # Remove year with "NA" (in this case is "NA", not NA)
  pts <-
    pts %>%
    filter(year != "NA") %>%
    mutate(year = as.character(year))
  
  nsp <- 0
  suppressMessages({
    for (i in 1:length(unique(pts$year))) {
      pts_filtered_year <-
        pts %>%
        filter(year <= sort(unique(pts$year))[i])
      
      nspy <- c()
      for (j in 1:nrow(map_geom)) {
        polySelect <- map_geom[j, ]
        pts2 <- st_intersection(pts_filtered_year, polySelect)
        nspy[j] <- length(unique(pts2$species))
      }
      
      map_geom <- bind_cols(map_geom, nspy)
    }
  })
  
  # Column names as number is very problematic, so add an letter (without space)
  # Identify columns with counts for each year
  name_list <- sub(" ", "", paste("y", sort(unique(pts$year))))
  colnames(map_geom)[3:(ncol(map_geom) - 1)] <- name_list
  
  map_geom
}

plot.nrec.along.years <- function(myfill) {
  # Plot map for each year
  #
  # Arg:
  #   myfill: list containing the column names

  # The map wasn't add in the function input base apparently so far there is
  # no easy way to use apply to a sf object, so I added the map inside the
  # function instead of adding it in the function input
  map_nrec_along_years <-
    map_nrec_along_years %>%
    mutate(across(starts_with("y"), as.numeric))

  # Get maximum number of records to set break points later
  nmax <-
    st_drop_geometry(map_nrec_along_years) %>%
    select(y2020) %>%
    max()

  # The {{ }} is a trick from rlang package
  plot <-
    ggplot(map_nrec_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Records") +
    annotate(
      geom = "text",
      x = -38,
      y = -15,
      size = 10,
      family = "Lato",
      label = paste(sub("y", "", myfill))
    ) +
    scale_fill_viridis(
      limits = c(1, nmax),
      breaks = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      ),
      labels = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      )
    ) +
    theme_light() +
    cowplot::background_grid("none") +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.3),
      axis.title = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.key.size = unit(0.7, "cm")
    ) +
    guides(fill = guide_colorbar(
      draw.ulim = FALSE,
      draw.llim = FALSE
    )) +
    # Scale bar in the bottom right
    annotation_scale(location = "br", width_hint = 0.5) +
    # North arrow in the bottom right above scale bar
    annotation_north_arrow(
      location = "br",
      style = north_arrow_fancy_orienteering(text_size = 12),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      pad_y = unit(0.5, "in")
    )

  legend <- get_legend(plot)
  
  plot <-
    plot + theme(legend.position = "none", title = element_blank())

  if(sub("y", "", myfill) >= 1949) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 1949),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if (sub("y", "", myfill) >= 1960) {
    plot <-
      plot +
      geom_sf(
        data = filter(institute_pts, year == 1960),
        size = 1.5,
        color = "white",
        pch = 17
      ) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    )
  }
  
  if(sub("y", "", myfill) >= 1986) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 1986),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2005) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 2005),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2006) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 2006),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  ggdraw(plot) +
    draw_plot(legend,
      hjust = -0.2
    )
}

plot.nsp.along.years <- function(myfill) {
  # Plot map for each year
  #
  # Arg:
  #   myfill: list containing the column names
  
  # The map wasn't add in the function input base apparently so far there is
  # no easy way to use apply to a sf object, so I added the map inside the
  # function instead of adding it in the function input
  map_nsp_along_years <-
    map_nsp_along_years %>%
    mutate(across(starts_with("y"), as.numeric))
  
  # Get maximum number of records to set break points later
  nmax <-
    st_drop_geometry(map_nsp_along_years) %>%
    select(y2020) %>%
    max()
  
  
  # The {{ }} is a trick from rlang package
  plot <-
    ggplot(map_nsp_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Species \nrecorded") +
    annotate(
      geom = "text",
      x = -38,
      y = -15,
      size = 10,
      family = "Lato",
      label = paste(sub("y", "", myfill))
    ) +
    scale_fill_viridis(
      limits = c(1, nmax),
      breaks = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      ),
      labels = c(
        1,
        round(nmax / 6, 0),
        round(nmax * 2 / 6, 0),
        round(nmax * 3 / 6, 0),
        round(nmax * 4 / 6, 0),
        round(nmax * 5 / 6, 0),
        nmax
      )
    ) +
    theme_light() +
    cowplot::background_grid("none") +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.3),
      axis.title = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.key.size = unit(0.7, "cm")
    ) +
    guides(fill = guide_colorbar(
      draw.ulim = FALSE,
      draw.llim = FALSE
    )) +
    
    # Scale bar in the bottom right
    annotation_scale(location = "br", width_hint = 0.5) +
    
    # North arrow in the bottom right above scale bar
    annotation_north_arrow(
      location = "br",
      style = north_arrow_fancy_orienteering(text_size = 12),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      pad_y = unit(0.5, "in")
    )
  
  legend <- get_legend(plot)
  plot <-
    plot + theme(legend.position = "none", title = element_blank())
  
  if(sub("y", "", myfill) >= 1949) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 1949),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if (sub("y", "", myfill) >= 1960) {
    plot <-
      plot +
      geom_sf(
        data = filter(institute_pts, year == 1960),
        size = 1.5,
        color = "white",
        pch = 17
      ) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 1986) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 1986),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2005) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 2005),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2006) {
    plot <- 
      plot +
      geom_sf(data = filter(institute_pts, year == 2006),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  ggdraw(plot) +
    draw_plot(legend,
              hjust = -0.2
    )
}

plot.together.years <- function(myfill){
  map_nrec_along_years <-
    map_nrec_along_years %>%
    mutate(across(starts_with("y"), as.numeric))
  
  map_nsp_along_years <-
    map_nsp_along_years %>%
    mutate(across(starts_with("y"), as.numeric))
  
  # Get maximum number of records to set break points later
  nmax_nrec <-
    st_drop_geometry(map_nrec_along_years) %>%
    select(y2020) %>%
    max()
  
  nmax_sp <-
    st_drop_geometry(map_nsp_along_years) %>%
    select(y2020) %>%
    max()
  
  
  # The {{ }} is a trick from rlang package
  plot_nrec <-
    ggplot(map_nrec_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Records") +
    annotate(
      geom = "text",
      x = -38,
      y = -14.4,
      size = 10,
      family = "Lato",
      label = paste(sub("y", "", myfill))
    ) +
    scale_fill_viridis(
      limits = c(1, nmax_nrec),
      breaks = c(
        1,
        round(nmax_nrec / 6, 0),
        round(nmax_nrec * 2 / 6, 0),
        round(nmax_nrec * 3 / 6, 0),
        round(nmax_nrec * 4 / 6, 0),
        round(nmax_nrec * 5 / 6, 0),
        nmax_nrec
      ),
      labels = c(
        1,
        round(nmax_nrec / 6, 0),
        round(nmax_nrec * 2 / 6, 0),
        round(nmax_nrec * 3 / 6, 0),
        round(nmax_nrec * 4 / 6, 0),
        round(nmax_nrec * 5 / 6, 0),
        nmax_nrec
      )
    ) +
    theme_light() +
    cowplot::background_grid("none") +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.3),
      axis.title = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.key.size = unit(0.7, "cm")
    ) +
    guides(fill = guide_colorbar(
      draw.ulim = FALSE,
      draw.llim = FALSE
    )) +
    
    # Scale bar in the bottom right
    annotation_scale(location = "br", width_hint = 0.5) +
    
    # North arrow in the bottom right above scale bar
    annotation_north_arrow(
      location = "br",
      style = north_arrow_fancy_orienteering(text_size = 12),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      pad_y = unit(0.5, "in")
    )
  
  plot_nsp <-
    ggplot(map_nsp_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Species \nrecorded") +
    annotate(
      geom = "text",
      x = -38,
      y = -14.4,
      size = 10,
      family = "Lato",
      label = paste(sub("y", "", myfill))
    ) +
    scale_fill_viridis(
      limits = c(1, nmax_sp),
      breaks = c(
        1,
        round(nmax_sp / 6, 0),
        round(nmax_sp * 2 / 6, 0),
        round(nmax_sp * 3 / 6, 0),
        round(nmax_sp * 4 / 6, 0),
        round(nmax_sp * 5 / 6, 0),
        nmax_sp
      ),
      labels = c(
        1,
        round(nmax_sp / 6, 0),
        round(nmax_sp * 2 / 6, 0),
        round(nmax_sp * 3 / 6, 0),
        round(nmax_sp * 4 / 6, 0),
        round(nmax_sp * 5 / 6, 0),
        nmax_sp
      )
    ) +
    theme_light() +
    cowplot::background_grid("none") +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.3),
      axis.title = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.key.size = unit(0.7, "cm")
    ) +
    guides(fill = guide_colorbar(
      draw.ulim = FALSE,
      draw.llim = FALSE
    )) +
    
    # Scale bar in the bottom right
    annotation_scale(location = "br", width_hint = 0.5) +
    
    # North arrow in the bottom right above scale bar
    annotation_north_arrow(
      location = "br",
      style = north_arrow_fancy_orienteering(text_size = 12),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      pad_y = unit(0.5, "in")
    )
  
  legend_nrec <- get_legend(plot_nrec)
  legend_nsp <- get_legend(plot_nsp)
  
  plot_nrec <-
    plot_nrec + theme(legend.position = "none", title = element_blank())
  plot_nsp <-
    plot_nsp + theme(legend.position = "none", title = element_blank())
  
  if(sub("y", "", myfill) >= 1949) {
    plot_nrec <- 
      plot_nrec +
      geom_sf(data = filter(institute_pts, year == 1949),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
    plot_nsp <- 
      plot_nsp +
      geom_sf(data = filter(institute_pts, year == 1949),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if (sub("y", "", myfill) >= 1960) {
    plot_nrec <-
      plot_nrec +
      geom_sf(
        data = filter(institute_pts, year == 1960),
        size = 1.5,
        color = "white",
        pch = 17
      ) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
    
    plot_nsp <-
      plot_nsp +
      geom_sf(
        data = filter(institute_pts, year == 1960),
        size = 1.5,
        color = "white",
        pch = 17
      ) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 1986) {
    plot_nrec <- 
      plot_nrec +
      geom_sf(data = filter(institute_pts, year == 1986),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
    
    plot_nsp <- 
      plot_nsp +
      geom_sf(data = filter(institute_pts, year == 1986),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2005) {
    plot_nrec <- 
      plot_nrec +
      geom_sf(data = filter(institute_pts, year == 2005),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
    
    plot_nsp <- 
      plot_nsp +
      geom_sf(data = filter(institute_pts, year == 2005),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  if(sub("y", "", myfill) >= 2006) {
    plot_nrec <- 
      plot_nrec +
      geom_sf(data = filter(institute_pts, year == 2006),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
    
    plot_nsp <- 
      plot_nsp +
      geom_sf(data = filter(institute_pts, year == 2005),
              size = 1.5,
              color = "white",
              pch = 17) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      )
  }
  
  plot_nrec_final <- ggdraw(plot_nrec) +
    draw_plot(legend_nrec,
              hjust = -0.3
    )
  plot_nsp_final <- ggdraw(plot_nsp) +
    draw_plot(legend_nsp,
              hjust = -0.3
    )
  
  plot_grid(plot_nsp_final, plot_nrec_final, nrow = 1)
}
