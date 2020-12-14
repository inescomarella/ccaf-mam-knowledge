
xfun::pkg_attach(c(
  "sp",
  "sf",
  "dplyr",
  "cowplot",
  "ggplot2",
  "viridis",
  "ggspatial"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")

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
plot.ccaf.grid.count <- function(myfill) {
  # Plot map for each year
  #
  # Arg:
  #   myfill: list containing the column names
  
  # The map wasn't add in the function input base apparently so far there is
  # no easy way to use apply to a sf object, so I added the map inside the
  # function instead of adding it in the function input
  
  institute_pts <-
    st_read(
      dsn = "../data/raw-data/research-institutes.csv",
      crs = CRS("+proj=longlat +datum=WGS84"),
      options = c(
        "X_POSSIBLE_NAMES=longitude",
        "Y_POSSIBLE_NAMES=latitude"
      )
    )
  
  # Get maximum number of records to set break points later
  nmax <-
    st_drop_geometry(ccaf_grid) %>%
    select({{ myfill }}) %>%
    max()
  
  # The {{ }} is a trick from rlang package
  plot <- 
    ccaf_grid %>%
    ggplot() +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    geom_sf(
      data = institute_pts,
      size = 1,
      color = "white",
      pch = 17
    ) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Records") +
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
    background_grid("none") +
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
    annotation_scale(location = "br", width_hint = 0.2) +
    
    # North arrow in the bottom right above scale bar
    annotation_north_arrow(
      location = "br",
      style = north_arrow_fancy_orienteering(text_size = 12),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      pad_y = unit(0.3, "in")
    )
  
  legend <- get_legend(plot)
  plot <-
    plot + theme(legend.position = "none", title = element_blank())
  
  ggdraw(plot) +
    draw_plot(legend,
              hjust = -0.26
    )
}

plot.nrec.nsps <- function(nrec, nsp){

  institute_pts <-
    st_read(
      dsn = "../data/raw-data/research-institutes.csv",
      crs = CRS("+proj=longlat +datum=WGS84"),
      options = c(
        "X_POSSIBLE_NAMES=longitude",
        "Y_POSSIBLE_NAMES=latitude"
      )
    )
  
  # Get maximum number of records to set break points later
  nmax_nrec <-
    st_drop_geometry(ccaf_grid) %>%
    select(nrec) %>%
    max()
  
  nmax_sp <-
    st_drop_geometry(ccaf_grid) %>%
    select(nsp) %>%
    max()
  
  plot_nrec <-
    ggplot(ccaf_grid) +
    geom_sf(aes(fill = nrec), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Records") +
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
    background_grid("none") +
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
      pad_y = unit(0.3, "in")
    )
  
  plot_nsp <-
    ggplot(ccaf_grid) +
    geom_sf(aes(fill = nsp), size = 0.2) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    labs(fill = "Species \nrecorded") +
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
    background_grid("none") +
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
      pad_y = unit(0.3, "in")
    )
  
  legend_nrec <- get_legend(plot_nrec)
  legend_nsp <- get_legend(plot_nsp)
  
  plot_nrec <-
    plot_nrec + theme(legend.position = "none", title = element_blank())
  plot_nsp <-
    plot_nsp + theme(legend.position = "none", title = element_blank())
  
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
