# File purpose: Count record number of mammal orders and species
# Date: 17/11/2020

xfun::pkg_attach(c(
  "FNN",
  "sf",
  "dplyr",
  "ggspatial",
  "cowplot"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")

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

remove.legend <- function(x) {
  x + theme(legend.position = "none")
}

plot.nrec.order <- function(myfill) {
  # Plot map for each year
  #
  # Arg:
  #   myfill: list containing the column names

  # The map wasn't add in the function input base apparently so far there is
  # no easy way to use apply to a sf object, so I added the map inside the
  # function instead of adding it in the function input

  # Get maximum number of records to set break points later
  nmax <-
    st_drop_geometry(ccaf_grid) %>%
    select({{ myfill }}) %>%
    max()

  # The {{ }} is a trick from rlang package
  ccaf_grid %>%
    ggplot() +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    geom_sf(
      data = institute_pts,
      size = 0.7,
      color = "white",
      pch = 17
    ) +
    coord_sf(
      # Limits of the ccaf bbox
      xlim = c(-41.87851, -37),
      ylim = c(-21.30178, -12.5),
      expand = TRUE,
      label_graticule = "NW"
    ) +
    annotate(
      geom = "text",
      x = -39.5,
      y = -12.5,
      size = 5,
      family = "Lato",
      label = myfill
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
    cowplot::background_grid("none") +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.3),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      axis.text = element_text(size = 7),
      title = element_text(size = 12, family = "Lato"),
      axis.title = element_blank()
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
      style = north_arrow_fancy_orienteering(text_size = 8),
      height = unit(0.9, "cm"),
      width = unit(0.9, "cm"),
      pad_y = unit(0.26, "in")
    ) +
    theme(legend.key.size = unit(0.5, "cm"))
}
