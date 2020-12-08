# File purpose: Functions to process data and plot maps to make animation
# Date: 30/11/2020

library(rlang)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(cowplot)
library(ggspatial)


conflicted::conflict_prefer("filter", "dplyr")

nreg.along.years <- function(pts, map_geom) {
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

  nreg <- 0
  suppressMessages({
    for (i in 1:length(unique(pts$year))) {
      pts_filtered_year <-
        pts %>%
        filter(year == sort(unique(pts$year))[i])

      nregy <- lengths(st_intersects(map_geom, pts_filtered_year))

      # Make it cumulative through years
      nreg <- nreg + nregy
      map_geom <- bind_cols(map_geom, nreg)
    }
  })

  # Column names as number is very problematic, so add an letter (without space)
  # Identify columns with counts for each year
  name_list <- sub(" ", "", paste("y", sort(unique(pts$year))))
  colnames(map_geom)[3:(ncol(map_geom) - 1)] <- name_list

  map_geom
}

plot.along.years <- function(myfill) {
  # Plot map for each year
  #
  # Arg:
  #   myfill: list containing the column names

  # The map wasn't add in the function input base apparently so far there is
  # no easy way to use apply to a sf object, so I added the map inside the
  # function instead of adding it in the function input
  map_nreg_along_years <-
    map_nreg_along_years %>%
    mutate(across(starts_with("y"), as.numeric))

  # Get maximum number of records to set break points later
  nmax <-
    st_drop_geometry(map_nreg_along_years) %>%
    select(y2020) %>%
    max()

  
  # The {{ }} is a trick from rlang package
    plot <-
      ggplot(map_nreg_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
      coord_sf(
        # Limits of the ccaf bbox
        xlim = c(-41.87851, -37),
        expand = TRUE,
        label_graticule = "NW"
      ) +
    labs(fill = "Records") +
    annotate(
      geom = "text", x = -38, y = -15, size = 10, family = "Lato",
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
      legend.key.size = unit(0.7, 'cm')
    ) +
    guides(
      fill = guide_colorbar(
        draw.ulim = FALSE,
        draw.llim = FALSE
      )
    ) +
    
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
    plot <- plot + theme(legend.position='none', title = element_blank())
    
    ggdraw(plot) +
      draw_plot(legend, 
                hjust = -0.2)
    
  }
