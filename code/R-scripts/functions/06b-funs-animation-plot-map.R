# File purpose: Functions to process data and plot maps to make animation
# Date: 30/11/2020

library(rlang)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)

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
  colnames(map_geom)[6:(ncol(map_geom) - 1)] <- name_list

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
    select({{ myfill }}) %>%
    max()

  # The {{ }} is a trick from rlang package
  ggplot(map_nreg_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    labs(fill = "Number of \n mammal records") +
    scale_fill_viridis(
      limits = c(10, 1600), breaks = c(10, 265, 530, 795, 1060, 1325, 1600), labels = c(10, 265, 530, 795, 1060, 1325, 1600)
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.75),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    ggtitle(paste(sub("y", "", myfill))) +
    guides(
      fill = guide_colorbar(
        draw.ulim = FALSE,
        draw.llim = FALSE
      )
    )
}
