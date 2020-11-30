# File purpose: Make a video os number os records in CCMA through time
# Data: 30/11/020

library(tidyverse)
library(animation)
library(sf)

record_data <-
  st_read(
    dsn = "../data/processed-data/clean-mammal-data.csv",
    crs = sp::CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  )
g025_geom <-
  st_read(
    dsn = "../data/processed-data",
    layer = "grid-025-ucs-joined"
)
g025_geom <- st_transform(g025_geom, sp::CRS("+proj=longlat +datum=WGS84"))

# Table ----------------------------------------------------------------------
nreg_along_years <- function(pts, map_geom) {
  pts <-
    pts %>%
    filter(year != "NA") %>%
    mutate(year = as.character(year))

  suppressMessages({
    for (i in 1:length(unique(pts$year))) {
      pts_filtered_year <-
        pts %>%
        filter(year == sort(unique(pts$year))[i])

      nreg <- lengths(st_intersects(map_geom, pts_filtered_year))
      map_geom <- bind_cols(map_geom, nreg)
    }
  })
  
  name_list <- sub(" ", "", paste("y", sort(unique(pts$year))))
  colnames(map_geom)[6:(ncol(map_geom)-1)] <- name_list
  
  map_geom
}
map_nreg_along_years <- nreg_along_years(record_data, g025_geom[1:5])

# Plot -----------------------------------------------------------------------
plot_along_years <- function(map_sf) {
  # Customized theme
  customPlot <- list(
    theme_light() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 0.75),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)
      )
  )

  ggplot(map_sf) +
    geom_sf(size = 0.2) +
    customPlot
}

test <-
  apply(map_nreg_along_years[6:(ncol(map_nreg_along_years) - 1)], 2, plot_along_years)

test <- plot_along_years(map_nreg_along_years)
map_sf <- map_nreg_along_years

saveVideo(
  expr_fun(x),
  video.name = "animation.mp4",
  img.name = "Rplot",
  ffmpeg = ani.options("ffmpeg"),
  other.opts = if (grepl("[.]mp4$",
                         video.name))
    "-pix_fmt yuv420p"
)