# File purpose: Make a video os number os records in CCMA through time
# Data: 30/11/020

library(tidyverse)
library(animation)
library(sf)
library(rlang)

conflicted::conflict_prefer("filter", "dplyr")

# Load data -------------------------------------------------------------------
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
  nreg <- 0
  suppressMessages({
    for (i in 1:length(unique(pts$year))) {
      pts_filtered_year <-
        pts %>%
        filter(year == sort(unique(pts$year))[i])

      nreg2 <- lengths(st_intersects(map_geom, pts_filtered_year))
      nreg <- nreg + nreg2
      map_geom <- bind_cols(map_geom, nreg)
    }
  })

  name_list <- sub(" ", "", paste("y", sort(unique(pts$year))))
  colnames(map_geom)[6:(ncol(map_geom) - 1)] <- name_list

  map_geom
}
map_nreg_along_years <- nreg_along_years(record_data, g025_geom[1:5])

# Plot -----------------------------------------------------------------------
plot_along_years <- function(myfill) {
  map_nreg_along_years <-
    map_nreg_along_years %>%
    mutate(across(starts_with("y"), as.numeric))

  nmax <-
    st_drop_geometry(map_nreg_along_years) %>%
    select({{ myfill }}) %>%
    max()

  ggplot(map_nreg_along_years) +
    geom_sf(aes_string(fill = {{ myfill }}), size = 0.2) +
    labs(fill = "Number of \n mammal records") +
    scale_fill_viridis_b(
      limits = c(0, nmax),
      breaks = c(
        0,
        round(nmax / 6, 0),
        round(2 * nmax / 6, 0),
        round(3 * nmax / 6, 0),
        round(4 * nmax / 6, 0),
        round(5 * nmax / 6, 0),
        nmax
      )
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

myfill_list <- colnames(map_nreg_along_years)[6:(ncol(map_nreg_along_years) - 1)]
plot_list <- lapply(myfill_list, FUN = plot_along_years)

# Save video ----------------------------------------------------------------

saveVideo(
  expr = print(plot_list),
  video.name = "animation.mp4",
  img.name = "Rplot",
  ffmpeg = ani.options("ffmpeg"),
  other.opts='-pix_fmt yuv420p -b 1000k'
)

saveHTML(expr = {
  png(ani.options("img.fmt"))
  print(plot_list)
  dev.off()
}, img.name = "custom_plot", use.dev = FALSE, ani.type = "png", 
description = "Note how we use our own graphics device in 'expr'.", 
htmlfile = "custom_device.html")
