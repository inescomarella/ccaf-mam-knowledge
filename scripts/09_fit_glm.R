# Date: 12/05/2021

xfun::pkg_attach2(c("tidyverse", "tidymodels", "sf", "sp"))

source("functions/get_nearest_dist.R")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data ------------------------------------------------------------

grid_data <- read_sf("data/processed/maps/grid_data.shp")

institutes <-
  st_read(
    dsn = "data/raw/research_institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Pre-process data ------------------------------------------------------------

# Euclidean distances
grid_data_dist <- get_nearest_dist(institutes, grid_data)

data <- grid_data_dist %>%
  st_drop_geometry() %>%
  filter(!is.na(AP)) %>%
  select(nrec, dist_inst)

# Fit model ------------------------------------------------------------

# Linear regression model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit model
lm_fit <-
  lm_mod %>%
  fit(nrec ~ dist_inst, data = data)

tidy(lm_fit)

# Predict number of records
predicted <- lm_fit %>%
  predict(data) 

# Export plot ------------------------------------------------------------

# Plot predicted and observed number of recordd
bind_cols(data, predicted) %>%
  ggplot(aes(x = dist_inst)) +
  geom_point(aes(y = nrec), alpha = 0.3) +
  geom_line(aes(y = .pred), color = "red", size = 1) +
  labs(
    x = "Distance to nearest collection",
    y = "Number of records"
  ) +
  theme_light()

ggsave("figs/glm_plot.png")

# Save workspace ------------------------------------------------------------
save.image("~/tcc-ccma/workspaces/fit_glm.RData")
