# Date: 07/04/2021

xfun::pkg_attach2(c("tidyverse", "openxlsx", "tidymodels", "recipes", "sf", "sp", "dotwhisker", "bbmle"))

source("functions/research_institute_proximity.R")
source("functions/model_selection.R")

# Set projections
longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data -------------------------
grid_data <- read_sf("data/processed/maps/grid_data.shp")

ccaf <- read_sf("data/processed/maps/ccaf_map.shp")

records <-
  st_read(
    dsn = "data/processed/clean_database.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    )
  ) %>%
  mutate(id = seq(1, nrow(.)))

institutes <-
  st_read(
    dsn = "data/raw/research_institutes.csv",
    crs = longlat,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Pre-process data --------------------------------
proximity <-
  research_institute_proximity(
    grid = st_join(records, grid_data),
    institutes_points = institutes
  )

data <- merge(grid_data, proximity, all = TRUE) %>%
  st_drop_geometry() %>%
  mutate(proximity = ifelse(is.na(proximity), 0, proximity)) %>%
  select(nrec, CU, forestw, proximity) %>%
  filter(!is.na(nrec))

# Fit model ----------------------

mod_nrec <- model_selection(data)

# Model selection results
mod_nrec$aic_tab

# Inspect coefficients from the best model
summary(mod_nrec$modelos$m_full)

# Calculate predicted values + SE
pred_nrec_glm <- predict(mod_nrec$modelos$m_full, type = "response", se.fit = TRUE)
df_nrec_glm <- data.frame(data,
  fit = pred_nrec_glm$fit,
  upr = pred_nrec_glm$fit + 2 * pred_nrec_glm$se.fit,
  lwr = pred_nrec_glm$fit - 2 * pred_nrec_glm$se.fit
)

# Format AIC table
aic_df <- as.data.frame(mod_nrec$aic_tab) %>%
  mutate(across(where(is.numeric), round, 2),
    model = c(
      "proximity + CU + forest",
      "proximity + CU",
      "proximity + forest",
      "proximity",
      "Null"
    ),
    weight = ifelse(.$weight == 1, "1", "<0.0001")
  ) %>%
  relocate(model, .before = AICc)

# Export model -------------------------------------------
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

write.csv(df_nrec_glm, paste0("outputs/model_table_glm.csv"),
  row.names = FALSE
)
write.csv(aic_df, paste0("outputs/aic_table_glm.csv"),
  row.names = TRUE
)
