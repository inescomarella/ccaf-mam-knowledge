# File purpose: Upload results to Google Drive
# Date: 27/11/2020

# Load library
library(googledrive)
library(stringr)

# Login ------------------------------------------------------

# Authorize googledrive to view and manage my Drive files
drive_auth(
  email = "inesmottacomarella@gmail.com",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

# Prepare folder and files -----------------------------------

# Check if folders already exist and create them
if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots/model")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots/model")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/species-knowledge-gaps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/species-knowledge-gaps")
}

# Get files path to upload
maps_paths <-
  list.files(
    "../data/results",
    pattern = "map",
    full.names = TRUE,
    recursive = TRUE
  )

animation_paths <-
  list.files(
    ".",
    pattern = "animation-n",
    full.names = TRUE,
    recursive = TRUE
  )

models_paths <-
  list.files(
    "../data/results",
    pattern = "model",
    full.names = TRUE,
    recursive = TRUE
  )

first_last_rec_plot_path <-
  "../data/results/first-last-record-plot.pdf"

species_table_path <- "../data/results/species-table.xlsx"

cus_path <- "../data/results/CUs-list.xlsx"

# Get file names by removing the path
maps_names <-
  str_replace(maps_paths, "../data/results/", "")

animation_names <-
  str_replace(animation_paths, "./", "")

models_names <-
  str_replace(models_paths, "../data/results/", "")

first_last_rec_plot_name <-
  str_replace(first_last_rec_plot_path, "../data/results/", "")

species_table_name <-
  str_replace(species_table_path, "../data/results/", "")

cus_name <-
  str_replace(cus_path, "../data/results/", "")

# Upload files -----------------------------------------------

# Upload all files to its respective folders
for (i in 1:length(maps_paths)) {
  drive_upload(
    maps_paths[i],
    name = maps_names[i],
    path = "~/ccma-tcc/results/ccma-knowledge-hotspots",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(animation_paths)) {
  drive_upload(
    animation_paths[i],
    name = animation_names[i],
    path = "~/ccma-tcc/results/ccma-knowledge-hotspots",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(models_paths)) {
  drive_upload(
    models_paths[i],
    name = models_names[i],
    path = "~/ccma-tcc/results/ccma-knowledge-hotspots/model",
    overwrite = TRUE,
    verbose = TRUE
  )
}

drive_upload(
  first_last_rec_plot_path,
  name = first_last_rec_plot_name,
  path = "~/ccma-tcc/results/species-knowledge-gaps",
  overwrite = TRUE,
  verbose = TRUE
)

drive_upload(
  species_table_path,
  name = species_table_name,
  path = "~/ccma-tcc/results/species-knowledge-gaps",
  overwrite = TRUE,
  verbose = TRUE
)

drive_upload(
  cus_path,
  name = cus_name,
  path = "~/ccma-tcc/results",
  overwrite = TRUE,
  verbose = TRUE
)


drive_upload(
  cus_path,
  name = cus_name,
  path = "~/ccma-tcc/results",
  overwrite = TRUE,
  verbose = TRUE
)
