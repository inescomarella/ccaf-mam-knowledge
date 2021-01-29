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

# Prepare folder -----------------------------------

# Check if folders already exist and create them
if (nrow(drive_get(path = "~/ccma-tcc/results/completeness")) == 0) {
  drive_mkdir("~/ccma-tcc/results/completeness")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots/model")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots/model")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/species-knowledge-gaps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/species-knowledge-gaps")
}

# Get files path to upload ------------------------------------
completeness_animation_path <- "./spatial_temporal_completeness_map_animation.gif"

completeness_paths <-
  list.files(
    "../data/results",
    pattern = "completeness",
    full.names = TRUE,
    recursive = TRUE
  )

nrec_paths <-
  list.files(
    "../data/results",
    pattern = "nrec_",
    full.names = TRUE,
    recursive = TRUE
  )

environmental_path <-
  list.files(
    "../data/results",
    pattern = "environmental_",
    full.names = TRUE,
    recursive = TRUE
  )

ks_stat_path <-
  list.files(
    "../data/results",
    pattern = "ks_stat",
    full.names = TRUE,
    recursive = TRUE
  )

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

scatter_path <- "../data/results/scatter-graph.pdf"

# Get file names by removing the path
completeness_animation_name <-
  str_replace(completeness_animation_path, "./", "")

completeness_names <-
  str_replace(completeness_paths, "../data/results/", "")

nrec_names <-
  str_replace(nrec_paths, "../data/results/", "")

environmental_name <-
  str_replace(environmental_path, "../data/results/", "")

ks_stat_name <-
  str_replace(ks_stat_path, "../data/results/", "")


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

scatter_name <-
  str_replace(cus_path, "../data/results/", "")

# Upload files -----------------------------------------------

# Upload all files to its respective folders
drive_upload(
  completeness_animation_path,
  name = completeness_animation_name,
  path = "~/ccma-tcc/results/completeness",
  overwrite = TRUE,
  verbose = TRUE
)

for (i in 1:length(completeness_names)) {
  drive_upload(
    completeness_paths[i],
    name = completeness_names[i],
    path = "~/ccma-tcc/results/completeness",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(nrec_names)) {
  drive_upload(
    nrec_paths[i],
    name = nrec_names[i],
    path = "~/ccma-tcc/results/completeness",
    overwrite = TRUE,
    verbose = TRUE
  )
}

drive_upload(
  environmental_path,
  name = environmental_name,
  path = "~/ccma-tcc/results/completeness",
  overwrite = TRUE,
  verbose = TRUE
)

drive_upload(
  ks_stat_path,
  name = ks_stat_name,
  path = "~/ccma-tcc/results/completeness",
  overwrite = TRUE,
  verbose = TRUE
)

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

drive_upload(
  scatter_path,
  name = scatter_name,
  path = "~/ccma-tcc/results/species-knowledge-gaps",
  overwrite = TRUE,
  verbose = TRUE
)
