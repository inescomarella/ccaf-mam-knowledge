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

# Get files path to upload ------------------------------------
completeness_animation_path <- "./spatial_temporal_completeness_map_animation.gif"

results_paths <-
  list.files(
    "../data/results",
    full.names = TRUE,
    recursive = TRUE
  )

# Get file names by removing the path
completeness_animation_names <-
  str_replace(completeness_animation_path, "./", "")

results_names <-
  str_replace(results_paths, "../data/results/", "")

# Upload files -----------------------------------------------

# Upload all files to its respective folders
drive_upload(
  completeness_animation_path,
  name = completeness_animation_names,
  path = "~/TCC-Bach/results",
  overwrite = TRUE,
  verbose = TRUE
)

for (i in 1:length(results_paths)) {
  drive_upload(
    results_paths[i],
    name = results_names[i],
    path = "~/TCC-Bach/results",
    overwrite = TRUE,
    verbose = TRUE
  )
}
