# File purpose: Upload results to Google Drive
# Date: 27/11/2020

# Load library
library(googledrive)
library(stringr)

# Login ---------------------------------------------------------------------

# Authorize googledrive to view and manage my Drive files
drive_auth(
  email = "inesmottacomarella@gmail.com",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

# Prepare folder and files ---------------------------------------------------

# Check if folders already exist and create them
if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/ccma-knowledge-hotspots/order-maps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/ccma-knowledge-hotspots/order-maps")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/species-knowledge-gaps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/species-knowledge-gaps")
}


# Get files path to upload
all_mammals_paths <-
  list.files(
    "../data/results",
    pattern = "all",
    full.names = TRUE,
    recursive = TRUE
  )

orders_plot_paths <-
  list.files(
    "../data/results",
    pattern = "order",
    full.names = TRUE,
    recursive = TRUE
  )

first_last_rec_plot_path <-
  "../data/results/first-last-record-plot.pdf"

species_table_path <- "../data/results/species-table.xlsx"


# Get file names by removing the path
all_mammals_names <-
  str_replace(all_mammals_paths, "../data/results/", "")

orders_plot_names <-
  str_replace(orders_plot_paths, "../data/results/", "")

first_last_rec_plot_name <-
  str_replace(first_last_rec_plot_path, "../data/results/", "")

species_table_name <-
  str_replace(species_table_path, "../data/results/", "")

# Upload files ---------------------------------------------------------------

# Upload all files to its respective folders
for (i in 1:length(all_mammals_paths)) {
  drive_upload(
    all_mammals_paths[i],
    name = all_mammals_names[i],
    path = "~/ccma-tcc/results/ccma-knowledge-hotspots",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(orders_plot_names)) {
  drive_upload(
    orders_plot_paths[i],
    name = orders_plot_names[i],
    path = "~/ccma-tcc/results/ccma-knowledge-hotspots/order-maps",
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
