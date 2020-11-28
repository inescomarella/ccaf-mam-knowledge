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
if (nrow(drive_get(path = "~/ccma-tcc/results/correlation")) == 0) {
  drive_mkdir("~/ccma-tcc/results/correlation")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/model")) == 0) {
  drive_mkdir("~/ccma-tcc/results/model")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/order-maps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/order-maps")
}

if (nrow(drive_get(path = "~/ccma-tcc/results/all-mammals-maps")) == 0) {
  drive_mkdir("~/ccma-tcc/results/all-mammals-maps")
}

# Get files path to upload
species_table_path <- "../data/results/species-table.xlsx"

records_plot_path <- "../data/results/first-last-record-plot.pdf"

file_paths_correlation <-
  list.files(
    "../data/results",
    pattern = "correlation",
    full.names = TRUE,
    recursive = TRUE
  )

file_paths_model_plots <-
  list.files(
    "../data/results",
    pattern = "model",
    full.names = TRUE,
    recursive = TRUE
  )

file_paths_orders <-
  list.files(
    "../data/results",
    pattern = "order",
    full.names = TRUE,
    recursive = TRUE
  )

file_paths_all_mammals <-
  list.files(
    "../data/results",
    pattern = "all",
    full.names = TRUE,
    recursive = TRUE
  )

# Get file names by removing the path
species_table_name <- str_replace(species_table_path, "../data/results/", "")

records_plot_name <- str_replace(records_plot_path, "../data/results/", "")

file_names_correlation <-
  str_replace(file_paths_correlation, "../data/results/", "")

file_names_model_plots <-
  str_replace(file_paths_model_plots, "../data/results/", "")

file_names_orders <-
  str_replace(file_paths_orders, "../data/results/", "")

file_names_all_mammals <-
  str_replace(file_paths_all_mammals, "../data/results/", "")

# Upload files ---------------------------------------------------------------

# Upload all files to its respective folders
drive_upload(
  species_table_path,
  name = species_table_name,
  path = "~/ccma-tcc/results",
  overwrite = TRUE,
  verbose = TRUE
)

drive_upload(
  records_plot_path,
  name = records_plot_name,
  path = "~/ccma-tcc/results",
  overwrite = TRUE,
  verbose = TRUE
)

for (i in 1:length(file_paths_correlation)) {
  drive_upload(
    file_paths_correlation[i],
    name = file_names_correlation[i],
    path = "~/ccma-tcc/results/correlation",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(file_paths_model_plots)) {
  drive_upload(
    file_paths_model_plots[i],
    name = file_names_model_plots[i],
    path = "~/ccma-tcc/results/model",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(file_paths_all_mammals)) {
  drive_upload(
    file_paths_all_mammals[i],
    name = file_names_all_mammals[i],
    path = "~/ccma-tcc/results/all-mammals-maps",
    overwrite = TRUE,
    verbose = TRUE
  )
}

for (i in 1:length(file_paths_orders)) {
  drive_upload(
    file_paths_orders[i],
    name = file_names_orders[i],
    path = "~/ccma-tcc/results/order-maps",
    overwrite = TRUE,
    verbose = TRUE
  )
}
