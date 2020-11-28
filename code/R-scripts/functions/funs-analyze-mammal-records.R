# File purpose: Functions to do some tables
# Date: 27/11/2020

library(dplyr)

do.reference.table <- function(df) {
  # Make a table with all collection and institutions sources of records for
  # each species, paste the references in a single cell
  #
  # Args:
  #   df: dataframe with "species", "collectionCode", "catalogNumber" and
  #   "citation" columns

  reference_table_df <- data.frame(species = unique(df$species))
  for (i in 1:length(unique(df$species))) {
    
    # Get the species rows 
    species_rows <-
      data %>%
      filter(species == unique(data$species)[i]) %>%
      mutate(collection = paste(collectionCode, catalogNumber)) %>%
      select(citation, collection)
    
    # Get the unique reference for each species
    species_citation <- unique(species_rows$citation)
    species_collection <- unique(species_rows$collection)

    citat <- c()
    colle <- c()
    
    # Paste every citation in a cell (?)
    for (j in 1:length(species_citation)) {
      if (is.na(species_citation[j]) == FALSE &&
        species_citation[j] != "" && species_citation[j] != " ") {
        citat <- paste(citat, species_citation[j], sep = ", ")
      }
    }
    
    # Do the same for collections
    for (k in 1:length(species_collection)) {
      if (is.na(species_collection[k]) == FALSE &&
        species_collection[k] != "" &&
        species_collection[k] != " ") {
        colle <- paste(colle, species_collection[k], sep = ", ")
      }
    }
    
    # Paste citations and collections in a same cell for each species (?)
    reference_table_df$reference[i] <-
      paste(citat, colle, sep = ", ")
  }
  
  # Correct commas in cases of missing values
  reference_table_df %>%
    mutate(reference = sub(", ,", ",", reference)) %>%
    mutate(reference = sub(" ,", ",", reference)) %>%
    mutate(reference = substring(reference, 3))
}

do.collection.institution.table <- function(df) {
  # Make a list of the institutions and collections
  #
  #   Args:
  #     df: dataframe with collectionCode and collectionCode columns

  df %>%
    select(collectionCode, collectionCode) %>%
    unique() %>%
    mutate(
      collectionCode = as.character(collectionCode),
      institutionCode = as.character(institutionCode)
    ) %>%
    filter(
      str_detect(collectionCode, "[:alpha:]") |
        str_detect(institutionCode, "[:alpha:]")
    )
}
