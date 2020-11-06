setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c("tidyverse", "rocc", "sf")
lapply(x, library, character.only = TRUE)

source('functions.R')

# Get occurence data from speciesLink
data_downloaded <- rspeciesLink(
  dir = '../data/',
  filename = 'spLink-mamm-raw',
  stateProvince = c('Espirito Santo', 'Espírito Santo', 'ES', 'Bahia', 'BA'),
  Coordinates = 'Yes',
  Scope = 'animals',
  Synonyms = 'species2000'
)

# Just mammal data
data_mammal <- data_downloaded %>% filter(class == 'Mammalia')

# Remove point outside CCMA
data_mammal_clipped <- clip.ccma(data_downloaded)

# Output
write.csv(data_mammal_clipped, '../data/spLink-mamm-clipped.csv')
