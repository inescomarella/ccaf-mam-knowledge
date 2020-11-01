x <- c("tidyverse", "rocc")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Get occurence data from speciesLink
data_downloaded <- rspeciesLink(filename = 'data-spLink',
             stateProvince = c('Espirito Santo', 'Espírito Santo', 'ES', 'Bahia', 'BA'),
             Scope = 'animals',
             Synonyms = 'species2000')

# Just mammal data
data_mammal_raw <- data_downloaded %>% filter(class == 'Mammalia')

# Remove data without geographic coordinates
data_mammal <- data_mammal_raw %>% filter(!is.na(decimalLatitude))

# Output
write.csv(data_mammal, './data-spLink.csv')
