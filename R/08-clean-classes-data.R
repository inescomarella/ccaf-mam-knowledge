setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <-
  c("tidyverse", "plyr", 'dplyr')
lapply(x, library, character.only = TRUE)

source('functions.R')

data_paper <- read.csv('../data/data-papers.csv')
gbif_mamm <- read.csv('../data/gbif-mamm-clipped.csv')
gbif_amph <- read.csv('../data/gbif-amph-clipped.csv')
gbif_rept <- read.csv('../data/gbif-rept-clipped.csv')
gbif_inse <- read.csv('../data/gbif-inse-clipped.csv')
gbif_arac <- read.csv('../data/gbif-arac-clipped.csv')
gbif_aves <- read.csv('../data/aves-clean.csv')
spLink_animals <- read.csv('../data/spLink-animals-clipped.csv')

# Standardizing columns
colnames(gbif_aves) <- colnames(gbif_amph)
gbif_mamm$scientificName <- gbif_mamm$species
gbif_aves$scientificName <- gbif_aves$species
gbif_amph$scientificName <- gbif_amph$species
gbif_rept$scientificName <- gbif_rept$species
gbif_inse$scientificName <- gbif_inse$species
gbif_arac$scientificName <- gbif_arac$species

# Select classes in speciesLink data
spLink_mamm <- spLink_animals %>% filter(class == 'Mammalia')
spLink_aves <- spLink_animals %>% filter(class == 'Aves')
spLink_amph <- spLink_animals %>% filter(class == 'Amphibia')
spLink_rept <- spLink_animals %>% filter(class == 'Reptilia')
spLink_inse <- spLink_animals %>% filter(class == 'Insecta')
spLink_arac <- spLink_animals %>% filter(class == 'Arachnida')

# Binding data.frames to a single obj
mamm_data <- rbind.fill(gbif_mamm, spLink_mamm)
aves_data <- rbind.fill(gbif_aves, spLink_aves)
amph_data <- rbind.fill(gbif_amph, spLink_amph)
rept_data <- rbind.fill(gbif_rept, spLink_rept)
inse_data <- rbind.fill(gbif_inse, spLink_inse)
arac_data <- rbind.fill(gbif_arac, spLink_arac)

# Keep only identified species
mamm_data_clean <- only.indetified.sp(mamm_data)
aves_data_clean <- only.indetified.sp(aves_data)
amph_data_clean <- only.indetified.sp(amph_data)
rept_data_clean <- only.indetified.sp(rept_data)
inse_data_clean <- only.indetified.sp(inse_data)
arac_data_clean <- only.indetified.sp(arac_data)

# Output
write.csv(mamm_data_clean, '../data/mamm-data.csv')
write.csv(aves_data_clean, '../data/aves-data.csv')
write.csv(amph_data_clean, '../data/amph-data.csv')
write.csv(rept_data_clean, '../data/rept-data.csv')
write.csv(inse_data_clean, '../data/inse-data.csv')
write.csv(arac_data_clean, '../data/arac-data.csv')
