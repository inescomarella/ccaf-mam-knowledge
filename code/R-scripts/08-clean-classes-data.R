# File purpose: Clean classes record from GBIF and speciesLink
# Remember: Aves data from GBIF are cleaned in another script, here they will
# just be attached to Aves data from speciesLink
# Date: 20/11/2020

# Load in libraries
x <-
  c("dplyr", "sp", "sf", "plyr", "conflicted")
lapply(x, library, character.only = TRUE)

conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")

# Source functions
source("./R-scripts/functions/05-funs-clean-classes-data.R")

# Load in data
gbif_mamm <- read.csv("../data/processed-data/mammal-gbif-data.csv")
gbif_amph <- read.csv("../data/processed-data/raw-gbif-amph.csv")
gbif_rept <- read.csv("../data/processed-data/raw-gbif-rept.csv")
gbif_inse <- read.csv("../data/processed-data/raw-gbif-inse.csv")
gbif_arac <- read.csv("../data/processed-data/raw-gbif-arac.csv")
gbif_aves <- read.csv("../data/processed-data/gbif-aves-clipped.csv")
spLk_animals <-
  read.csv("../data/processed-data/raw-spLink-animals.csv")
ccma <- st_read(dsn = "../data/processed-data",
                layer = "ccma-clipped",
                check_ring_dir = TRUE)
ccma <- st_transform(ccma, crs = CRS("+proj=longlat +datum=WGS84"))

# Prepare datasets -----------------------------------------------------------

# Save GBIF raw data in a single object
gbif_list <- list(gbif_mamm,
                  gbif_amph,
                  gbif_rept,
                  gbif_inse,
                  gbif_arac)

# Saving memory
rm(gbif_mamm,
   gbif_amph,
   gbif_rept,
   gbif_inse,
   gbif_arac)

# Remove some columns to make objects easier to handle
gbif_list <- lapply(gbif_list, select.gbif.columns)
spLk_animals$species <- spLk_animals$scientificName
spLk_animals <-
  spLk_animals %>%
  select(
    class,
    order,
    family,
    species,
    scientificName,
    locality,
    stateProvince,
    decimalLatitude,
    decimalLongitude,
    year,
    basisOfRecord,
    institutionCode,
    collectionCode,
    catalogNumber,
    recordNumber,
    recordedBy
  )

# Clean datasets --------------------------------------------------------------

# Remove fossil record and iNaturalist records
gbif_list <- lapply(gbif_list, remove.fossil.iNaturalist)

# Keep only identified species
gbif_list <- lapply(gbif_list, only.indetified.sp)

to_remove <-
  spLk_animals %>%
  filter(species == "" | is.na(species))

spLk_animals <- anti_join(spLk_animals, to_remove)
rm(to_remove)

# Remove point outside CCMA
# Takes to 8min run
gbif_list <- lapply(gbif_list, clip.ccma)
# Takes to 35min run
spLk_animals <- clip.ccma(spLk_animals)

# Bind datasetss ------------------------------------------------------------

# Select classes in speciesLink data
spLk_mamm <-
  spLk_animals %>%
  filter(class == "Mammalia")

spLk_amph <-
  spLk_animals %>%
  filter(class == "Amphibia")

spLk_rept <-
  spLk_animals %>%
  filter(class == "Reptilia")

spLk_inse <-
  spLk_animals %>%
  filter(class == "Insecta")

spLk_arac <-
  spLk_animals %>%
  filter(class == "Arachnida")

spLk_aves <-
  spLk_animals %>%
  filter(class == "Aves")

# Binding datasets
mamm_data <- bind_rows(gbif_list[[1]], spLk_mamm)
amph_data <- bind_rows(gbif_list[[2]], spLk_amph)
rept_data <- bind_rows(gbif_list[[3]], spLk_rept)
inse_data <- bind_rows(gbif_list[[4]], spLk_inse)
arac_data <- bind_rows(gbif_list[[5]], spLk_arac)

gbif_aves <-
  gbif_aves %>%
  mutate(class = as.character(class))

spLk_aves <-
  spLk_aves %>%
  mutate(class = as.character(class))

aves_data <- bind_rows(gbif_aves, spLk_aves)

# Save data -----------------------------------------------------------------
write.csv(mamm_data, "../data/processed-data/clean-mamm-data-gbif-spLk.csv")
write.csv(aves_data, "../data/processed-data/clean-aves-data.csv")
write.csv(amph_data, "../data/processed-data/clean-amph-data.csv")
write.csv(rept_data, "../data/processed-data/clean-rept-data.csv")
write.csv(inse_data, "../data/processed-data/clean-inse-data.csv")
write.csv(arac_data, "../data/processed-data/clean-arac-data.csv")

# Remove raw files
file.remove("../data/processed-data/mammal-gbif-data.csv")
file.remove("../data/processed-data/raw-gbif-amph.csv")
file.remove("../data/processed-data/raw-gbif-rept.csv")
file.remove("../data/processed-data/raw-gbif-inse.csv")
file.remove("../data/processed-data/raw-gbif-arac.csv")
file.remove("../data/processed-data/gbif-aves-clipped.csv")
file.remove("../data/processed-data/raw-spLink-animals.csv")

