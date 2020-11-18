# File purpose: Clean and standardize mammal data from paper, GBIF and
# speciesLink
# Pay attention: The rredlist::rl_synonyms() function always get some error and
# take quite some time to run
#
# Date: 16/11/2020

# Load in libraries
x <- c("tidyverse", "rgbif", "plyr", "rredlist", "sf", "conflicted")
lapply(x, library, character.only = TRUE)

conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/02-funs-clean-all-data.R")

# Load in data
data_paper <-
  read.csv("../data/processed-data/clean-papers-data.csv")
data_splink <-
  read.csv("../data/processed-data/mammal-spLink-data.csv")
data_gbif <-
  read.csv("../data/processed-data/mammal-gbif-data.csv")
ccma <- st_read(dsn = '../data/processed-data',
                layer = 'ccma-clipped',
                check_ring_dir = TRUE)
rlkey <-
  "6abf9b5a0010ab26140c401c1c394a22c43c0a555d9dee8c72976d3c71c5e402"

# Standardize columns
data_gbif$scientificName <- data_gbif$species
colnames(data_paper)[16] <- "year"

# Binding data.frames
data_all_raw <- rbind.fill(data_paper, data_splink, data_gbif)

# Keep data_paper columns, remove others
data_all <- select(data_all_raw, colnames(data_paper))

# Keep only identified species
data_all$scientificName <- as.character(data_all$scientificName)
to_remove_scientificName <-
  data_all %>%
  filter(
    is.na(scientificName) |
      str_detect(scientificName, " ") == FALSE |
      str_detect(scientificName, " sp"),
    !str_detect(scientificName, "spinosus")
  )

to_remove_acceptedNameUsage <-
  data_all %>% filter(
    is.na(acceptedNameUsage) |
      str_detect(acceptedNameUsage, " ") == FALSE |
      str_detect(acceptedNameUsage, " sp"),
    !str_detect(acceptedNameUsage, "spinosus")
  )
to_remove <-
  intersect(to_remove_acceptedNameUsage, to_remove_scientificName)

data_all <- anti_join(data_all, to_remove)

# Correct species name in acceptedNameUsage instead of scientificName
sp_in_acceptedNameUsage <-
  anti_join(to_remove_scientificName, to_remove_acceptedNameUsage)
data_all <- anti_join(data_all, sp_in_acceptedNameUsage)

sp_in_acceptedNameUsage$scientificName <-
  sp_in_acceptedNameUsage$acceptedNameUsage

data_all <- bind_rows(data_all, sp_in_acceptedNameUsage)

# Remove records outside CCMA limits
# This might take a while
data_all_clipped <- clip.ccma(data_all)

# Removing dots (to identify and remove special characters later)
data_all_clipped$scientificName <-
  str_replace_all(data_all_clipped$scientificName, "[.]", " ")

data_all_clipped$scientificName <-
  str_replace(data_all_clipped$scientificName,
              pattern = " gr",
              replacement = " ")
data_all_clipped$scientificName <-
  str_replace(data_all_clipped$scientificName,
              pattern = "cf ",
              replacement = " ")
data_all_clipped$scientificName <-
  str_replace(data_all_clipped$scientificName,
              pattern = "Cf ",
              replacement = "")
data_all_clipped$scientificName <-
  str_replace(data_all_clipped$scientificName,
              pattern = " j ",
              replacement = " ")
data_all_clipped$scientificName <-
  str_replace(data_all_clipped$scientificName,
              pattern = "   ",
              replacement = " ")

to_remove <-
  data_all_clipped %>%
  filter(str_detect(scientificName, "[?]"))
data_all_clipped <- anti_join(data_all_clipped, to_remove)

to_remove <-
  data_all_clipped %>%
  filter(str_detect(scientificName, "[/]"))
data_all_clipped <- anti_join(data_all_clipped, to_remove)

to_remove <-
  data_all_clipped %>%
  filter(str_detect(scientificName, "brido"))
data_all_clipped <- anti_join(data_all_clipped, to_remove)

# Species list
sp_list_all <- sort(unique(data_all_clipped$scientificName))

# Get synonyms ---------------------------------------------------------------
# This might take a while and it's normal to get "Error: Bad Gateway (HTTP
# 502)", although the inputs were subdivided to prevent this problem, but just
# try again until it runs
#
apply_synonyms_1 <- lapply(sp_list_all[1:100], rl.synonyms)
apply_synonyms_2 <- lapply(sp_list_all[101:200], rl.synonyms)
apply_synonyms_3 <- lapply(sp_list_all[201:300], rl.synonyms)
apply_synonyms_4 <-
  lapply(sp_list_all[301:length(sp_list_all)], rl.synonyms)

synonyms_df_1 <- ldply(apply_synonyms_1, data.frame)
synonyms_df_2 <- ldply(apply_synonyms_2, data.frame)
synonyms_df_3 <- ldply(apply_synonyms_3, data.frame)
synonyms_df_4 <- ldply(apply_synonyms_4, data.frame)

synonyms_df_corrected <-
  rbind.data.frame(synonyms_df_1, synonyms_df_2, synonyms_df_3, synonyms_df_4)

# Correcting accepted_name
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Anoura geoffroyi"] <-
  "Anoura geoffroyi"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Lagothrix lagotricha"] <-
  "Lagothrix lagotricha"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Mimon crenulatum"] <-
  "Mimon crenulatum"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Natalus macrourus"] <-
  "Natalus macrourus"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Natalus stramineus"] <-
  "Natalus macrourus"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Nectomys squamipes"] <-
  "Nectomys squamipes"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Saguinus bicolor"] <-
  "Saguinus martinsi"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Micoureus paraguayanus"] <-
  "Marmosa paraguayana"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Micoureus travassosi"] <-
  "Micoureus travassosi"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Anoura caudifer"] <-
  "Anoura caudifer"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Alouatta fusca"] <-
  "Alouatta guariba"
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == "Callicebus personatus"] <-
  "Callicebus personatus"

# Removing Cebus spp. to identify separately based on the record site
synonyms_df_corrected <-
  synonyms_df_corrected %>%
  filter(!str_detect(scientificName, "Cebus"))

# Separate the reviewed and not-reviewed synonym
iucn_synonyms_df <-
  synonyms_df_corrected %>%
  filter(!is.na(accepted_name))
no_iucn_synonyms_df <-
  synonyms_df_corrected %>%
  filter(is.na(accepted_name))

# Taxonomy backbone reviewed synonym ----------------------------------------
# Get species backbone and keep a scientificName column as a key to merge
# This might take a while
apply_backbone_iucn <-
  lapply(iucn_synonyms_df$accepted_name, name.backbone)
backbone_iucn_df <- ldply(apply_backbone_iucn, data.frame)

backbone_iucn_df_selected <-
  backbone_iucn_df %>%
  select(order, family, canonicalName, scientificName)
colnames(backbone_iucn_df_selected) <-
  c("order", "family", "species", "scientificName")

# Taxonomy backbone not reviewed synonym ------------------------------------
# Get species backbone and keep a scientificName column as a key to merge
# This might take a while
apply_backbone_gbif <-
  lapply(no_iucn_synonyms_df$scientificName, name.backbone)
backbone_gbif_df <- ldply(apply_backbone_gbif, data.frame)

# Correcting species
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Puma yagouaroundi"] <-
  "Herpailurus yagouaroundi"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Puma yaguarondi"] <-
  "Herpailurus yagouaroundi"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Lutra brasiliensis"] <-
  "Pteronura brasiliensis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Mycetes ursinus"] <-
  "Alouatta guariba"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Mazama simplicicornis"] <-
  "Mazama gouazoubira"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Dicotyles labiatus"] <-
  "Tayassu pecari"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Ateles hypoxanthus"] <-
  "Brachyteles hypoxanthus"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Dasyprocta agouti"] <-
  "Dasyprocta aguti"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Cavia aguti"] <-
  "Cuniculus paca"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Cavia agouti"] <-
  "Cuniculus paca"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Oryzomys trinitatis"] <-
  "Oecomys trinitatis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Callithrix leucocephalus"] <-
  "Callithrix geoffroyi"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Lagothrix infumata"] <-
  "Lagothrix lagotricha"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Synetheres prehensilis"] <-
  "Coendou prehensilis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Trinomys atiosus"] <-
  "Trinomys gratiosus"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Guerlinguetus ingrami"] <-
  "Guerlinguetus ingrami"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Rhipidomys tribei"] <-
  "Rhipidomys tribei"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Philander quica"] <-
  "Philander quica"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Metachirus myosurus"] <-
  "Metachirus myosurus"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Lichonycteris degener"] <-
  "Lichonycteris degener"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Epitesicus furinalis"] <-
  "Eptesicus furinalis"
backbone_gbif_df$order[backbone_gbif_df$scientificName == "Epitesicus furinalis"] <-
  "Chiroptera"
backbone_gbif_df$family[backbone_gbif_df$scientificName == "Epitesicus furinalis"] <-
  "Vespertilionidae"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Blarynomis breviceps"] <-
  "Blarinomys breviceps"
backbone_gbif_df$order[backbone_gbif_df$scientificName == "Blarynomis breviceps"] <-
  "Rodentia"
backbone_gbif_df$family[backbone_gbif_df$scientificName == "Blarynomis breviceps"] <-
  "Cricetidae"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Leopardus tigrinus"] <-
  "Leopardus guttulus"
#Nascimento, 2010. Revisão taxonomica do gênero Leopardus. Tese doutorado, USP.
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Felis brasiliensis"] <-
  "Leopardus pardalis"

backbone_gbif_df_selected <-
  backbone_gbif_df %>%
  select(order, family, species, scientificName)

# Taxonomy backbone Sapajus spp. --------------------------------------------
S_nigritus <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    str_detect(stateProvince, "Santo"),
    decimalLatitude < -19.5
  )
S_robustus_es <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    str_detect(stateProvince, "Santo"),
    decimalLatitude > -19.5
  )
S_robustus_ba <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    str_detect(stateProvince, "Bahia"),
    decimalLatitude < -15.8
  )
S_xanthosternos <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    str_detect(stateProvince, "Bahia"),
    decimalLatitude > -15.8
  )

S_nigritus$acceptedNameUsage <- "Sapajus nigritus"
S_robustus_es$acceptedNameUsage <- "Sapajus robustus"
S_robustus_ba$acceptedNameUsage <- "Sapajus robustus"
S_xanthosternos$acceptedNameUsage <- "Sapajus xanthosternos"

sapajus_df <-
  bind_rows(S_nigritus, S_robustus_es, S_robustus_ba, S_xanthosternos)

# Get species backbone and keep a scientificName column as a key to merge
# This might take a while
apply_backbone_sapajus <-
  lapply(sapajus_df$acceptedNameUsage, name.backbone)
backbone_sapajus_df <- ldply(apply_backbone_sapajus, data.frame)

# Removing Sapajus records to add separately
# Identification not based on scietificName, so I can't use merge
data_all_without_sapajus <-
  data_all_clipped %>%
  filter(!str_detect(scientificName, "Cebus"))

# Add Sapajus backbone to sapajus data.frame
backbone_sapajus_df_selected <-
  backbone_sapajus_df %>%
  select(order, family, species)
sapajus_df_final <-
  bind_cols(sapajus_df, backbone_sapajus_df_selected)

sapajus_df_final <-
  sapajus_df_final %>%
  select(-c(order...28, family...29))
colnames(sapajus_df_final)[35:36] <- c("order", "family")

sapajus_df_final_ordered <-
  sapajus_df_final %>%
  select(colnames(data_all_clipped), species)

# Merge GBIF & IUCN backbones to data.frame without Sapajus -----------------
backbone_sp_gbif_iucn_df <-
  bind_rows(backbone_iucn_df_selected, backbone_gbif_df_selected)

backbone_sp_gbif_iucn_df <-
  as.data.frame(unique(backbone_sp_gbif_iucn_df))

data_all_backbone_iucn_gbif_merged <-
  merge(data_all_without_sapajus, backbone_sp_gbif_iucn_df, by = "scientificName")

data_all_backbone_iucn_gbif_merged <-
  data_all_backbone_iucn_gbif_merged %>%
  select(-c(order.x, family.x))

colnames(data_all_backbone_iucn_gbif_merged)[35:36] <-
  c("order", "family")

# Reorder
data_iucn_gbif_ordered <-
  data_all_backbone_iucn_gbif_merged %>%
  select(colnames(data_all), species)

# Bind all data.frames ---------------------------------------------------------
data_all_united <-
  bind_rows(data_iucn_gbif_ordered, sapajus_df_final_ordered)

# column "species" = current name
data_all_united <-
  data_all_united %>%
  select(-acceptedNameUsage)

# Remove non-native species --------------------------------------------------
exotic_sp_list <- data.frame(
  species = c(
    "Canis lupus",
    "Rattus rattus",
    "Mus musculus",
    "Felis catus",
    "Alouatta caraya",
    "Artibeus jamaicensis",
    "Rattus norvegicus",
    "Eumops patagonicus",
    "Conepatus chinga",
    "Histiotus montanus",
    "Lonchophylla mordax",
    "Lophostoma silvicolum",
    "Molossops neglectus",
    "Myotis dinellii",
    "Myotis izecksohni",
    "Myotis lavali",
    "Platyrrhinus incarum",
    "Dasypus hybridus",
    "Leopardus braccatus",
    "Molossops temminckii",
    "Noctilio albiventris",
    "Canis griseoargenteus",
    "Lepus europaeus",
    "Pseudalopex vetulus",
    "Ziphius cavirostris",
    "Mazama nana",
    "Cebus apella",
    "Sapajus apella",
    "Cebus flavius",
    "Cebus libidinosus",
    "Sapajus libidinosus",
    "Sapajus flavius",
    "Saguinus bicolor",
    "Lagothrix lagotricha",
    "Saguinus martinsi",
    "Chiropotes utahickae",
    "Chiropotes satanas",
    "Coendou",
    "Micoureus",
    "Lagothrix lagothricha",
    "Bradypus tridactylus",
    "Callithrix aurita",
    "Brachyteles arachnoides",
    # (retirado de Reis et al., 2017) "De acordo com Nogueira et al. (2014a), os registros de Carollia castanea H. Allen, 1890 indicados para o Brasil eram equivocados." Não dá para saber se é C. brevicauda ou C. perspicillata
    "Carollia castanea",
    # (retirado de Reis et al., 2017) "O gênero Platyrrhinus atualmente é composto por vinte espécies (VELAZCO et al., 2010) e, segundo Nogueira et al. (2014a), apenas oito ocorrem em território brasileiro." P. helleri não está incluindo nestas 8 spp
    "Platyrrhinus helleri"
  )
)

# Removing CCMA non-native species
data_all_sp_clean <-
  data_all_united %>%
  filter(!species %in% exotic_sp_list$species)

# Removing marine mammals
to_remove <-
  data_all_sp_clean %>%
  filter(str_detect(order, "Cetacea"))
data_all_sp_clean <- anti_join(data_all_sp_clean, to_remove)

# Correct some species names ------------------------------------------
data_all_sp_clean$species[data_all_sp_clean$species == "Puma yagouaroundi"] <-
  "Herpailurus yagouaroundi"

#Adotando nomenclatura de Reis et al. 2017
data_all_sp_clean$species[data_all_sp_clean$species == "Gardnerycteris crenulatum"] <-
  "Mimon crenulatum"

#(retirado de Reis et al. 2017) "Estudos genéticos de Baker et al. (1998) e de Morales e Bickham (1995) indicam que L. borealis limita-se ao centro-oeste dos EUA e Canadá, e nordeste do México. Todas as outras populações, com exceção das Antilhas (que podem representar uma outra espécie), estariam incluídas em L. blossevillii (REID, 1997)."
data_all_sp_clean$species[data_all_sp_clean$species == "Lasiurus borealis"] <-
  "Lasiurus blossevillii"

data_all_sp_clean$species[data_all_sp_clean$species == "Natalus stramineus"] <-
  "Natalus macrourus"

data_all_sp_clean$species[data_all_sp_clean$species == "Oxymycterus caparoae"] <-
  "Oxymycterus caparaoe"

data_all_sp_clean$species[data_all_sp_clean$scientificName == "Brucepattersonius iserufescens"] <-
  "Brucepattersonius griserufescens"

data_all_sp_clean$species[data_all_sp_clean$scientificName == "Guerlinguetus brasiliensi"] <-
  "Guerlinguetus ingrami"

# scientificName as the scientific name containing the species author, not as
# the key to the raw data reference anymore
# Species taxonomy
species_df <-
  data.frame(species = sort(unique(data_all_sp_clean$species)))
species_backbone_df <-
  bind_rows(apply(X = species_df, MARGIN = 1, FUN = name_backbone))

species_df$scientificName <- species_backbone_df$scientificName

data_all_sp_clean <-
  data_all_sp_clean %>%
  select(-scientificName)

clean_data <- merge(data_all_sp_clean, species_df, by = "species")

# Select columns of interest
clean_data_slct <-
  clean_data %>%
  select(
    datasetName,
    institutionCode,
    collectionCode,
    catalogNumber,
    reference_std,
    citation,
    PublicationYear,
    year,
    country,
    stateProvince,
    county,
    locality,
    decimalLongitude,
    decimalLatitude,
    order,
    family,
    species,
    scientificName
  )

colnames(clean_data_slct)[5] <- "reference"

# Export data.frame ------------------------------------------------------
write.csv(clean_data_slct,
          "../data/processed-data/clean-mammal-data.csv")
