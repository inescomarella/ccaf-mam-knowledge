# File purpose: Clean and standardize mammal data from paper, GBIF and
# speciesLink
# Pay attention: The rredlist::rl_synonyms() function always gets some error
# and takes hours to run
#
# Date: 16/11/2020

# Load in libraries
xfun::pkg_attach(c(
  "tidyverse",
  "rredlist",
  "rgbif",
  "plyr",
  "sf"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "mutate", winner = "dplyr")

# Source functions
source("./R-scripts/functions/03-funs-clean-all-data.R")

# Load in data
data_paper <-
  read.csv("../data/processed-data/clean-papers-data.csv")
data_downl <-
  read.csv("../data/processed-data/downloaded-data.csv")

rlkey <-
  "*******"

# Pre-process data -------------------------------------------

# Standardize columns
data_paper <- select(data_paper, -X)
colnames(data_paper)[15] <- "year"

# Binding data.frames
data_all_raw <- rbind.fill(data_paper, data_downl)

# Keep data_paper columns, remove others
data_all <- select(data_all_raw, colnames(data_paper))

# Keep only identified species and remove hybrids
data_all_only_indetified_species <- only.indentified.species(data_all)

# Remove records outside CCMA limits
# Takes 3min to run
data_all_clipped <- clip.ccma(data_all_only_indetified_species)

# Species list
sp_list_all <- sort(unique(data_all_clipped$scientificName))

# Get rl.synonyms --------------------------------------------
# Takes a few hours to run and it's normal to get "Error: Bad Gateway (HTTP
# 502)", although the inputs were subdivided to prevent this problem, but just
# try again until it runs
apply_synonyms_1 <- lapply(sp_list_all[1:55], rl.synonyms)
apply_synonyms_2 <- lapply(sp_list_all[56:110], rl.synonyms)
apply_synonyms_3 <- lapply(sp_list_all[111:165], rl.synonyms)
apply_synonyms_4 <- lapply(sp_list_all[166:220], rl.synonyms)
apply_synonyms_5 <- lapply(sp_list_all[221:275], rl.synonyms)
apply_synonyms_6 <- lapply(sp_list_all[276:330], rl.synonyms)
apply_synonyms_7 <- lapply(sp_list_all[331:length(sp_list_all)], rl.synonyms)

synonyms_df_1 <- ldply(apply_synonyms_1, data.frame)
synonyms_df_2 <- ldply(apply_synonyms_2, data.frame)
synonyms_df_3 <- ldply(apply_synonyms_3, data.frame)
synonyms_df_4 <- ldply(apply_synonyms_4, data.frame)
synonyms_df_5 <- ldply(apply_synonyms_5, data.frame)
synonyms_df_6 <- ldply(apply_synonyms_6, data.frame)
synonyms_df_7 <- ldply(apply_synonyms_7, data.frame)

synonyms_df_corrected <-
  rbind.data.frame(
    synonyms_df_1,
    synonyms_df_2,
    synonyms_df_3,
    synonyms_df_4,
    synonyms_df_5,
    synonyms_df_6,
    synonyms_df_7
  )

# Correct accepted_name before get species backbone
synonyms_df_corrected <-
  synonyms_df_corrected %>%
  mutate(accepted_name = ifelse(
    scientificName == "Anoura geoffroyi",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Lagothrix lagotricha",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Mimon crenulatum",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Natalus macrourus",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Natalus stramineus",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Nectomys squamipes",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Micoureus travassosi",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Anoura caudifer",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Callicebus personatus",
    scientificName,
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Micoureus paraguayanus",
    "Marmosa paraguayana",
    accepted_name
  )) %>%
  mutate(accepted_name = ifelse(
    scientificName == "Alouatta fusca",
    "Alouatta guariba",
    accepted_name
  ))

# Remove Cebus spp. to identify separately based on the record site
to_remove <-
  synonyms_df_corrected %>%
  filter(str_detect(scientificName, "Cebus"))
synonyms_df_corrected <- anti_join(synonyms_df_corrected, to_remove)

# Separate the reviewed and not-reviewed synonymd by rl.synonyms()
iucn_synonyms_df <-
  synonyms_df_corrected %>%
  filter(!is.na(accepted_name))

no_iucn_synonyms_df <-
  synonyms_df_corrected %>%
  filter(is.na(accepted_name))

# Get name.backbone ------------------------------------------
apply_backbone_iucn <-
  lapply(iucn_synonyms_df$accepted_name, name.backbone)
apply_backbone_gbif <-
  lapply(no_iucn_synonyms_df$scientificName, name.backbone)

backbone_iucn_df <- ldply(apply_backbone_iucn, data.frame)
backbone_gbif_df <- ldply(apply_backbone_gbif, data.frame)

# Keep a scientificName column as a key to merge
backbone_iucn_df_selected <-
  backbone_iucn_df %>%
  select(order, family, canonicalName, scientificName)

colnames(backbone_iucn_df_selected) <-
  c("order", "family", "species", "scientificName")

backbone_gbif_df_selected <-
  backbone_gbif_df %>%
  select(order, family, species, scientificName)

# Correct species
backbone_gbif_df_selected <-
  backbone_gbif_df_selected %>%
  mutate(species = ifelse(
    scientificName == "Puma yagouaroundi" |
      scientificName == "Puma yaguarondi",
    "Herpailurus yagouaroundi",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Lutra brasiliensis",
    "Pteronura brasiliensis",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Mycetes ursinus",
    "Alouatta guariba",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Mazama simplicicornis",
    "Mazama gouazoubira",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Dicotyles labiatus",
    "Tayassu pecari",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Ateles hypoxanthus",
    "Brachyteles hypoxanthus",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Dasyprocta agouti",
    "Dasyprocta leporina",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Cavia aguti" |
      scientificName == "Cavia agouti",
    "Cuniculus paca",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Oryzomys trinitatis",
    "Oecomys trinitatis",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Callithrix leucocephalus",
    "Callithrix geoffroyi",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Lagothrix infumata",
    "Lagothrix lagotricha",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Synetheres prehensilis",
    "Coendou prehensilis",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Trinomys atiosus",
    "Trinomys gratiosus",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Guerlinguetus ingrami",
    "Guerlinguetus ingrami",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Rhipidomys tribei",
    "Rhipidomys tribei",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Philander quica",
    scientificName,
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Metachirus myosurus",
    scientificName,
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Lichonycteris degener",
    scientificName,
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Leopardus tigrinus",
    "Leopardus guttulus",
    species
  )) %>%
  # Nascimento, 2010. Revisão taxonomica do gênero Leopardus. Tese doutorado, USP.
  mutate(species = ifelse(
    scientificName == "Felis brasiliensis",
    "Leopardus pardalis",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Epitesicus furinalis",
    "Eptesicus furinalis",
    species
  )) %>%
  mutate(species = ifelse(
    scientificName == "Blarynomis breviceps",
    "Blarynomis breviceps",
    species
  )) %>%
  mutate(order = ifelse(
    scientificName == "Blarynomis breviceps",
    "Rodentia",
    order
  )) %>%
  mutate(family = ifelse(
    scientificName == "Blarynomis breviceps",
    "Cricetidae",
    family
  )) %>%
  mutate(order = ifelse(
    scientificName == "Epitesicus furinalis",
    "Chiroptera",
    order
  )) %>%
  mutate(family = ifelse(
    scientificName == "Epitesicus furinalis",
    "Vespertilionidae",
    family
  ))

# Bind backbones
backbone_sp_gbif_iucn_df <-
  bind_rows(backbone_iucn_df_selected, backbone_gbif_df_selected)

# Remove repeated rows to do merge
backbone_sp_gbif_iucn_df <-
  as.data.frame(unique(backbone_sp_gbif_iucn_df))

# Remove Cebus spp. to be handled later
to_remove <-
  data_all_clipped %>%
  filter(str_detect(scientificName, "Cebus"))
data_all_without_sapajus <- anti_join(data_all_clipped, to_remove)

# Remove old columns to add new identified columns from name.backbone()
data_all_without_sapajus <-
  data_all_without_sapajus %>%
  select(-c(order, family))

# Little fix
backbone_sp_gbif_iucn_df <-
  backbone_sp_gbif_iucn_df %>%
  mutate(scientificName = sub(
    "Brucepattersonius griserufescens", 
    "Brucepattersonius iserufescens", 
    scientificName))

# Merge backbones with the main dataframe
data_all_backbone_iucn_gbif_merged <-
  merge(data_all_without_sapajus, backbone_sp_gbif_iucn_df, by = "scientificName", all = TRUE)

# Removing extra rows created during merge
data_all_backbone_iucn_gbif_merged <-
  data_all_backbone_iucn_gbif_merged %>%
  filter(!is.na(decimalLatitude))

# Keep columns ordered as in the main dataframe
data_iucn_gbif_ordered <-
  data_all_backbone_iucn_gbif_merged %>%
  select(colnames(data_all), species)

# Sapajus spp. -----------------------------------------------
S_nigritus <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    decimalLatitude <= -19.5
  )
S_robustus <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    decimalLatitude > -19.5 && decimalLatitude < -15.8
  )

S_xanthosternos <-
  data_all_clipped %>%
  filter(
    str_detect(scientificName, "Cebus"),
    decimalLatitude >= -15.8
  )

S_nigritus$acceptedNameUsage <- "Sapajus nigritus"
S_robustus$acceptedNameUsage <- "Sapajus robustus"
S_xanthosternos$acceptedNameUsage <- "Sapajus xanthosternos"

sapajus_df <-
  bind_rows(S_nigritus, S_robustus, S_xanthosternos)

# Get name.backbone
# This might take a while
apply_backbone_sapajus <-
  lapply(sapajus_df$acceptedNameUsage, name.backbone)
backbone_sapajus_df <- ldply(apply_backbone_sapajus, data.frame)

backbone_sapajus_df_selected <-
  backbone_sapajus_df %>%
  select(order, family, species)

# Remove old columns to add new identified columns from name.backbone()
sapajus_df <-
  sapajus_df %>%
  select(-c(order, family))

# Bind Sapajus backbone with Sapajus data.frame
sapajus_df_final <-
  bind_cols(sapajus_df, backbone_sapajus_df_selected)

# Keep columns ordered as in the main dataframe
sapajus_df_final_ordered <-
  sapajus_df_final %>%
  select(colnames(data_all_clipped), species)

# Bind all dataframes ----------------------------------------
data_all_united <-
  bind_rows(data_iucn_gbif_ordered, sapajus_df_final_ordered)

# Use column "species" as the species current name
data_all_united <-
  data_all_united %>%
  select(-acceptedNameUsage)

# Remove non-native species ----------------------------------
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

# Remove CCMA non-native species
data_all_sp_clean <-
  data_all_united %>%
  filter(!species %in% exotic_sp_list$species)

# Remove marine mammals
to_remove <-
  data_all_sp_clean %>%
  filter(str_detect(order, "Cetacea"))
data_all_sp_clean <- anti_join(data_all_sp_clean, to_remove)

# Correct some species names ---------------------------------
data_all_sp_clean <-
  data_all_sp_clean %>%
  mutate(species = ifelse(
    species == "Puma yagouaroundi",
    "Herpailurus yagouaroundi",
    species
  )) %>%
  mutate(species = ifelse(
    species == "Gardnerycteris crenulatum",
    "Mimon crenulatum",
    species
  )) %>%
  mutate(species = ifelse(
    species == "Natalus stramineus",
    "Natalus macrourus",
    species
  )) %>%
  mutate(species = sub(
    "Oxymycterus caparoae", 
    "Oxymycterus caparaoe", 
    species
    )) %>%
  mutate(species = sub(
    "Brucepattersonius iserufescens", 
    "Brucepattersonius griserufescens", 
    species
    )) %>%
  # (retirado de Reis et al. 2017) "Estudos genéticos de Baker et al. (1998) e   #de Morales e Bickham (1995) indicam que L. borealis limita-se ao centro
  #-oeste dos EUA e Canadá, e nordeste do México. Todas as outras populações, 
  # com exceção das Antilhas (que podem representar uma outra espécie), 
  # estariam incluídas em L. blossevillii (REID, 1997)."
  mutate(species = ifelse(
    species == "Lasiurus borealis",
    "Lasiurus blossevillii",
    species
  )) %>%
  mutate(species = ifelse(
    str_detect(species, "Felis"),
    "Leopardus pardalis",
    species
    )) %>%
  mutate(species = sub(
    "myosurus", 
    "myosuros", 
    species
    )) %>%
  mutate(species = sub(
    "Blarynomis", 
    "Blarinomys", 
    species
    )) %>%
  mutate(species = sub(
    "arviculoides", 
    "cursor", 
    species
    )) %>%
  mutate(species = sub(
    "Oligoryzomys eliurus", 
    "Oligoryzomys nigripes", 
    species
    )) %>%
  mutate(species = ifelse(
    str_detect(species, "Hylaeamys"),
    "Hylaeamys laticeps",
    species
    )) %>%
  mutate(species = sub(
    "Dasyprocta aguti", 
    "Dasyprocta leporina", 
    species))


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

clean_data <- merge(data_all_sp_clean, species_df, by = "species", all = TRUE)

# Select columns of interest
clean_data_slct <-
  clean_data %>%
  select(
    reference,
    citation,
    PublicationYear,
    datasetName,
    institutionCode,
    collectionCode,
    catalogNumber,
    recordedBy,
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

clean_data_slct <-
  clean_data_slct %>%
  filter(!is.na(species))

# Remove duplicated records
clean_data_distincted <-
  clean_data_slct %>%
  
  # Keep the first option with year, and remove the others
  arrange(year) %>%
  mutate(catalogNumber = str_remove_all(catalogNumber, ".0")) %>%
  mutate(
    catalogNumber = str_remove_all(catalogNumber, "[:alpha:]"),
    catalogNumber = str_remove_all(catalogNumber, ":")
  ) %>%
  distinct(
    species,
    catalogNumber,
    institutionCode,
    decimalLatitude,
    decimalLongitude,
    .keep_all = TRUE
  )


# Save data.frame --------------------------------------------
write.csv(
  clean_data_distincted,
  "../data/processed-data/clean-mammal-data.csv"
)
