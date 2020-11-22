# File purpose: Clean and standardize raw papers data
# Date: 16/11/2020

# Load in libraries
library(tidyverse)

# Source functions
source("./R-scripts/functions/00-funs-clean-papers-data.R")

# Load in data
raw_data <-
  read.csv("../data/raw-data/raw-papers-data.csv", stringsAsFactors = FALSE)
references_df <-
  read.csv("../data/raw-data/references.csv")

# Remove marine mammals
data_modif <- raw_data[!(raw_data$order == "Cetartiodactyla" |
                           raw_data$order == "Cetacea"), ]

# Remove unpublished data
data_modif <-
  data_modif %>%
  filter(!str_detect(typeOfPublication, "Unpubl"))

# Remove rows without coordinates
data_modif <- remove.row.without.coordinates(data_modif)

# Standardize writing ---------------------------------------------------------

# Standardize scientificName writing
# Removing special characters
data_modif$scientificName <-
  str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

data_modif$scientificName <- as.character(data_modif$scientificName)

# Correcting "Gender.species"
data_modif$scientificName <-
  gsub("[.]", " ", data_modif$scientificName)

data_modif$scientificName[data_modif$scientificName == "Alouatta fusca "] <-
  "Alouatta fusca"
data_modif$scientificName[data_modif$scientificName == "Sapajus  robustus"] <-
  "Sapajus robustus"
data_modif$scientificName[data_modif$scientificName == "Tamanduá tetradactyla"] <-
  "Tamandua tetradactyla"
data_modif$scientificName[data_modif$scientificName == "Eira barbara "] <-
  "Eira barbara"
data_modif$scientificName[data_modif$scientificName == "Mazama  americana "] <-
  "Mazama americana"
data_modif$scientificName[data_modif$scientificName == "Dicotyles labiatus "] <-
  "Dicotyles labiatus"
data_modif$scientificName[data_modif$scientificName == "Sapajus  robustus "] <-
  "Sapajus robustus"
data_modif$scientificName[data_modif$scientificName == "Panthera onca "] <-
  "Panthera onca"
data_modif$scientificName[data_modif$scientificName == "Phantera onca"] <-
  "Panthera onca"
data_modif$scientificName[data_modif$scientificName == "Oligoryzomys gr  nigripes"] <-
  "Oligoryzomys gr. nigripes"
data_modif$scientificName[data_modif$scientificName == "Akodon gr  cursor"] <-
  "Akodon gr. cursor"
data_modif$scientificName[data_modif$scientificName == "Rhipidomys cf  mastacalis"] <-
  "Rhipidomys cf. mastacalis"
data_modif$scientificName[data_modif$scientificName == "Dasyprocta aguti"] <-
  "Dasyprocta agouti"
data_modif$scientificName[data_modif$scientificName == "Didelphis aurita "] <-
  "Didelphis aurita"
data_modif$scientificName[data_modif$scientificName == "Galictis vittata "] <-
  "Galictis vittata"
data_modif$scientificName[data_modif$scientificName == "Galictis vitatta"] <-
  "Galictis vittata"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yaguarond"] <-
  "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yaguarondi"] <-
  "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yagouarondi"] <-
  "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Bradypus torquatus "] <-
  "Bradypus torquatus"
data_modif$scientificName[data_modif$scientificName == "Brachyteles hypoxanthus "] <-
  "Brachyteles hypoxanthus"
data_modif$scientificName[data_modif$scientificName == "Callithrix kuhli"] <-
  "Callithrix kuhlii"
data_modif$scientificName[data_modif$scientificName == "Cavis fulgida"] <-
  "Cavia fulgida"
data_modif$scientificName[data_modif$scientificName == "Diaemus youngi"] <-
  "Diaemus youngii"
data_modif$scientificName[data_modif$scientificName == "Euryoryzomys russatus"] <-
  "Euryoryzomys russatus"
data_modif$scientificName[data_modif$scientificName == "Hydrochaeris hydrochaeris"] <-
  "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrochaeris "] <-
  "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrochaeris"] <-
  "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrocharis"] <-
  "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Leontophitecus chrysomelas"] <-
  "Leontopithecus chrysomelas"
data_modif$scientificName[data_modif$scientificName == "Mazama gouazoupira"] <-
  "Mazama gouazoubira"
data_modif$scientificName[data_modif$scientificName == "Mazama guazoubira"] <-
  "Mazama gouazoubira"
data_modif$scientificName[data_modif$scientificName == "Metachirus nudicaudatus "] <-
  "Metachirus nudicaudatus"
data_modif$scientificName[data_modif$scientificName == "Myrmecophaga tridactyla "] <-
  "Myrmecophaga tridactyla"
data_modif$scientificName[data_modif$scientificName == "Necromys laisiurus"] <-
  "Necromys lasiurus"
data_modif$scientificName[data_modif$scientificName == "Olygoryzomys nigripes"] <-
  "Oligoryzomys nigripes"
data_modif$scientificName[data_modif$scientificName == "Oryzomys laticeps "] <-
  "Oryzomys laticeps"
data_modif$scientificName[data_modif$scientificName == "Peropteryx cf  kappleri"] <-
  "Peropteryx cf. kappleri"
data_modif$scientificName[data_modif$scientificName == "Philander frenata"] <-
  "Philander frenatus"
data_modif$scientificName[data_modif$scientificName == "Priodontes maximus "] <-
  "Priodontes maximus"
data_modif$scientificName[data_modif$scientificName == "Procyon cancrivoros"] <-
  "Procyon cancrivorus"
data_modif$scientificName[data_modif$scientificName == "Tapirus terrestris "] <-
  "Tapirus terrestris"
data_modif$scientificName[data_modif$scientificName == "Trinomys miraptanga"] <-
  "Trinomys mirapitanga"
data_modif$scientificName[data_modif$scientificName == "Calicebus personatus"] <-
  "Callicebus personatus"
data_modif$scientificName[data_modif$scientificName == "Guerlinguetus  aestuans"] <-
  "Guerlinguetus aestuans"
data_modif$scientificName[data_modif$scientificName == "Micoureus  paraguayanus"] <-
  "Micoureus paraguayanus"
data_modif$scientificName[data_modif$scientificName == "Sapajus libidinosus libidinosus"] <-
  "Sapajus libidinosus"
data_modif$scientificName[data_modif$scientificName == "Peropteryx cf. kappleri"] <-
  "Peropteryx kappleri"
data_modif$scientificName[data_modif$scientificName == "Peropteryx trinitatis trinitatis"] <-
  "Peropteryx trinitatis"
data_modif$scientificName[data_modif$scientificName == "Sciurus alphonsei alphonsei"] <-
  "Sciurus alphonsei"

# Standardize country writing
data_modif$country <- "Brazil"

# Standardize stateProvince writing
data_modif$stateProvince <- as.character(data_modif$stateProvince)

data_modif$stateProvince[data_modif$stateProvince == "Espírito Santo"] <-
  "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ES"] <-
  "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "Espiríto Santo"] <-
  "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ESPIRITO SANTO"] <-
  "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "BAHIA"] <-
  "Bahia"
data_modif$stateProvince[data_modif$stateProvince == "BA"] <-
  "Bahia"

# Standardize UC writing
data_modif$UC <- as.character(data_modif$UC)

data_modif$UC[data_modif$UC == "yes"] <-
  "Yes"
data_modif$UC[data_modif$UC == "no"] <-
  "No"
data_modif$UC[data_modif$UC == "Parque Estadual da Fonte Grande"] <-
  "Yes"
data_modif$UC[is.na(data_modif$UC)] <-
  ""

# Standardize georeferencePrecision writing
data_modif$georeferencePrecision <-
  as.character(data_modif$georeferencePrecision)

data_modif$georeferencePrecision[data_modif$georeferencePrecision == "precise"] <-
  "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "localidade"] <-
  "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Localidade"] <-
  "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "notPrecise"] <-
  "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not-Precise"] <-
  "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not precise"] <-
  "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "[no data]"] <-
  ""

# Standardize collectionCode writing
data_modif$collectionCode <- as.character(data_modif$collectionCode)

data_modif$collectionCode[data_modif$collectionCode == "Coleção de Mamíferos  do Museu Nacional (MN)"] <-
  "MN - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o de Mam�feros, Universidade Estadual de Santa Cruz (UESC), Ilh�us, BA, Brazil"] <-
  "UESC - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <-
  "UFRRJ - ALP Collection"
data_modif$collectionCode[data_modif$collectionCode == "Laboratorio de Diversidade de Morcegos (LDM) and Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <-
  "UFRRJ -LDM and ALP Collections"
data_modif$collectionCode[data_modif$collectionCode == "Museum of Vertebrate Zoology"] <-
  "MVZ"
data_modif$collectionCode[data_modif$collectionCode == "Not located"] <-
  ""

# Standardize fieldNumber writing
data_modif$fieldNumber <- as.character(data_modif$fieldNumber)

data_modif$fieldNumber[data_modif$fieldNumber == "?"] <- ""

# Standardize preparations writing
data_modif$preparations <- as.character(data_modif$preparations)

data_modif$preparations[data_modif$preparations == "pele"] <-
  "Skin"
data_modif$preparations[data_modif$preparations == "skin, study"] <-
  "Skin"
data_modif$preparations[data_modif$preparations == "skin (dry)"] <-
  "Skin"
data_modif$preparations[data_modif$preparations == "Pele/ pelo"] <-
  "Skin"
data_modif$preparations[data_modif$preparations == "espécime inteiro (ETOH)"] <-
  "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "whole animal (ethanol)"] <-
  "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "filhotes (ETOH)"] <-
  "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "whole organism (ethanol)"] <-
  "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "corpo (ETOH)"] <-
  "Body (ethanol)"
data_modif$preparations[data_modif$preparations == "esqueleto completo"] <-
  "Whole skeleton"
data_modif$preparations[data_modif$preparations == "esqueleto parcial"] <-
  "Partial skeleton"
data_modif$preparations[data_modif$preparations == "Sangue"] <-
  "Blood"
data_modif$preparations[data_modif$preparations == "sangue"] <-
  "Blood"
data_modif$preparations[data_modif$preparations == "Carcaça"] <-
  "Carcass"
data_modif$preparations[data_modif$preparations == "carcaça (ETOH)"] <-
  "Carcass (ethanol)"
data_modif$preparations[data_modif$preparations == "Carcaça / Fígado"] <-
  "Carcass/Liver"
data_modif$preparations[data_modif$preparations == "Álcool"] <-
  "Ethanol"
data_modif$preparations[data_modif$preparations == "embriões (ETOH)"] <-
  "Embryos (ethanol)"
data_modif$preparations[data_modif$preparations == "crânio"] <-
  "Skull"
data_modif$preparations[data_modif$preparations == "Álcool e Crânio"] <-
  "Skull"
data_modif$preparations[data_modif$preparations == "skull"] <-
  "Skull"
data_modif$preparations[data_modif$preparations == "skull (dry)"] <-
  "Skull"
data_modif$preparations[data_modif$preparations == "tissue (ethanol)"] <-
  "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tecido (ETOH)"] <-
  "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tissue (95% ethanol)"] <-
  "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tissue (frozen)"] <-
  "Tissue (frozen)"
data_modif$preparations[data_modif$preparations == "Líquido"] <-
  "Liquid"
data_modif$preparations[data_modif$preparations == "seca"] <-
  "Dry"
data_modif$preparations[data_modif$preparations == "Cartilagem"] <-
  "Cartilage"
data_modif$preparations[data_modif$preparations == "cytogenetic"] <-
  "Cytogenetic"
data_modif$preparations[data_modif$preparations == "skull; tissue (frozen); skin,"] <-
  "Skull; tissue (frozen); skin"
data_modif$preparations[data_modif$preparations == "skull; skin, study"] <-
  "Skull; skin, study"
data_modif$preparations[data_modif$preparations == "fragmento de mandíbula"] <-
  "Jaw fragment"
data_modif$preparations[data_modif$preparations == "Carapaça"] <-
  "Hoof"

# Correct columns -------------------------------------------------------------

# 1. Add PublicationYear
# Add publication year when data lacks this information
data_modif <- add.PublicationYear(data_modif)

# 2. Add eventYear
# Consider event date or publication year as event year when data lacks this 
# information
data_modif <- add.eventYear(data_modif)

# 3. Correct eventYear 
# Correct eventYear column to keep just the year
data_modif <- correct.eventYear(data_modif)

# Correct geographical coordinates
# Move coordinates in the wrong column to the correct one
data_modif <- correct.coordinates.in.geodeticDatum(data_modif)

# Correct mixed latitude/longitude
data_modif <- correct.mixed.latlong(data_modif)

# Convert coordinates to lat/long --------------------------------------------

# Convert coordinates in degree to lat/long
# Ignore:
# > Warning messages:
# > 1: Expected 2 pieces. Additional pieces discarded in 282 rows [1, 2, 3, 4,
# > 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
data_modif <- convert.coordinate.degree.to.latlong(data_modif)

# Convert coordinates in UTM to lat/long
utm_data_modif <-
  data_modif %>%
  filter(str_detect(geodeticDatum, "UTM")) %>%
  select(
    verbatimLatitude,
    verbatimLongitude,
    decimalLatitude,
    decimalLongitude,
    geodeticDatum
  )

utm <-
  utm_data_modif %>%
  select(verbatimLatitude, verbatimLongitude)

utm_data_modif$decimalLatitude <- NA

df <- data.frame()
for (i in 1:length(unique(utm$verbatimLatitude))) {
  x <-
    utm_data_modif %>%
    filter(verbatimLatitude == sort(unique(utm$verbatimLatitude))[i]) %>%
    select(verbatimLatitude,
           verbatimLongitude,
           decimalLatitude,
           decimalLongitude)
  df <- bind_rows(x[1,], df)
}

df <- arrange(df, by = verbatimLatitude)

# Convert UTM to decimal degree http://splink.cria.org.br/conversor
df$decimalLatitude <-
  c(
    "-16.599388",
    "-17.169331",
    "-13.578941",
    "-13.864987",
    "-17.292106",
    "-15.927010",
    "-15.155310",
    "-13.952912",
    "-16.286450",
    "-15.197319",
    "-15.973720",
    "-17.106801",
    "-16.512313",
    "-13.701098",
    "-15.619982",
    "-14.018092",
    "-16.324448",
    "-14.343671",
    "-15.172064",
    "-14.424250",
    "-15.166549",
    "-13.525323"
  )
df$decimalLongitude <-
  c(
    "-39.913983",
    "-39.841776",
    "-39.706685",
    "-39.672635",
    "-39.673031",
    "-39.635847",
    "-39.526954",
    "-39.451138",
    "-39.424079",
    "-39.391085",
    "-39.373680",
    "-39.339753",
    "-39.303612",
    "-39.232629",
    "-39.161263",
    "-39.143283",
    "-39.121001",
    "-39.086907",
    "-39.061124",
    "-39.060414",
    "-39.059754",
    "-39.035311"
  )

utm_data_modif <- merge(utm_data_modif, df, by = "verbatimLatitude")

# Return to main dataframe
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLatitude.y

data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLongitude.y

# Manipulate references ----------------------------------------
references_df$reference <- as.character(references_df$reference)
data_modif$reference <- as.character(data_modif$reference)

data_modif <- merge(data_modif, references_df, all = TRUE)

# Remove references
to_remove <-
  data_modif %>%
  filter(reference_std == "removed")

data_modif <- anti_join(data_modif, to_remove)

# assume thesis year, instead of publication year
to_corret_year <-
  data_modif %>%
  filter(str_detect(reference, "Hirsch, A.,"))
data_modif <- anti_join(data_modif, to_corret_year)

to_corret_year$year <- "1988"
data_modif$year <- as.character(data_modif$year)
data_modif <- bind_rows(data_modif, to_corret_year)

to_remove <-
  data_modif %>%
  filter(citation == "Aguirre (1971)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Gatti et al. (2014)", year == "1988" |
           datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Kinzey (1982)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Mittermeier et al. (1987)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Moura (2003)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Palma (1996)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Paresque et al. (2004)", datasetName != "LGA")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Passamani (2000)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Passamani et al. (2005)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Passamani & Fernandez (2011)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(citation == "Passamani et al. (2000)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(
    citation == "Passamani & Ribeiro (2009)",
    datasetName == "",
    scientificName != "Callithrix geoffroyi",
    scientificName != "Sciurus aestuans"
  )
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>% 
  filter(citation == "Pinto (1994)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>% 
  filter(citation == "Santos et al. (2004)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>% 
  filter(citation == "Santos et al. (1987)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- 
  data_modif %>% 
  filter(
  citation == "Soares et al. (2013)",
  scientificName != "Cerdocyon thous",
  scientificName != "Cuniculus paca",
  scientificName != "Dasyprocta leporina",
  scientificName != "Dasypus novemcinctus",
  scientificName != "Didelphis aurita",
  scientificName != "Leopardus wiedii",
  scientificName != "Lontra longicaudis",
  scientificName != "Marmosa murina",
  scientificName != "Mazama americana",
  scientificName != "Metachirus nudicaudatus",
  scientificName != "Nasua nasua",
  scientificName != "Philander opossum",
  scientificName != "Sciurus aestuans"
)
data_modif <- anti_join(data_modif, to_remove)

to_remove <- 
  data_modif %>%
  filter(
    citation == "Srbek-Araujo & Chiarello (2005)",
    scientificName != "Didelphis aurita",
    scientificName != "Metachirus nudicaudatus",
    scientificName != "Philander frenata",
    #just second year
    scientificName != "Dasypus novemcinctus",
    scientificName != "Euphractus sexcinctus",
    #just second year
    scientificName != "Cerdocyon thous",
    scientificName != "Nasua nasua",
    scientificName != "Procyon cancrivorus",
    scientificName != "Eira barbara",
    #just first year
    scientificName != "Didelphis aurita",
    scientificName != "Herpailurus yaguarondi",
    scientificName != "Leopardus pardalis",
    scientificName != "Herpailurus yaguarondi",
    scientificName != "Leopardus tigrinus",
    scientificName != "Puma concolor",
    scientificName != "Mazama americana",
    scientificName != "Pecari tajacu",
    scientificName != "Sciurus aestuans",
    scientificName != "Hydrochoerus hydrochaeris",
    scientificName != "Cuniculus paca",
    scientificName != "Dasyprocta leporina",
    scientificName != "Sylvilagus brasiliensis"
  )
data_modif <- anti_join(data_modif, to_remove)

to_remove <- 
  data_modif %>%
  filter(
    citation == "Srbek-Araujo & Chiarello (2013)",
    scientificName != "Didelphis aurita",
    scientificName != "Metachirus nudicaudatus",
    scientificName != "Cabassous tatouay",
    scientificName != "Euphractus sexcinctus",
    scientificName != "Cerdocyon thous",
    scientificName != "Tamandua tetradactyla",
    scientificName != "Procyon cancrivorus",
    scientificName != "Callithrix geoffroyi",
    scientificName != "Sapajus robustus",
    scientificName != "Cerdocyon thous",
    scientificName != "Galictis cuja",
    scientificName != "Nasua nasua",
    scientificName != "Procyon cancrivorus",
    scientificName != "Eira barbara",
    scientificName != "Leopardus pardalis",
    scientificName != "Leopardus wiedii",
    scientificName != "Panthera onca",
    scientificName != "Puma concolor",
    scientificName != "Herpailurus yagouaroundi",
    scientificName != "Tapirus terrestris",
    scientificName != "Tayassu pecari",
    scientificName != "Pecari tajacu",
    scientificName != "Guerlinguetus ingrami",
    scientificName != "Hydrochoerus hydrochaeris",
    scientificName != "Cuniculus paca",
    scientificName != "Dasyprocta leporina",
    scientificName != "Sylvilagus brasiliensis"
  )
data_modif <- anti_join(data_modif, to_remove)

to_remove <- 
  data_modif %>%
  filter(citation == "Tonini et al. (2010)", datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_correct <-
  data_modif %>% 
  filter(citation == "Travassos & Freitas (1948)")
correct <- 
  to_correct %>% 
  distinct(scientificName, .keep_all = TRUE)
data_modif <- anti_join(data_modif, to_correct)
data_modif <- bind_rows(data_modif, correct)

# Save clean data.frame --------------------
write.csv(data_modif, "../data/processed-data/clean-papers-data.csv")

