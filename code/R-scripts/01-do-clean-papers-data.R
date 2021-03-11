# File purpose: Clean and standardize raw papers data
# Date: 16/11/2020

# Load in libraries
xfun::pkg_attach(c("tidyverse", "lubridate", "pdftools", "stringi", "rgdal"))

# Source functions
source("./R-scripts/functions/01-funs-clean-papers-data.R")

# Load in data
raw_data <-
  read.csv("../data/raw-data/raw-papers-data.csv", stringsAsFactors = FALSE)
references_df <-
  read.csv("../data/raw-data/references.csv")

# Remove marine mammals
to_remove <-
  raw_data %>%
  filter(str_detect(order, "Cetariodac") | str_detect(order, "Cetacea"))

data_modif <- anti_join(raw_data, to_remove)

# Remove unpublished data
data_modif <-
  data_modif %>%
  filter(!str_detect(typeOfPublication, "Unpubl"))

# Remove rows without coordinates
data_modif <- remove.row.without.coordinates(data_modif)

# Standardize writing -------------------------------------

# Standardize scientificName writing
# Remove special characters and correct "Gender.species"
data_modif <-
  data_modif %>%
  mutate(scientificName = str_replace(scientificName, "[^[:alnum:]]", " ")) %>%
  mutate(scientificName = as.character(scientificName)) %>%
  mutate(scientificName = gsub("[.]", " ", scientificName))

data_modif <-
  data_modif %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "fusca"),
    "Alouatta fusca",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "tetradactyla"),
    "Tamandua tetradactyla",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "nigripes"),
    "Oligoryzomys nigripes",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "cursor"),
    "Akodon cursor",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "mastacalis"),
    "Rhipidomys mastacalis",
    scientificName
  )) %>%
  mutate(
    scientificName = ifelse(
      str_detect(scientificName, "yaguarond") |
        str_detect(scientificName, "yagouarondi"),
      "Herpailurus yagouaroundi",
      scientificName
    )
  ) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "kuhli"),
    "Callithrix kuhlii",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Phantera onca"),
    "Panthera onca",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "aguti"),
    "Dasyprocta agouti",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "vitatta"),
    "Galictis vittata",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Cavis fulgida"),
    "Cavia fulgida",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Diaemus youngi"),
    "Diaemus youngii",
    scientificName
  )) %>%
  mutate(
    scientificName = ifelse(
      str_detect(scientificName, "hydrochaeris") |
        str_detect(scientificName, "Hydrochoerus"),
      "Hydrochoerus hydrochaeris",
      scientificName
    )
  ) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "chrysomelas"),
    "Leontopithecus chrysomelas",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Mazama g"),
    "Mazama gouazoubira",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Necromys la"),
    "Necromys lasiurus",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "zomys nigripes"),
    "Oligoryzomys nigripes",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "kappleri"),
    "Peropteryx kappleri",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Philander frenat"),
    "Philander frenatus",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Procyon cancri"),
    "Procyon cancrivorus",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Trinomys mirap"),
    "Trinomys mirapitangas",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "licebus personatus"),
    "Callicebus personatus",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Sapajus libidinosus"),
    "Sapajus libidinosus",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Peropteryx trinitatis"),
    "Peropteryx trinitatis",
    scientificName
  )) %>%
  mutate(scientificName = ifelse(
    str_detect(scientificName, "Sciurus alphonsei"),
    "Sciurus alphonsei",
    scientificName
  ))

# Standardize country writing
data_modif$country <- "Brazil"

# Standardize stateProvince writing
data_modif <-
  data_modif %>%
  mutate(stateProvince = as.character(stateProvince)) %>%
  mutate(
    stateProvince = ifelse(
      str_detect(stateProvince, "anto") |
        str_detect(stateProvince, "ANTO") |
        str_detect(stateProvince, "ES"),
      "Espirito Santo",
      stateProvince
    )
  ) %>%
  mutate(stateProvince = ifelse(str_detect(stateProvince, "BA"),
    "Bahia",
    stateProvince
  ))

# Standardize UC writing
data_modif <-
  data_modif %>%
  mutate(UC = as.character(UC)) %>%
  mutate(UC = ifelse(str_detect(UC, "yes") |
    str_detect(UC, "Parque"), "Yes", UC)) %>%
  mutate(UC = ifelse(str_detect(UC, "no"), "No", UC))

# Standardize georeferencePrecision writing
data_modif <-
  data_modif %>%
  mutate(georeferencePrecision = as.character(georeferencePrecision)) %>%
  mutate(
    georeferencePrecision = ifelse(
      str_detect(georeferencePrecision, "calidade") |
        georeferencePrecision == "precise",
      "Precise",
      georeferencePrecision
    )
  ) %>%
  mutate(georeferencePrecision = ifelse(
    str_detect(georeferencePrecision, "ot"),
    "NotPrecise",
    georeferencePrecision
  )) %>%
  mutate(georeferencePrecision = ifelse(
    str_detect(georeferencePrecision, "[no data]"),
    "",
    georeferencePrecision
  ))

# Standardize collectionCode writing
data_modif <-
  data_modif %>%
  mutate(collectionCode = as.character(collectionCode)) %>%
  mutate(collectionCode = ifelse(
    str_detect(collectionCode, "Museu Nacional"),
    "MN - Mammal Collection",
    collectionCode
  )) %>%
  mutate(collectionCode = ifelse(
    str_detect(collectionCode, "UESC"),
    "UESC - Mammal Collection",
    collectionCode
  )) %>%
  mutate(collectionCode = ifelse(
    str_detect(collectionCode, "ALP"),
    "UFRRJ - ALP Collection",
    collectionCode
  )) %>%
  mutate(collectionCode = ifelse(
    str_detect(collectionCode, "Museum of Vertebrate Zoology"),
    "MVZ",
    collectionCode
  )) %>%
  mutate(collectionCode = ifelse(str_detect(collectionCode, "Not located"),
    "",
    collectionCode
  ))

# Standardize fieldNumber writing
data_modif <-
  data_modif %>%
  mutate(fieldNumber = as.character(fieldNumber)) %>%
  mutate(fieldNumber = ifelse(str_detect(fieldNumber, "[?]"), "", fieldNumber))

# Standardize preparations writing
data_modif <-
  data_modif %>%
  mutate(preparations = as.character(preparations)) %>%
  mutate(preparations = ifelse(str_detect(preparations, "pele"),
    "Skin",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "angue"),
    "Blood",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "arcaça"),
    "Carcass",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "ecido"),
    "Tissue",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "quido"),
    "Liquid",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "seca"),
    "Dry",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "espécime inteiro") |
    str_detect(preparations, "filhotes") |
    str_detect(preparations, "orpo (ETOH)"),
  "Body (ethanol)",
  preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "squeleto"),
    "Skeleton",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "Cartilagem"),
    "Cartilage",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "lcool"),
    "Ethanol",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "embriões"),
    "Embryos",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "rânio"),
    "Skull",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "fragmento de mandíbula"),
    "Jaw fragment",
    preparations
  )) %>%
  mutate(preparations = ifelse(str_detect(preparations, "Carapaça"),
    "Hoof",
    preparations
  ))


# Correct columns -----------------------------------------

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

# Correct eventDate column
data_modif <- correct.eventDate(data_modif)

# Match years
data_modif <-
  data_modif %>%
  mutate(eventYear = ifelse(
    test = format(as.Date(eventDate, "%Y-%m-%d"), "%Y") != eventYear,
    yes = format(as.Date(eventDate, "%Y-%m-%d"), "%Y"),
    no = eventYear
  )) %>%
  mutate(eventYear = ifelse(
    test = !is.na(eventDate) & is.na(eventYear),
    yes = format(as.Date(eventDate, "%Y-%m-%d"), "%Y"),
    no = eventYear
  ))

# Correct geographical coordinates
# Move coordinates in the wrong column to the correct one
data_modif <- correct.coordinates.in.geodeticDatum(data_modif)

# Correct mixed latitude/longitude
data_modif <- correct.mixed.latlong(data_modif)

# Convert coordinates to lat/long -------------------------

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

coords_to_correct <- data.frame()
for (i in 1:length(unique(utm$verbatimLatitude))) {
  x <-
    utm_data_modif %>%
    filter(verbatimLatitude == sort(unique(utm$verbatimLatitude))[i]) %>%
    select(
      verbatimLatitude,
      verbatimLongitude
    )
  coords_to_correct <- bind_rows(x[1, ], coords_to_correct)
}

coords_to_correct <- arrange(coords_to_correct, by = verbatimLatitude) %>%
  mutate(
    verbatimLatitude = as.numeric(verbatimLatitude),
    verbatimLongitude = as.numeric(verbatimLongitude)
  )
utms <-
  SpatialPoints(coords_to_correct[, c("verbatimLatitude", "verbatimLongitude")], proj4string = CRS("+proj=utm +zone=24 +south +units=m +EPSG=4225 +nadgrids=ca7072_003.gsb +towgs84=-67.35,3.88,-38.22")) # create UTM matrix

longlats <- spTransform(utms, CRS("+proj=longlat")) # transform

coords_corrected <- as.data.frame(longlats) %>%
  rename("decimalLatitude" = verbatimLatitude, "decimalLongitude" = verbatimLongitude) %>%
  bind_cols(coords_to_correct)

utm_data_modif <- merge(utm_data_modif, coords_corrected, by = "verbatimLatitude")

# Return to main dataframe
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLatitude.y

data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLongitude.y

# Manipulate references -----------------------------------
references_df$reference <- as.character(references_df$reference)
data_modif$reference <- as.character(data_modif$reference)

data_modif <- merge(data_modif, references_df, all = TRUE)

data_modif <- select(data_modif, -reference)
colnames(data_modif)[34] <- "reference"

# Remove references
to_remove <-
  data_modif %>%
  filter(reference == "removed")

data_modif <- anti_join(data_modif, to_remove)

# assume thesis year, instead of publication year
data_modif <-
  data_modif %>%
  mutate(eventYear = ifelse(str_detect(reference, "Hirsch, A.,"),
    "1988",
    eventYear
  ))

to_remove <-
  data_modif %>%
  filter(citation == "Aguirre (1971)", datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <-
  data_modif %>%
  filter(
    citation == "Gatti et al. (2014)",
    eventYear == "1988" |
      datasetName == ""
  )
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

to_keep_list <- c(
  "Cerdocyon thous",
  "Cuniculus paca",
  "Dasyprocta leporina",
  "Dasypus novemcinctus",
  "Didelphis aurita",
  "Leopardus wiedii",
  "Lontra longicaudis",
  "Marmosa murina",
  "Mazama americana",
  "Metachirus nudicaudatus",
  "Nasua nasua",
  "Philander opossum",
  "Sciurus aestuans"
)
to_remove <-
  data_modif %>%
  filter(
    citation == "Soares et al. (2013)",
    (!scientificName %in% to_keep_list))
data_modif <- anti_join(data_modif, to_remove)

to_keep_list <- c(
  "Didelphis aurita",
  "Metachirus nudicaudatus",
  "Philander frenata",
  # just second year
  "Dasypus novemcinctus",
  "Euphractus sexcinctus",
  # just second year
  "Cerdocyon thous",
  "Nasua nasua",
  "Procyon cancrivorus",
  "Eira barbara",
  # just first year
  "Didelphis aurita",
  "Herpailurus yaguarondi",
  "Leopardus pardalis",
  "Herpailurus yaguarondi",
  "Leopardus tigrinus",
  "Puma concolor",
  "Mazama americana",
  "Pecari tajacu",
  "Sciurus aestuans",
  "Hydrochoerus hydrochaeris",
  "Cuniculus paca",
  "Dasyprocta leporina",
  "Sylvilagus brasiliensis"
)

to_remove <-
  data_modif %>%
  filter(
    citation == "Srbek-Araujo & Chiarello (2005)",
    (!scientificName %in% to_keep_list)
    )
data_modif <- anti_join(data_modif, to_remove)

to_keep_list <- c(
  "Didelphis aurita",
  "Metachirus nudicaudatus",
  "Cabassous tatouay",
  "Euphractus sexcinctus",
  "Cerdocyon thous",
  "Tamandua tetradactyla",
  "Procyon cancrivorus",
  "Callithrix geoffroyi",
  "Sapajus robustus",
  "Cerdocyon thous",
  "Galictis cuja",
  "Nasua nasua",
  "Procyon cancrivorus",
  "Eira barbara",
  "Leopardus pardalis",
  "Leopardus wiedii",
  "Panthera onca",
  "Puma concolor",
  "Herpailurus yagouaroundi",
  "Tapirus terrestris",
  "Tayassu pecari",
  "Pecari tajacu",
  "Guerlinguetus ingrami",
  "Hydrochoerus hydrochaeris",
  "Cuniculus paca",
  "Dasyprocta leporina",
  "Sylvilagus brasiliensis"
)
to_remove <-
  data_modif %>%
  filter(
    citation == "Srbek-Araujo & Chiarello (2013)",
    (!scientificName %in% to_keep_list)
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

# Correct bat data -------------------------------------------------------------
# BATS dataset is unreliable
data_modif <- 
  data_modif %>% 
  filter(!str_detect(datasetName, "BATS"))

# Import Faria (2006) pdf
article <-
  pdf_text("../documents/record-references/Faria (2006).pdf")

# Select Table 1
bat_table <- article[7][1]

# Split rows
bat_table_splited <- str_split(bat_table, "\n")

# Remove table header and footnotes
bat_table_splited_clean <- as.data.frame(unlist(bat_table_splited))[9:51, ]

# Remove numbers
bat_table_splited_clean <-
  gsub("[[:digit:]]+", "", bat_table_splited_clean)

# Keep only species names
bat_table_splited_clean <-
  word(bat_table_splited_clean, start = 1, end = 7)

# Remove unnecessary spaces
bat_table_df <-
  stri_trim_both(bat_table_splited_clean) %>%
  as.data.frame() %>%
  filter(!is.na(.))

# Add record information
colnames(bat_table_df) <- "scientificName"
bat_table_df$PublicationYear <- 2006
bat_table_df$typeOfPublication <- "Article"
bat_table_df$eventYear <- 2000
bat_table_df$eventDate <- as.Date("2000-01-01")
bat_table_df$country <- "Brazil"
bat_table_df$stateProvince <- "Bahia"
bat_table_df$county <- "Una"
bat_table_df$locality <- "Una Biological Reserve"
bat_table_df$decimalLatitude <- -15.1802118
bat_table_df$decimalLongitude <- -39.1729935
bat_table_df$reference <-
  "Faria, D., 2006. Phyllostomid bats of a fragmented landscape in the north-eastern Atlantic forest, Brazil. Journal of Tropical Ecology, 22(5), pp.531-542."
bat_table_df$citation <- "Faria (2006)"

data_final <- plyr::rbind.fill(data_modif, bat_table_df)

# Save clean data.frame -----------------------------------
write.csv(data_final, "../data/processed-data/clean-papers-data.csv")
