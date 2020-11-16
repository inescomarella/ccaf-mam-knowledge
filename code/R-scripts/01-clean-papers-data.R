# File purpose: Clean and standardize raw papers data
# 16/11/2020

# Load in libraries
x <-
  c("tidyverse", "CoordinateCleaner", "lubridate", "biogeo", "sf")
lapply(x, library, character.only = TRUE)

# Load in data
raw_data <-
  read.csv("../data/raw-data/raw-papers-data.csv", stringsAsFactors = FALSE)
references_df <- 
  read.csv("../data/raw-data/references.csv")

# Remove marine mammals
data_modif <- raw_data[!(raw_data$order == "Cetartiodactyla" |
                           raw_data$order == "Cetacea"),]

# Remove unpublished data
data_modif <-
  data_modif %>% filter(!str_detect(typeOfPublication, "Unpubl"))

# Standardize writing ---------------------------------------------------------

# Standardize scientificName writing
# Removing special characters
data_modif$scientificName <-
  str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

data_modif$scientificName <- as.character(data_modif$scientificName)

# Correcting 'Gender.species'
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

# Correct eventYear column ------------------------------------------------------

# Extract wrong lines: Correcting format from year/year to year
data_bar <- data_modif %>% filter(str_detect(eventYear, "[/]"))

year_bar <- as.data.frame(data_bar$eventYear)

# Separating first/last years in two columns
year_bar_sep <-
  separate(
    data = year_bar,
    col = 'data_bar$eventYear',
    into = c("A", "B"),
    sep = "[/]"
  )

# Keep the last year
year_bar_correct <- data.frame(year_bar_correct = year_bar_sep$B)

# Return corrected column
data_bar$eventYear <- year_bar_correct$year_bar_correct

# Wrong rows to remove
to_remove <- data_modif %>% filter(str_detect(eventYear, "[/]"))
data_bar_less <-
  data_modif %>% filter(!eventYear %in% to_remove$eventYear)

# Add correct rows
data_modif  <- rbind(data_bar_less, data_bar)

nrow(data_modif)
nrow(data_bar) + nrow(data_bar_less)

# Correct PublicationYear column ------------------------------------

# Extract rows with reference and without PublicationYear
PublicationYear_less <-
  data_modif %>% filter(
    is.na(PublicationYear) &
      is.na(reference) == FALSE & reference != "" |
      PublicationYear == "" &
      is.na(reference) == FALSE & reference != ""
  )

# Remove them from main dataframe
data_modif <- anti_join(data_modif, PublicationYear_less)

# Add year: assuming the first four numbers as publication year
for (i in 1:nrow(PublicationYear_less))
  PublicationYear_less$PublicationYear[i] <-
  substr(gsub("[^0-9]", "", PublicationYear_less$reference[i]), 1, 4)

data_modif$PublicationYear <-
  as.character(data_modif$PublicationYear)

# Return rows with publication year added
data_modif <- bind_rows(data_modif, PublicationYear_less)

# Extracting rows without reference
data_modif_reference_less <- data_modif %>% filter(reference == "")

# Standardizing  date in rows without reference
data_modif_reference_less$eventYear <-
  format(as.Date(data_modif_reference_less$eventDate, format = "%m/%d/%y"),
         "%Y")

# Removing those rows from main dataframe
data_modif <- data_modif %>% filter(!reference == "")

# Return them with date corrected
data_modif <- rbind(data_modif, data_modif_reference_less)

# In the absence of collect year, consider the publication year
for (i in 1:nrow(data_modif))
  if (data_modif$eventYear[i] == "" |
      is.na(data_modif$eventYear[i]))
    data_modif$eventYear[i] <- data_modif$PublicationYear[i]

# Correct geographical coordinates --------------------------------------------
# Geographical coordinates in geodeticDatum column
for (i in 1:nrow(data_modif))
  if (is.na(data_modif$decimalLatitude[i]))
    data_modif$decimalLatitude[i] <-
  as.character(data_modif$geodeticDatum[i])

# Remove from geodeticDatum
to_remove <- data_modif %>%
  select(geodeticDatum) %>%
  filter(!str_detect(geodeticDatum, "[[:alpha:] ]+")) #filtrar dados sem letras

for (i in 1:nrow(data_modif))
  if (data_modif$geodeticDatum[i] %in% to_remove$geodeticDatum)
    data_modif$geodeticDatum[i] <- NA

# Removing rows without adequate coordinates
to_correct <- data_modif %>%
  filter(str_detect(decimalLatitude, "[[:alpha:] ]+"),
         !str_detect(geodeticDatum, 'UTM'))

data_modif <- anti_join(data_modif, to_correct)

to_correct$decimalLatitude <- NA

data_modif <- bind_rows(data_modif, to_correct)

# Extract coordinates in UTM and in degrees
grau_utm_data_modif <- data_modif %>%
  filter(
    str_detect(verbatimLatitude, "[[:alpha:] ]+"),
    decimalLatitude == "" | is.na(decimalLatitude)
  ) %>%
  select(verbatimLatitude, verbatimLongitude)

# UTM coordinates in grau_utm_data_modif
utm_data_modif <- grau_utm_data_modif %>%
  filter(verbatimLatitude == "240487949 N")

# Degree coordinates
grau_data_modif <- grau_utm_data_modif %>%
  filter(!verbatimLatitude %in% utm_data_modif$verbatimLatitude)

# Preparing columns to convert to decimal degrees
# Keep a single type of degree (º) to be easily identified
grau_data_modif$verbatimLatitude <-
  grau_data_modif$verbatimLatitude %>% str_replace(pattern = "[°]", replacement = "º")
grau_data_modif$verbatimLongitude <-
  grau_data_modif$verbatimLongitude %>% str_replace(pattern = "[°]", replacement = "º")

grau_data_modif <-
  separate(
    as.data.frame(grau_data_modif),
    col = verbatimLatitude,
    into = c("grau_lat", "verbatimLatitude"),
    sep = "[º]"
  )

grau_data_modif <-
  separate(
    as.data.frame(grau_data_modif),
    col = verbatimLongitude,
    into = c("grau_long", "verbatimLongitude"),
    sep = "[º]"
  )

# Remove special characters
grau_data_modif$verbatimLongitude <-
  str_replace_all(grau_data_modif$verbatimLongitude, "[[:alpha:] ]+", "")
grau_data_modif$verbatimLatitude <-
  str_replace_all(grau_data_modif$verbatimLatitude, "[[:alpha:] ]+", "")

# Separate minute and second
grau_data_modif <-
  separate(
    as.data.frame(grau_data_modif),
    col = verbatimLatitude,
    into = c("min_lat", "seg_lat"),
    sep = "[^[:alnum:]]"
  )
grau_data_modif <-
  separate(
    as.data.frame(grau_data_modif),
    col = verbatimLongitude,
    into = c("min_long", "seg_long"),
    sep = "[^[:alnum:]]"
  )

# Remove special characters
grau_data_modif$grau_lat <-
  str_replace_all(grau_data_modif$grau_lat, "[[:alpha:] ]+", "")
grau_data_modif$grau_long <-
  str_replace_all(grau_data_modif$grau_long, "[[:alpha:] ]+", "")

grau_data_modif$seg_lat[grau_data_modif$seg_lat == ""] <- "0"
grau_data_modif$seg_long[grau_data_modif$seg_long == ""] <- "0"

# Convert coordinate in degrees to decimal
lat_grau_data_modif <-
  dms2dd(
    as.numeric(grau_data_modif$grau_lat),
    as.numeric(grau_data_modif$min_lat),
    as.numeric(grau_data_modif$seg_lat),
    "S"
  )
long_grau_data_modif <-
  dms2dd(
    as.numeric(grau_data_modif$grau_long),
    as.numeric(grau_data_modif$min_long),
    as.numeric(grau_data_modif$seg_long),
    "W"
  )

coord <- data.frame(lat = as.data.frame(lat_grau_data_modif),
                    long = as.data.frame(long_grau_data_modif))

# Extracting degree and UTM coordinates to remove and later return correct
grau_utm_data_modif_df <- 
  data_modif %>%
  filter(
    str_detect(verbatimLatitude, "[[:alpha:] ]+"),
    decimalLatitude == "" | is.na(decimalLatitude)
  )

utm_data_modif_df <- g
rau_utm_data_modif_df %>%
  filter(verbatimLatitude == "240487949 N")

grau_data_modif_df <-
  anti_join(grau_utm_data_modif_df, utm_data_modif_df)

data_modif <- anti_join(data_modif, grau_data_modif_df)

grau_data_modif_df$decimalLatitude <-
  as.character(coord$lat_grau_data_modif)
grau_data_modif_df$decimalLongitude <-
  as.character(coord$long_grau_data_modif)
data_modif$decimalLatitude <-
  as.character(data_modif$decimalLatitude)
data_modif$decimalLongitude <-
  as.character(data_modif$decimalLongitude)

grau_data_modif_df <-
  grau_data_modif_df %>% 
  select(colnames(data_modif))

data_modif <- bind_rows(data_modif, grau_data_modif_df)

# Convert UTM to decimal degree coordinates
utm_data_modif <- 
  data_modif %>%
  filter(str_detect(geodeticDatum, 'UTM')) %>%
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
    select(verbatimLatitude, verbatimLongitude, decimalLatitude, decimalLongitude)
  df <- bind_rows(x[1, ], df)
}

df <- arrange(df, by = verbatimLatitude)

# Convert UTM to decimal degree http://splink.cria.org.br/conversor
df$decimalLatitude <-
  c(
    '-16.599388',
    '-17.169331',
    '-13.578941',
    '-13.864987',
    '-17.292106',
    '-15.927010',
    '-15.155310',
    '-13.952912',
    '-16.286450',
    '-15.197319',
    '-15.973720',
    '-17.106801',
    '-16.512313',
    '-13.701098',
    '-15.619982',
    '-14.018092',
    '-16.324448',
    '-14.343671',
    '-15.172064',
    '-14.424250',
    '-15.166549',
    '-13.525323'
  )
df$decimalLongitude <-
  c(
    '-39.913983',
    '-39.841776',
    '-39.706685',
    '-39.672635',
    '-39.673031',
    '-39.635847',
    '-39.526954',
    '-39.451138',
    '-39.424079',
    '-39.391085',
    '-39.373680',
    '-39.339753',
    '-39.303612',
    '-39.232629',
    '-39.161263',
    '-39.143283',
    '-39.121001',
    '-39.086907',
    '-39.061124',
    '-39.060414',
    '-39.059754',
    '-39.035311'
  )

utm_data_modif <- merge(utm_data_modif, df, by = 'verbatimLatitude')

# Returnin to main dataframe
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLatitude.y
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <-
  utm_data_modif$decimalLongitude.y

# Correcting mixed latitude/longitude
to_correct_latlong <- data_modif %>% filter(decimalLongitude < -30)

lon <- to_correct_latlong$decimalLatitude
lat <- to_correct_latlong$decimalLongitude

correct_latlong <- to_correct_latlong
correct_latlong$decimalLongitude <- lon
correct_latlong$decimalLatitude <- lat

# Removing wrong rows
data_modif <- anti_join(data_modif, to_correct_latlong)
data_modif <- rbind(data_modif, correct_latlong)


# Manipulate references ----------------------------------------
references_df$reference <- as.character(references_df$reference)
data_modif$reference <- as.character(data_modif$reference)

data_modif <- merge(data_modif, references_df, all = TRUE)

# Remove references
to_remove <- data_modif %>% filter(reference_std == 'removed')
data_modif <- anti_join(data_modif, to_remove)

# assume thesis year, instead of publication year
to_corret_year <- data_modif %>% filter(str_detect(reference, 'Hirsch, A.,'))
data_modif <- anti_join(data_modif, to_corret_year)

to_corret_year$year <- '1988'
data_modif$year <- as.character(data_modif$year)
data_modif <- bind_rows(data_modif, to_corret_year)

to_remove <- data_modif %>% filter(citation == 'Aguirre (1971)', datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Gatti et al. (2014)', year == '1988' | datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Kinzey (1982)', datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Mittermeier et al. (1987)', datasetName == "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Moura (2003)', datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Palma (1996)', datasetName != "")
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Paresque et al. (2004)', datasetName != 'LGA')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Passamani (2000)', datasetName == '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Passamani et al. (2005)', datasetName != '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Passamani & Fernandez (2011)', datasetName == '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Passamani et al. (2000)', datasetName != '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Passamani & Ribeiro (2009)', datasetName == '', scientificName != 'Callithrix geoffroyi', scientificName != 'Sciurus aestuans')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Pinto (1994)', datasetName == '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Santos et al. (2004)', datasetName != '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(citation == 'Santos et al. (1987)', datasetName == '')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% filter(
  citation == 'Soares et al. (2013)',
  scientificName != 'Cerdocyon thous',
  scientificName != 'Cuniculus paca',
  scientificName != 'Dasyprocta leporina',
  scientificName != 'Dasypus novemcinctus',
  scientificName != 'Didelphis aurita',
  scientificName != 'Leopardus wiedii',
  scientificName != 'Lontra longicaudis',
  scientificName != 'Marmosa murina',
  scientificName != 'Mazama americana',
  scientificName != 'Metachirus nudicaudatus',
  scientificName != 'Nasua nasua',
  scientificName != 'Philander opossum',
  scientificName != 'Sciurus aestuans')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% 
  filter(citation == 'Srbek-Araujo & Chiarello (2005)',
         scientificName != 'Didelphis aurita',
         scientificName != 'Metachirus nudicaudatus',
         scientificName != 'Philander frenata', #just second year
         scientificName != 'Dasypus novemcinctus',
         scientificName != 'Euphractus sexcinctus', #just second year
         scientificName != 'Cerdocyon thous',
         scientificName != 'Nasua nasua',
         scientificName != 'Procyon cancrivorus',
         scientificName != 'Eira barbara', #just first year
         scientificName != 'Didelphis aurita',
         scientificName != 'Herpailurus yaguarondi',
         scientificName != 'Leopardus pardalis',
         scientificName != 'Herpailurus yaguarondi',
         scientificName != 'Leopardus tigrinus',
         scientificName != 'Puma concolor',
         scientificName != 'Mazama americana',
         scientificName != 'Pecari tajacu',
         scientificName != 'Sciurus aestuans',
         scientificName != 'Hydrochoerus hydrochaeris',
         scientificName != 'Cuniculus paca',
         scientificName != 'Dasyprocta leporina',
         scientificName != 'Sylvilagus brasiliensis')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% 
  filter(citation == 'Srbek-Araujo & Chiarello (2013)',
         scientificName != 'Didelphis aurita',
         scientificName != 'Metachirus nudicaudatus',
         scientificName != 'Cabassous tatouay',
         scientificName != 'Euphractus sexcinctus',
         scientificName != 'Cerdocyon thous',
         scientificName != 'Tamandua tetradactyla',
         scientificName != 'Procyon cancrivorus',
         scientificName != 'Callithrix geoffroyi',
         scientificName != 'Sapajus robustus',
         scientificName != 'Cerdocyon thous',
         scientificName != 'Galictis cuja',
         scientificName != 'Nasua nasua',
         scientificName != 'Procyon cancrivorus',
         scientificName != 'Eira barbara',
         scientificName != 'Leopardus pardalis',
         scientificName != 'Leopardus wiedii',
         scientificName != 'Panthera onca',
         scientificName != 'Puma concolor',
         scientificName != 'Herpailurus yagouaroundi',
         scientificName != 'Tapirus terrestris',
         scientificName != 'Tayassu pecari',
         scientificName != 'Pecari tajacu',
         scientificName != 'Guerlinguetus ingrami',
         scientificName != 'Hydrochoerus hydrochaeris',
         scientificName != 'Cuniculus paca',
         scientificName != 'Dasyprocta leporina',
         scientificName != 'Sylvilagus brasiliensis')
data_modif <- anti_join(data_modif, to_remove)

to_remove <- data_modif %>% 
  filter(citation == 'Tonini et al. (2010)', datasetName != '')
data_modif <- anti_join(data_modif, to_remove)

to_correct <- data_modif %>% filter(citation == 'Travassos & Freitas (1948)')
correct <- to_correct %>% distinct(scientificName, .keep_all = TRUE)
data_modif <- anti_join(data_modif, to_correct)
data_modif <- bind_rows(data_modif, correct)

# Export clean data.frame --------------------
write_csv(data_modif, '../data/processed-data/clean-papers-data.csv')
