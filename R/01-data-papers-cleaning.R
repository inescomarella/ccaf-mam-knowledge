setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c("tidyverse", "CoordinateCleaner", "lubridate", "biogeo", "sf")
lapply(x, library, character.only = TRUE)

source('functions.R')

# Input
data_raw <- read.csv("../data/data-raw.csv", stringsAsFactors = FALSE)
ccma <-
  st_read(dsn = '../outputs',
          layer = 'ccma-clipped',
          check_ring_dir = TRUE)

# Remove marine mammals
data_modif <- data_raw[!(data_raw$order == "Cetartiodactyla" | 
                           data_raw$order == "Cetacea"), ]

# Remove speciesLink data
to_remove <- data_modif %>% filter_all(any_vars(str_detect(., "speciesLink")))
data_modif <- anti_join(data_modif, to_remove)

# Remove unpublished data
data_modif <- data_modif %>% filter(!str_detect(typeOfPublication, 'Unpubl'))

# Correct column name
colnames(data_modif)[34] <- "scientificName"

# Standardizing collectionCode column ----
data_modif$collectionCode <- as.character(data_modif$collectionCode)

data_modif$collectionCode[data_modif$collectionCode == "Coleção de Mamíferos  do Museu Nacional (MN)"] <- "MN - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o de Mam�feros, Universidade Estadual de Santa Cruz (UESC), Ilh�us, BA, Brazil"] <- "UESC - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ - ALP Collection"
data_modif$collectionCode[data_modif$collectionCode == "Laboratorio de Diversidade de Morcegos (LDM) and Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ -LDM and ALP Collections"
data_modif$collectionCode[data_modif$collectionCode == "Museum of Vertebrate Zoology"] <- "MVZ"
data_modif$collectionCode[data_modif$collectionCode == "Not located"] <- ""

# Standardizing fieldNumber column ----
data_modif$fieldNumber <- as.character(data_modif$fieldNumber)

data_modif$fieldNumber[data_modif$fieldNumber == "?"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Daniela Rossoni"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Valéria Fagundes"] <- " "

# Standardizing preparations column ----
data_modif$preparations <- as.character(data_modif$preparations)

data_modif$preparations[data_modif$preparations == "pele"] <- "Skin"
data_modif$preparations[data_modif$preparations == "skin, study"] <- "Skin"
data_modif$preparations[data_modif$preparations == "skin (dry)"] <- "Skin"
data_modif$preparations[data_modif$preparations == "Pele/ pelo"] <- "Skin"
data_modif$preparations[data_modif$preparations == "espécime inteiro (ETOH)"] <- "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "whole animal (ethanol)"] <- "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "filhotes (ETOH)"] <- "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "whole organism (ethanol)"] <- "Whole animal (ethanol)"
data_modif$preparations[data_modif$preparations == "corpo (ETOH)"] <- "Body (ethanol)"
data_modif$preparations[data_modif$preparations == "esqueleto completo"] <- "Whole skeleton"
data_modif$preparations[data_modif$preparations == "esqueleto parcial"] <- "Partial skeleton"
data_modif$preparations[data_modif$preparations == "Sangue"] <- "Blood"
data_modif$preparations[data_modif$preparations == "sangue"] <- "Blood"
data_modif$preparations[data_modif$preparations == "Carcaça"] <- "Carcass"
data_modif$preparations[data_modif$preparations == "carcaça (ETOH)"] <- "Carcass (ethanol)"
data_modif$preparations[data_modif$preparations == "Carcaça / Fígado"] <- "Carcass/Liver"
data_modif$preparations[data_modif$preparations == "Álcool"] <- "Ethanol"
data_modif$preparations[data_modif$preparations == "embriões (ETOH)"] <- "Embryos (ethanol)"
data_modif$preparations[data_modif$preparations == "crânio"] <- "Skull"
data_modif$preparations[data_modif$preparations == "Álcool e Crânio"] <- "Skull"
data_modif$preparations[data_modif$preparations == "skull"] <- "Skull"
data_modif$preparations[data_modif$preparations == "skull (dry)"] <- "Skull"
data_modif$preparations[data_modif$preparations == "tissue (ethanol)"] <- "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tecido (ETOH)"] <- "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tissue (95% ethanol)"] <- "Tissue (ethanol)"
data_modif$preparations[data_modif$preparations == "tissue (frozen)"] <- "Tissue (frozen)"
data_modif$preparations[data_modif$preparations == "Líquido"] <- "Liquid"
data_modif$preparations[data_modif$preparations == "seca"] <- "Dry"
data_modif$preparations[data_modif$preparations == "Cartilagem"] <- "Cartilage"
data_modif$preparations[data_modif$preparations == "cytogenetic"] <- "Cytogenetic"
data_modif$preparations[data_modif$preparations == "skull; tissue (frozen); skin,"] <- "Skull; tissue (frozen); skin"
data_modif$preparations[data_modif$preparations == "skull; skin, study"] <- "Skull; skin, study"
data_modif$preparations[data_modif$preparations == "fragmento de mandíbula"] <- "Jaw fragment"
data_modif$preparations[data_modif$preparations == "Carapaça"] <- "Hoof"

# Standardizing eventYear column ----
# Correcting format from year/year to year
# Extract wrong lines
data_bar <- data_modif %>% filter(str_detect(eventYear, "[/]")) 

# Extract eventYear column from wrong rows
year_bar <- as.data.frame(data_bar$eventYear)

# Separating first/last years in two columns
year_bar_sep <- separate(data = year_bar, col = 'data_bar$eventYear', into = c("A", "B"), sep = "[/]") 

# Keep last year
year_bar_correct <- data.frame(year_bar_correct = year_bar_sep$B)

# Return corrected column
data_bar$eventYear <- year_bar_correct$year_bar_correct 

# Wrong rows to remove
to_remove <- data_modif %>% filter(str_detect(eventYear, "[/]")) 

# Removing wrong rows
data_bar_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) 

# Add correct rows
data_modif  <- rbind(data_bar_less, data_bar) 

nrow(data_modif)
nrow(data_bar) + nrow(data_bar_less) 

# Standardizing PublicationYear column ----

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
  PublicationYear_less$PublicationYear[i] <- substr(gsub("[^0-9]", "", PublicationYear_less$reference[i]), 1, 4)

data_modif$PublicationYear <- as.character(data_modif$PublicationYear)

# Return rows with publication year added
data_modif <- bind_rows(data_modif, PublicationYear_less)

# Extracting rows without reference
data_modif_reference_less <- data_modif %>% filter(reference == "")

# Standardizing  date in rows without reference
data_modif_reference_less$eventYear <-
  format(as.Date(data_modif_reference_less$eventDate, format = "%m/%d/%y"), "%Y")

# Removing those rows from main dataframe
data_modif <- data_modif %>% filter(!reference == "") 

# Return them with date corrected
data_modif <- rbind(data_modif, data_modif_reference_less) 

# In the absence of collect year, consider the publication year
for (i in 1:nrow(data_modif))
  if (data_modif$eventYear[i] == "" | is.na(data_modif$eventYear[i]))
    data_modif$eventYear[i] <- data_modif$PublicationYear[i]

# country ----
data_modif$country <- "Brazil"

# Stardizing stateProvince column ----
data_modif$stateProvince <- as.character(data_modif$stateProvince)

data_modif$stateProvince[data_modif$stateProvince == "Espírito Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ES"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "Espiríto Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ESPIRITO SANTO"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "BAHIA"] <- "Bahia"
data_modif$stateProvince[data_modif$stateProvince == "BA"] <- "Bahia"

# Standardizing UC column ----
data_modif$UC <- as.character(data_modif$UC)

data_modif$UC[data_modif$UC == "yes"] <- "Yes"
data_modif$UC[data_modif$UC == "no"] <- "No"
data_modif$UC[data_modif$UC == "Parque Estadual da Fonte Grande"] <- "Yes"
data_modif$UC[is.na(data_modif$UC)] <- ""

# Standardizing georeferencePrecision column ----
data_modif$georeferencePrecision <- as.character(data_modif$georeferencePrecision)

data_modif$georeferencePrecision[data_modif$georeferencePrecision == "precise"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "notPrecise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not-Precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "[no data]"] <- ""

# Correct geographical coordinates -----
# Geographical coordinates in geodeticDatum column
for (i in 1:nrow(data_modif))
  if (is.na(data_modif$decimalLatitude[i]))
    data_modif$decimalLatitude[i] <- as.character(data_modif$geodeticDatum[i])

# Remove from geodeticDatum
to_remove <- data_modif %>% 
  select(geodeticDatum) %>% 
  filter(!str_detect(geodeticDatum, "[[:alpha:] ]+")) #filtrar dados sem letras

for (i in 1:nrow(data_modif))
  if (data_modif$geodeticDatum[i] %in% to_remove$geodeticDatum)
    data_modif$geodeticDatum[i] <- NA

# Removing rows without adequate coordinates
to_remove <- data_modif %>% 
  select(decimalLatitude) %>% 
  filter(str_detect(decimalLatitude, "[[:alpha:] ]+"))

for (i in 1:nrow(data_modif))
  if (data_modif$decimalLatitude[i] %in% to_remove$decimalLatitude)
    data_modif$decimalLatitude[i] <- NA

# Extract coordinates in UTM and in degrees
grau_utm_data_modif <- data_modif %>% 
  select(verbatimLatitude, verbatimLongitude) %>% 
  filter(str_detect(verbatimLatitude, "[[:alpha:] ]+"))

# UTM coordinates
utm_data_modif <- data_modif %>% 
  filter(verbatimLatitude == "240487949 N")

# Degree coordinates
grau_data_modif <- grau_utm_data_modif %>% 
  filter(!verbatimLatitude %in% utm_data_modif$verbatimLatitude)

# Preparing columns to convert to decimal degrees
# Separate degree
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
grau_data_modif$grau_lat <- str_replace_all(grau_data_modif$grau_lat, "[[:alpha:] ]+", "")
grau_data_modif$grau_long <- str_replace_all(grau_data_modif$grau_long, "[[:alpha:] ]+", "")

# Correct some specific rows
grau_data_modif$seg_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "20"
grau_data_modif$min_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "16"
grau_data_modif$grau_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "18"

grau_data_modif$seg_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "55"
grau_data_modif$min_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "48"
grau_data_modif$grau_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "39"

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

# Rertuning coordinates degrees corrected to main dataframe
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% grau_data_modif$verbatimLatitude] <- coord$lat_grau_data_modif
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% grau_data_modif$verbatimLatitude] <- coord$long_grau_data_modif

# Removing rows without coordinates
to_remove <-
  data_modif %>% filter(
    verbatimLatitude == "", 
    verbatimLongitude == "",
    is.na(decimalLatitude),
    is.na(decimalLongitude)
  )
data_modif <- anti_join(data_modif, to_remove)

# Convert UTM to decimal degree coordinates
utm_data_modif <- data_modif %>% 
  select(verbatimLatitude, verbatimLongitude, decimalLatitude, decimalLongitude, geodeticDatum) %>% 
  filter(is.na(decimalLatitude) & is.na(decimalLongitude))

utm <- utm_data_modif %>% select(verbatimLatitude, verbatimLongitude)

# Convert UTM to decimal degree http://www.dpi.inpe.br/calcula/
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[2]] <- '-13.52529945007'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[2]] <- '-39.035338180018'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[3]] <- '-13.701074292447'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[3]] <- '-39.23265636598'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[4]] <- '-13.578916617967'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[4]] <- '-39.706712069419'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[5]] <- '-14.018067512186'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[5]] <- '-39.143309618845'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[6]] <- '-13.952887397248'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[6]] <- '-39.451164678044'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[7]] <- '-13.864962356948'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[7]] <- '-39.672662266565'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[8]] <- '-14.4242257748'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[8]] <- '-39.060440860355'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[9]] <- '-14.343646782334'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[9]] <- '-39.086933586738'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[10]] <- '-15.166524911197'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[10]] <- '-39.059781006338'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[11]] <- '-15.197294397659'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[11]] <- '-39.391112215309'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[12]] <- '-39.526980972912'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[12]] <- '-15.155285086932'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[13]] <- '-15.172039617868'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[13]] <- '-39.061151005409'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[14]] <- '-39.161289926413'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[14]] <- '-15.619957524626'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[15]] <- '-15.97369495811'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[15]] <- '-39.373706923518'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[16]] <- '-15.926985134731'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[16]] <- '-39.635874022237'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[17]] <- '-16.324422945368'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[17]] <- '-39.121028294735'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[18]] <-  '-16.286425392045'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[18]] <- '-39.424106517465'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[19]] <- '-16.599363511356'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[19]] <- '-39.914010388505'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[20]] <- '-17.106776328205'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[20]] <- '-39.339780321709'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[21]] <- '-17.292080472538'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[21]] <- '-39.673058656741'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[22]] <- '-17.169305566935'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[22]] <- '-39.841802660895'
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[23]] <- '-16.512288600716'
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[23]] <- '-39.30363966433'
utm_data_modif <- utm_data_modif[utm_data_modif$verbatimLatitude != unique(utm$verbatimLatitude)[1],]

# Returnin to main dataframe
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLatitude
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLongitude

# Removing rows without coordinates
data_modif <- data_modif %>% filter(!is.na(decimalLatitude)) 

to_remove <- data_modif %>% filter(decimalLatitude == "")
data_modif <- anti_join(data_modif, to_remove)

# Correcting mixed latitude/longitude ------
to_correct_latlong <- data_modif %>% filter(decimalLongitude < -30)
lon <- to_correct_latlong$decimalLatitude
lat <- to_correct_latlong$decimalLongitude
correct_latlong <- to_correct_latlong
correct_latlong$decimalLongitude <- lon
correct_latlong$decimalLatitude <- lat

# Removing wrong rows
data_modif <- anti_join(data_modif, to_correct_latlong)

# Adding corrected rows
data_modif <- rbind(data_modif, correct_latlong)

# Standardizing scientificName column ----
# Removing special characters
data_modif$scientificName <- str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

data_modif$scientificName <- as.character(data_modif$scientificName)

# Correcting 'Gender.species'
data_modif$scientificName <- gsub("[.]", " ", data_modif$scientificName)

# Standardizing writting
data_modif$scientificName[data_modif$scientificName == "Alouatta fusca "] <- "Alouatta fusca"
data_modif$scientificName[data_modif$scientificName == "Sapajus  robustus"] <- "Sapajus robustus"
data_modif$scientificName[data_modif$scientificName == "Tamanduá tetradactyla"] <- "Tamandua tetradactyla"
data_modif$scientificName[data_modif$scientificName == "Eira barbara "] <- "Eira barbara"
data_modif$scientificName[data_modif$scientificName == "Mazama  americana "] <- "Mazama americana"
data_modif$scientificName[data_modif$scientificName == "Dicotyles labiatus "] <- "Dicotyles labiatus"
data_modif$scientificName[data_modif$scientificName == "Sapajus  robustus "] <- "Sapajus robustus"
data_modif$scientificName[data_modif$scientificName == "Panthera onca "] <- "Panthera onca"
data_modif$scientificName[data_modif$scientificName == "Phantera onca"] <- "Panthera onca"
data_modif$scientificName[data_modif$scientificName == "Oligoryzomys gr  nigripes"] <- "Oligoryzomys gr. nigripes"
data_modif$scientificName[data_modif$scientificName == "Akodon gr  cursor"] <- "Akodon gr. cursor"
data_modif$scientificName[data_modif$scientificName == "Rhipidomys cf  mastacalis"] <- "Rhipidomys cf. mastacalis"
data_modif$scientificName[data_modif$scientificName == "Dasyprocta aguti"] <- "Dasyprocta agouti"
data_modif$scientificName[data_modif$scientificName == "Didelphis aurita "] <- "Didelphis aurita"
data_modif$scientificName[data_modif$scientificName == "Galictis vittata "] <- "Galictis vittata"
data_modif$scientificName[data_modif$scientificName == "Galictis vitatta"] <- "Galictis vittata"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yaguarond"] <- "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yaguarondi"] <- "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Herpailurus yagouarondi"] <- "Herpailurus yagouaroundi"
data_modif$scientificName[data_modif$scientificName == "Bradypus torquatus "] <- "Bradypus torquatus"
data_modif$scientificName[data_modif$scientificName == "Brachyteles hypoxanthus "] <- "Brachyteles hypoxanthus"
data_modif$scientificName[data_modif$scientificName == "Callithrix kuhli"] <- "Callithrix kuhlii"
data_modif$scientificName[data_modif$scientificName == "Cavis fulgida"] <- "Cavia fulgida"
data_modif$scientificName[data_modif$scientificName == "Diaemus youngi"] <- "Diaemus youngii"
data_modif$scientificName[data_modif$scientificName == "Euryoryzomys russatus"] <- "Euryoryzomys russatus"
data_modif$scientificName[data_modif$scientificName == "Hydrochaeris hydrochaeris"] <- "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrochaeris "] <- "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrochaeris"] <- "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Hydrochoerus hydrocharis"] <- "Hydrochoerus hydrochaeris"
data_modif$scientificName[data_modif$scientificName == "Leontophitecus chrysomelas"] <- "Leontopithecus chrysomelas"
data_modif$scientificName[data_modif$scientificName == "Mazama gouazoupira"] <- "Mazama gouazoubira"
data_modif$scientificName[data_modif$scientificName == "Mazama guazoubira"] <- "Mazama gouazoubira"
data_modif$scientificName[data_modif$scientificName == "Metachirus nudicaudatus "] <- "Metachirus nudicaudatus"
data_modif$scientificName[data_modif$scientificName == "Myrmecophaga tridactyla "] <- "Myrmecophaga tridactyla"
data_modif$scientificName[data_modif$scientificName == "Necromys laisiurus"] <- "Necromys lasiurus"
data_modif$scientificName[data_modif$scientificName == "Olygoryzomys nigripes"] <- "Oligoryzomys nigripes"
data_modif$scientificName[data_modif$scientificName == "Oryzomys laticeps "] <- "Oryzomys laticeps"
data_modif$scientificName[data_modif$scientificName == "Peropteryx cf  kappleri"] <- "Peropteryx cf. kappleri"
data_modif$scientificName[data_modif$scientificName == "Philander frenata"] <- "Philander frenatus"
data_modif$scientificName[data_modif$scientificName == "Priodontes maximus "] <- "Priodontes maximus"
data_modif$scientificName[data_modif$scientificName == "Procyon cancrivoros"] <- "Procyon cancrivorus"
data_modif$scientificName[data_modif$scientificName == "Tapirus terrestris "] <- "Tapirus terrestris"
data_modif$scientificName[data_modif$scientificName == "Trinomys miraptanga"] <- "Trinomys mirapitanga"
data_modif$scientificName[data_modif$scientificName == "Calicebus personatus"] <- "Callicebus personatus"
data_modif$scientificName[data_modif$scientificName == "Guerlinguetus  aestuans"] <- "Guerlinguetus aestuans"
data_modif$scientificName[data_modif$scientificName == "Micoureus  paraguayanus"] <- "Micoureus paraguayanus"
data_modif$scientificName[data_modif$scientificName == "Sapajus libidinosus libidinosus"] <- "Sapajus libidinosus"
data_modif$scientificName[data_modif$scientificName == "Peropteryx cf. kappleri"] <- "Peropteryx kappleri"
data_modif$scientificName[data_modif$scientificName == "Peropteryx trinitatis trinitatis"] <- "Peropteryx trinitatis"
data_modif$scientificName[data_modif$scientificName == "Sciurus alphonsei alphonsei"] <- "Sciurus alphonsei"

# Remove points outside CCMA
data_modif_clipped <- clip.ccma(data_modif)

# Output -----
write_csv(data_modif_clipped, '../data/papers-mamm-clipped.csv')

