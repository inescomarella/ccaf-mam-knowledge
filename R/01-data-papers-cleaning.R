x <- c("tidyverse", "CoordinateCleaner", "lubridate", "biogeo")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Input
data_raw <- read.csv("data-raw.csv", stringsAsFactors = FALSE)

# Primeira limpeza ----
# Retirando registros marinhos
data_modif <- data_raw[!(data_raw$order == "Cetartiodactyla" | data_raw$order == "Cetacea"), ]

# Retirando registros do speciesLink
to_remove <- data_modif %>% filter_all(any_vars(str_detect(., "speciesLink")))
data_modif <- anti_join(data_modif, to_remove)

# Retirando registros não publicados
data_modif <- data_modif %>% filter(!str_detect(typeOfPublication, 'Unpubl'))

# Corrigindo nome da coluna
colnames(data_modif)[34] <- "scientificName"

# collectionCode -----
# Compatibilizando o formato
data_modif$collectionCode <- as.character(data_modif$collectionCode)

# Padronizando nomes
data_modif$collectionCode[data_modif$collectionCode == "Coleção de Mamíferos  do Museu Nacional (MN)"] <- "MN - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o de Mam�feros, Universidade Estadual de Santa Cruz (UESC), Ilh�us, BA, Brazil"] <- "UESC - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ - ALP Collection"
data_modif$collectionCode[data_modif$collectionCode == "Laboratorio de Diversidade de Morcegos (LDM) and Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ -LDM and ALP Collections"
data_modif$collectionCode[data_modif$collectionCode == "Museum of Vertebrate Zoology"] <- "MVZ"
data_modif$collectionCode[data_modif$collectionCode == "Not located"] <- ""

# Compatibilizando o formato
data_modif$fieldNumber <- as.character(data_modif$fieldNumber)

# Padronizando coluna
data_modif$fieldNumber[data_modif$fieldNumber == "?"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Daniela Rossoni"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Valéria Fagundes"] <- " "

# Compatibilizando o formato
data_modif$preparations <- as.character(data_modif$preparations)

# Padronizando coluna
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

# eventYear----

# Corrigindo eventYear no formato ano-mes-dia
# separando as linhas erradas
data_trace <- data_modif %>% filter(str_detect(eventYear, "[-]")) 

# separando a coluna eventYear
year_trace <- data_trace$eventYear 
year_trace <- as.data.frame(year_trace)

# extraindo o ano das data_modif
year_trace_sep <- format(as.Date(year_trace$year_trace, format = "%Y-%m-%d"), "%Y") 
year_trace_sep <- as.data.frame(year_trace_sep)

# retornando os anos corrigidos para a coluna
data_trace$eventYear <- year_trace_sep$year_trace_sep 

# separando as linhas erradas
to_remove <- data_modif %>% filter(str_detect(eventYear, "[-]")) 

# removendo as linhas erradas
data_trace_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) 

# adicionando as linhas corrigidas
data_modif  <- rbind(data_trace_less, data_trace) 

# confirmando os tamanhos das tabelas
nrow(data_modif)
nrow(data_trace_less) + nrow(data_trace) 

# Corrigindo eventYear no formato ano/ano
# separando as linhas erradas
data_bar <- data_modif %>% filter(str_detect(eventYear, "[/]")) 

# separando a coluna eventYear
year_bar <- data_bar$eventYear 
year_bar <- as.data.frame(year_bar)

# separando primeiro/ultimo ano em duas colunas
year_bar_sep <- separate(data = year_bar, col = year_bar, into = c("A", "B"), sep = "[/]") 

# retirando o primeiro ano
year_bar_correct <- year_bar_sep$B 
year_bar_correct <- as.data.frame(year_bar_correct)

# retornando os anos corrigidos para a coluna
data_bar$eventYear <- year_bar_correct$year_bar_correct 

# separando as linhas erradas
to_remove <- data_modif %>% filter(str_detect(eventYear, "[/]")) 

# removendo as linhas erradas
data_bar_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) 

# adicionando as linhas corrigidas
data_modif  <- rbind(data_bar_less, data_bar) 

# confirmando os tamanhos das tabelas
nrow(data_modif)
nrow(data_bar) + nrow(data_bar_less) 

# PublicationYear ----
# Primeiro separa os registros com referencia e sem o ano
PublicationYear_less <- data_modif %>% filter(is.na(PublicationYear) & is.na(reference) == FALSE & reference != "" |
                                                PublicationYear == "" & is.na(reference) == FALSE & reference != "")

# Retira esses registros da tabela
data_modif <- anti_join(data_modif, PublicationYear_less)

# Adiciona o ano (assumindo que os primeiros 4 numeros da referência sempre serão o ano de publicação)
for (i in 1:nrow(PublicationYear_less))
  PublicationYear_less$PublicationYear[i] <- substr(gsub("[^0-9]", "", PublicationYear_less$reference[i]), 1, 4)

# Compatibilizando o formato a coluna
data_modif$PublicationYear <- as.character(data_modif$PublicationYear)

# Retornando os registros com o ano de publicação adicionado
data_modif <- bind_rows(data_modif, PublicationYear_less)

# Separando registros sem referencia da tabela principal
data_modif_reference_less <- data_modif %>% filter(reference == "")

# Padronizando a data dos registros sem referencia
data_modif_reference_less$eventYear <- format(as.Date(data_modif_reference_less$eventDate, format = "%m/%d/%y"), "%Y") 

# removendo registros sem referência da tabela principal
data_modif <- data_modif %>% filter(!reference == "") 

# retornando os registros sem referencia com as datas corrigidas
data_modif <- rbind(data_modif, data_modif_reference_less) 

# Na ausência de ano de coleta, considerar o ano de publicação
for (i in 1:nrow(data_modif))
  if (data_modif$eventYear[i] == "" | is.na(data_modif$eventYear[i]))
    data_modif$eventYear[i] <- data_modif$PublicationYear[i]

# Removendo registros sem ano 
data_modif <- data_modif %>% filter(!eventYear == "") # removendo registros

# country -----
data_modif$country <- "Brazil"

# stateProvince -----
data_modif$stateProvince <- as.character(data_modif$stateProvince)

# Padronizando preenchimento da coluna
data_modif$stateProvince[data_modif$stateProvince == "Espírito Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ES"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "Espiríto Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ESPIRITO SANTO"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "BAHIA"] <- "Bahia"
data_modif$stateProvince[data_modif$stateProvince == "BA"] <- "Bahia"

# UC ----
data_modif$UC <- as.character(data_modif$UC)

# Padronizando preenchimento da coluna
data_modif$UC[data_modif$UC == "yes"] <- "Yes"
data_modif$UC[data_modif$UC == "no"] <- "No"
data_modif$UC[data_modif$UC == "Parque Estadual da Fonte Grande"] <- "Yes"
data_modif$UC[is.na(data_modif$UC)] <- ""

# georeferencePrecision ----
data_modif$georeferencePrecision <- as.character(data_modif$georeferencePrecision)

# Padronizando preenchimento da coluna
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "precise"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "notPrecise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not-Precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "[no data]"] <- ""

# Coordenadas geográficas -----
# Passando coordenadas que estão na coluna errada para a coluna correta
for (i in 1:nrow(data_modif))
  if (is.na(data_modif$decimalLatitude[i]))
    data_modif$decimalLatitude[i] <- as.character(data_modif$geodeticDatum[i])

# Retirando as coordenadas que estão na coluna errada
to_remove <- data_modif %>% 
  select(geodeticDatum) %>% 
  filter(!str_detect(geodeticDatum, "[[:alpha:] ]+")) #filtrar dados sem letras

for (i in 1:nrow(data_modif))
  if (data_modif$geodeticDatum[i] %in% to_remove$geodeticDatum)
    data_modif$geodeticDatum[i] <- NA

# Retirando os dados errado da coluna certa
to_remove <- data_modif %>% 
  select(decimalLatitude) %>% 
  filter(str_detect(decimalLatitude, "[[:alpha:] ]+"))

for (i in 1:nrow(data_modif))
  if (data_modif$decimalLatitude[i] %in% to_remove$decimalLatitude)
    data_modif$decimalLatitude[i] <- NA

# Separando as coordenadas em graus e UTM
grau_utm_data_modif <- data_modif %>% 
  select(verbatimLatitude, verbatimLongitude) %>% 
  filter(str_detect(verbatimLatitude, "[[:alpha:] ]+"))

# Retirado as coordenadas em UTM
utm_data_modif <- data_modif %>% 
  filter(verbatimLatitude == "240487949 N") # separando coordenadas em UTM
grau_data_modif <- grau_utm_data_modif %>% 
  filter(!verbatimLatitude %in% utm_data_modif$verbatimLatitude) # removendo as linhas com coordenadas em UTM

# Preparando as colunas para converter de graus para decimais

# Separado grau
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

# Retirando caracteres especiais
grau_data_modif$verbatimLongitude <-
  str_replace_all(grau_data_modif$verbatimLongitude, "[[:alpha:] ]+", "")
grau_data_modif$verbatimLatitude <-
  str_replace_all(grau_data_modif$verbatimLatitude, "[[:alpha:] ]+", "")

# Separando minuto e segundo
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

# Retirando caracteres especiais
grau_data_modif$grau_lat <- str_replace_all(grau_data_modif$grau_lat, "[[:alpha:] ]+", "")
grau_data_modif$grau_long <- str_replace_all(grau_data_modif$grau_long, "[[:alpha:] ]+", "")

# Corrigindo alguns erros pontuais
grau_data_modif$seg_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "20"
grau_data_modif$min_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "16"
grau_data_modif$grau_lat[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "18"

grau_data_modif$seg_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "55"
grau_data_modif$min_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "48"
grau_data_modif$grau_long[grau_data_modif$grau_lat == unique(grau_data_modif$grau_lat)[2]] <- "39"

grau_data_modif$seg_lat[grau_data_modif$seg_lat == ""] <- "0"
grau_data_modif$seg_long[grau_data_modif$seg_long == ""] <- "0"

# Convertendo as coordenadas em graus para UTM
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

# Passando para a tabela de trabalho
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% grau_data_modif$verbatimLatitude] <- coord$lat_grau_data_modif
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% grau_data_modif$verbatimLatitude] <- coord$long_grau_data_modif

# Removendo dados sem registro de coordenada geográfica
to_remove <-
  data_modif %>% filter(
    verbatimLatitude == "", 
    verbatimLongitude == "",
    is.na(decimalLatitude),
    is.na(decimalLongitude)
  )
data_modif <- anti_join(data_modif, to_remove)

# Convertendo UTM para coordenadas decimais
utm_data_modif <- data_modif %>% 
  select(verbatimLatitude, verbatimLongitude, decimalLatitude, decimalLongitude, geodeticDatum) %>% 
  filter(is.na(decimalLatitude) & is.na(decimalLongitude))

utm <- utm_data_modif %>% select(verbatimLatitude, verbatimLongitude)

# Os calculos das coordenadas foram feitar num site (acho que IBGE)
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[2]] <- "-16.324448"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[2]] <- "-39.121001"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[3]] <- "-14.018092"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[3]] <- "-39.143283"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[4]] <- "-17.106801"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[4]] <- "-39.339753"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[5]] <- "-14.424250"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[5]] <- "-39.060414"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[6]] <- "-13.525323"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[6]] <- "-39.035311"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[7]] <- "-15.172064"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[7]] <- "-39.061124"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[8]] <- "-17.169331"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[8]] <- "-39.841776"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[9]] <- "-15.155310"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[9]] <- "-39.526954"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[10]] <- "-15.927010"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[10]] <- "-39.635847"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[11]] <- "-17.292106"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[11]] <- "-39.673031"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[12]] <- "-13.578941"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[12]] <- "-39.706685"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[13]] <- "-15.619982"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[13]] <- "-39.161263"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[14]] <- "-15.973720"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[14]] <- "-39.373680"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[15]] <- "-16.512313"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[15]] <- "-39.303612"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[16]] <- "-13.701098"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[16]] <- "-39.232629"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[17]] <- "-15.166549"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[17]] <- "-39.059754"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[18]] <- "-13.952912"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[18]] <- "-39.451138"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[19]] <- "-16.599388"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[19]] <- "-39.913983"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[20]] <- "-13.864987"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[20]] <- "-39.672635"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[21]] <- "-16.286450"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[21]] <- "-39.424079"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[22]] <- "-15.197319"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[22]] <- "-39.391085"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[23]] <- "-14.343671"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(utm$verbatimLatitude)[23]] <- "-39.086907"

# Passando para a tabela de trabalho
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLatitude
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLongitude

# Removendo registros sem coordenadas geográfica
data_modif <- data_modif %>% filter(!is.na(decimalLatitude)) 

to_remove <- data_modif %>% filter(decimalLatitude == "")
data_modif <- anti_join(data_modif, to_remove)

# Corrigindo latitude/longitude trocada ------

# Separando dados trocados
to_correct_latlong <- data_modif %>% filter(decimalLongitude < -30)
long <- to_correct_latlong$decimalLatitude
lat <- to_correct_latlong$decimalLongitude

# Corrigindo a troca
correct_latlong <- to_correct_latlong
correct_latlong$decimalLatitude <- lat
correct_latlong$decimalLongitude <- long

# Removendo registros com os dados trocados
data_modif <- anti_join(data_modif, to_correct_latlong)

# Adicionando registros com os dados corrigidos
data_modif <- rbind(data_modif, correct_latlong)

# scientificName ----
# Removendo caracteres especiais
data_modif$scientificName <- str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

data_modif$scientificName <- as.character(data_modif$scientificName)

# Corrigindo nome científico de espécie tipo 'Genero.epiteto'
data_modif$scientificName <- gsub("[.]", " ", data_modif$scientificName)

# Padronizando a escrita
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

# Output -----

# Exportando tabela padronizada
write_csv(data_modif, 'data-papers.csv')

