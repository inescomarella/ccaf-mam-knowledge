######################### 
### Para fazer ##########

#2. Padronizar as cidades
#3. Padronizar as reservas
#
##########################

library(tidyverse)
library(CoordinateCleaner)
library(lubridate)
library(biogeo)

setwd('./data')

# Base de dados
data_raw <- read.csv("data-raw.csv")
glimpse(data_raw)

# retirando registros marinhos
data_modif <- data_raw[!(data_raw$order == "Cetartiodactyla" | data_raw$order == "Cetacea"), ]

# basisOfRecord -----
unique(data_modif$basisOfRecord)
filter(data_modif, data_modif$basisOfRecord == "S")

# datasetName -----
unique(data_modif$datasetName)

# Removendo caracteres especiais para evitar bugs
data_modif$data_modifetName <- str_replace_all(data_modif$datasetName, "[^[:alnum:]]", "")

# Rremovendo registros desse dataset pois contém muitos registros errados
data_modif <- data_modif[!(str_detect(data_modif$datasetName, "Lima  F   Beca  G   Muylaert  R L   Jenkins  C N   Perilli  M L L   Paschoal  A M O   Massara  R L   Paglia  A P   Chiarello  A G   Graipel  M E   Cherem  J J   Regolin  A L   Oliveira Santos  L G R   Brocardo  C R   Paviolo  A   Di Bitetti  M S   Scoss  L M   Rocha  F L   Fusco Costa  R   Rosa  C A   Da Silva  M X   Hufnagell  L   Santos  P M   Duarte  G T   Guimarães  L N   Bailey  L L   Rodrigues  F H G   Cunha  H M   Fantacini  F M   Batista  G O   Bogoni  J A   Tortato  M A   Luiz  M R   Peroni  N   De Castilho  P V   Maccarini  T B   Filho  V P   Angelo  C D   Cruz  P   Quiroga  V   Iezzi  M E   Varela  D   Cavalcanti  S M C   Martensen  A C   Maggiorini  E V   Keesen  F F   Nunes  A V   Lessa  G M   Cordeiro Estrela  P   Beltrão  M G   De Albuquerque  A C F   Ingberman  B   Cassano  C R   Junior  L C   Ribeiro  M C  and Galetti  M   2017   ATLANTIC CAMTRAPS  a data_modifet of medium and large terrestrial mammal communities in the Atlantic Forest of South America  Ecology  98  2979 2979  doi 10 1002 ecy 1998")), ] 

# language -----
unique(data_modif$language)

# institutionCode -----
unique(data_modif$institutionCode)

# Mudando formato para evitar bug
data_modif$institutionCode <- as.character(data_modif$institutionCode)

# Padronizando para sigla
data_modif$institutionCode[data_modif$institutionCode == "Berkeley Natural History Museum"] <- "BNHM"

# collectionCode -----
unique(data_modif$collectionCode)

# Mudando formato para evitar bug
data_modif$collectionCode <- as.character(data_modif$collectionCode)

# Padronizando nomes
data_modif$collectionCode[data_modif$collectionCode == "Coleção de Mamíferos  do Museu Nacional (MN)"] <- "MN - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o de Mam�feros, Universidade Estadual de Santa Cruz (UESC), Ilh�us, BA, Brazil"] <- "UESC - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ - ALP Collection"
data_modif$collectionCode[data_modif$collectionCode == "Laboratorio de Diversidade de Morcegos (LDM) and Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ -LDM and ALP Collections"
data_modif$collectionCode[data_modif$collectionCode == "Museum of Vertebrate Zoology"] <- "MVZ"
data_modif$collectionCode[data_modif$collectionCode == "Not located"] <- ""

#data_modif$collectionCode <- str_replace_all(data_modif$collectionCode, "[^[:alnum:]]", " ") #substituindo caracteres especiais por espaço

# reference -----
unique(data_modif$reference)

# Mudando formato para evitar bug
data_modif$reference <- as.character(data_modif$reference)

# Padronizando preenchimento da coluna
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Sérgio Fernandes Freitas"] <- "S. F. Freitas, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Sheila Rancura"] <- "S. Rancura, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Osmar Borges"] <- "O. Borges, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Eliton Silva"] <- "E. Silva, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Leonardo Brasil de Matos Nunes."] <- "L. B. de M. Nunes, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento:  LEONY WAND DEL REY DE OLIVEIRA"] <- "Leony Wand Del Rey Oliveira answered a questionnaire about records of the presence of mammals in the Conservation Unit"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Waldomiro de Paula Lopes."] <- "W. de P. Lopes, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento:  Eliton de Almeida Lima"] <- "E. de A. L. Oliveira, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: OSMAR BARRETO BORGES"] <- "O. B. Borges, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para a Unidade de Conservação. Responsável pelo preenchimento: Alfredo Antonio Neto"] <- "A. A. Neto, personal communication"
data_modif$reference[data_modif$reference == "Foi enviado um questionário sobre registros de presença de mamíferos para os pesquisadores. Informações de: Priscila Suscke Gouveia. Bióloga USP. psuscke@hotmail.com"] <- "P. S. G., personal communication"
data_modif$reference[data_modif$reference == "Sergio Lucena Mendes, pesquisados da UFES. slmendes1@gmail.com"] <- "S. L. Mendes, personal communication"
data_modif$reference[data_modif$reference == "Romari Martinez, pesquisadora da UESC. cebus@yahoo.com"] <- "R. Martinez, personal communication"
data_modif$reference[data_modif$reference == "Rita de Cássia Bianchi. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Universidade Estadual Paulista \"Júlio de Mesquita Filho\", Jaboticabal, SP."] <- "R. de C. Bianchi, personal communication, 2012"
data_modif$reference[data_modif$reference == "Reserva Biológica do Córrego do Veado. Gabriel Fernando Rezende. 2012. Entrevista feita com o pesquisador por intermédio de questionário. ICMBIO."] <- "G. F. Rezende, personal communication, 2012"
data_modif$reference[data_modif$reference == "Renato Richard Hilário. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Universidade Federal da Paraíba. renatohilario@gmail.com."] <- "R. R. Hilário, personal communication, 2012"
data_modif$reference[data_modif$reference == "Diego Rocha. Foi segurança da Garra, terceirizada pela Fibria S.A., e acompanhava a equipe do Pró-Tapir na RPPN Recanto das Antas, Linhares, ES. "] <- "D. Rocha, personal communication"
data_modif$reference[data_modif$reference == "Danielle de Oliveira Moreira, pesquisadora do Laboratorio de Biologia da Conservacao de Vertebrados da UFES."] <- "D. de O. Moreira, personal communication"
data_modif$reference[data_modif$reference == "Bruno Marchena Romão Tardio. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Instituto Chico Mendes de Conservação da Biodiversidade - Rebio de Uma. bruno.tardio@icmbio.gov.br."] <- "B. M. R. Tardio, personal communication, 2012"
data_modif$reference[data_modif$reference == "Andressa Gatti. 2011. Entrevista feita com o pesquisador por intermédio de questionário. Pró-Tapir - ES."] <- "A. Gatti, personal communication, 2011"
data_modif$reference[data_modif$reference == "Ana Carolina Loss. UFES. Respondeu ao questionario sobre mamiferos na Mata Atlantica"] <- "A. C. Loss, personal communication"
data_modif$reference[data_modif$reference == "Adriano Paglia. 2011. Entrevista feita com o pesquisador por intermédio de questionário. UFMG."] <- "A. Paglia, personal communication, 2011"
data_modif$reference[data_modif$reference == "Adriana C. Colosio. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Instituto Baelia Jubarte. adriana.colosio@baleiajubarte.org.br"] <- "A. C. Colosio, personal communication, 2012"
data_modif$reference[data_modif$reference == "Leonardo Neves. IESB, BA, respondeu ao questionario enviado sobre mamiferos da Mata Atlantica. lgneves@yahoo.com."] <- "L. Neves, personal communication"
data_modif$reference[data_modif$reference == "Karoline Luiza Sarges Marques. Ufes, Projeto Muriqui, ES, respondeu ao questionario enviado sobre mamiferos da Mata Atlantica. kakasarges@gmail.com."] <- "K. L. S. Marques, personal communication"
data_modif$reference[data_modif$reference == "Barbara Regina Neves Chaves. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Universidade Federal de Minas Gerais (UFMG)."] <- "B. R. N. Chaves, personal communication, 2012"
data_modif$reference[data_modif$reference == "Fernando Moreira Flores. 2012. Entrevista feita com o pesquisador por intermédio de questionário. E-mail: nandomoreiraf@gmail.com"] <- "F. M. Flores, personal communication, 2012"
data_modif$reference[data_modif$reference == "Helimar Rabello. 2012. Entrevista feita com o pesquisador por intermédio de questionário. Sao Camilo ES e Habitatil Consultoria, Biólogo. helimarbio@hotmail.com"] <- "H. Rabello, personal communication, 2012"
data_modif$reference[data_modif$reference == "Jardel Brandão Seibert. Ufes, Pro-Tapir, ES, respondeu ao questionario enviado sobre mamiferos da Mata Atlantica. jardelseibert@gmail.com."] <- "J. B. Seibert, personal communication"
data_modif$reference[data_modif$reference == "Fábio Falcão. 2011. Entrevista feita com o pesquisador por intermédio de questionário. UFBA."] <- "F. Falcão, personal communication, 2011"

# typeOfPublication -----
unique(data_modif$typeOfPublication)

# Padronizando coluna
data_modif$typeOfPublication[data_modif$typeOfPublication == "Unpublish"] <- "Unpublished"

# catalogNumber -----
unique(data_modif$catalogNumber)

# recordedBy -----
unique(data_modif$recordedBy)

# fieldNumber -----
unique(data_modif$fieldNumber)

# Mudando formato para evitar bug
data_modif$fieldNumber <- as.character(data_modif$fieldNumber)

# Padronizando coluna
data_modif$fieldNumber[data_modif$fieldNumber == "?"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Daniela Rossoni"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Valéria Fagundes"] <- " "

# preparations -----
unique(data_modif$preparations)

# Mudando formato para evitar bug
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

# associatedReferences -----
unique(data_modif$associatedReferences)
str(data_modif$associatedReferences)

# eventDate -----
unique(data_modif$eventDate)

# eventYear (1/2) ----
unique(data_modif$eventYear)

# Corrigindo as data_modif no formato ano-mes-dia

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

# Corrigindo as data_modif no formato ano/ano
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

# country -----
unique(data_modif$country)
data_modif$country <- "Brazil"

# stateProvince -----
unique(data_modif$stateProvince)
data_modif$stateProvince <- as.character(data_modif$stateProvince)

# Padronizando preenchimento da coluna
data_modif$stateProvince[data_modif$stateProvince == "Espírito Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ES"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "Espiríto Santo"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "ESPIRITO SANTO"] <- "Espirito Santo"
data_modif$stateProvince[data_modif$stateProvince == "BAHIA"] <- "Bahia"
data_modif$stateProvince[data_modif$stateProvince == "BA"] <- "Bahia"

# TERMINAR county -----
unique(data_modif$county)

# UC ----
unique(data_modif$UC)
data_modif$UC <- as.character(data_modif$UC)

# Padronizando preenchimento da coluna
data_modif$UC[data_modif$UC == "yes"] <- "Yes"
data_modif$UC[data_modif$UC == "no"] <- "No"
data_modif$UC[data_modif$UC == "Parque Estadual da Fonte Grande"] <- "Yes"
data_modif$UC[is.na(data_modif$UC)] <- ""

# georeferencePrecision ----
unique(data_modif$georeferencePrecision)
data_modif$georeferencePrecision <- as.character(data_modif$georeferencePrecision)

# Padronizando preenchimento da coluna
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "precise"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "notPrecise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not-Precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "[no data]"] <- ""

# scientificName ----
# corrigindo o nome da coluna 'scientificName'
col_names <- colnames(data_modif)
col_names[34] <- "scientificName"
colnames(data_modif) <- col_names

# Removendo caracteres especiais
data_modif$scientificName <- str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

data_modif$scientificName <- as.character(data_modif$scientificName)

# Corrigindo nome científico de espécie tipo 'Genero.epiteto'
data_modif$scientificName <- gsub("[.]", " ", data_modif$scientificName)

# Removendo registros sem espécie
data_modif <- data_modif[!is.na(data_modif$scientificName), ] 
data_modif <- data_modif[!(str_detect(data_modif$scientificName, "sp.")), ] # ou só a nível de gênero

# Registros para sere removidos com base na espécie
to_remove <- c("DD", 
               "EN", 
               "NT", 
               "LC",
               "Eptesicus",
               "Thrichomys", 
               "Echimys",
               "Rhipidomys",
               "Philander",
               "Oxymycterus?",
               "Oecomys",
               "Necromys",
               "Nectomys",
               "Metachirus",
               "Marmosops?",
               "Lonchophylla",  
               "Euryoryzomys", 
               "Coendou", 
               "Cerradomys", 
               "Artibeus",  
               "Oxymycterus",  
               "Uroderma",
               "Marmosa", 
               "Oryzomys",  
               "Chiroptera", 
               "Akodon",
               "Myotis", 
               "Micronycteris",   
               "Delomys", 
               "Calomys",  
               "Ateles", 
               "Carollia",
               "Felis catus",
               "Canis lupus familiaris",
               "Canis lupus",
               "Rattu rattus", #Exótico
               "Mus musculus", #Exótico
               "Noctilionoidea",
               "Canis familiaris", #Doméstico
               "Molossidae",
               "Natalidae",
               "Phyllostomidae",
               "Thyropteridae",
               "Furipteridae",
               "Emballonuroidea",
               "Emballonuridae",
               "Oligoryzomys",
               "Rattus rattus", #Exótico
               "Monodelphis",
               "Noctilionidae",
               "Akodon arvicoloides", #Não foi possível ID da espécie atual 
               "Alouatta caraya", #Fora da área de ocorrência
               "Artibeus jamaicensis", #Exótico
               "Artibeus jamaicensis planirostris", #Exótico
               "Rattus norvegicus", #Exótico
               "Eumops patagonicus", #Exótico
               "Glyphonycteris daviesi", #Norte da América do Sul
               "Conepatus chinga", #Fora da área de ocorrência
               "Cynomops abrasus", #Fora da área de ocorrência
               "Histiotus montanus", #Fora da área de ocorrência
               "Lonchophylla mordax", #Fora da área de ocorrência
               "Lophostoma silvicolum", #Fora da área de ocorrência
               "Molossops neglectus", #Fora da área de ocorrência
               "Myotis dinellii", #Fora da área de ocorrência
               "Myotis izecksohni", #Fora da área de ocorrência
               "Myotis lavali", #Fora da área de ocorrência
               "Peropteryx leucoptera", #Fora da área de ocorrência
               "Platyrrhinus incarum", #Fora da área de ocorrência
               "Dasypus hybridus", #Fora da área de ocorrência
               "Monodelphis domestica", #Fora da área de ocorrência
               "Leopardus braccatus", #Fora da área de ocorrência
               "Furipterus horrens", #Fora da área de ocorrência
               "Molossops temminckii", #Fora da área de ocorrência
               "Noctilio albiventris", #Fora da área de ocorrência
               "Promops nasutus", #Fora da área de ocorrência
               "Brucepattersonius",
               "Canis griseoargenteus",
               "Oxymycterus ",
               "Lepus europaeus",
               "Lycalopex vetulus", #Endêmico do Cerrado e este registro não é seguro o suficiente
               "Pseudalopex vetulus",
               "Lycalopex gymnocercus", #Fora da área de ocorrência e todos os registros neste BD são do mesmo data_modifet CAMTRAP e alguns dados não batem com a referência, ou seja não são confiáveis
               "Ziphius cavirostris", #Marinho
               "Mazama nana",
               "Cebus apella",
               "Cebus flavius",
               "Cebus libidinosus",
               "")
data_modif <- data_modif %>% filter(!scientificName %in% to_remove) # removendo registros

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

# Lista de todas as espécies registradas
species <- sort(unique(data_modif$scientificName))
species <- as.data.frame(species)
colnames(species) <- "scientificName"

# acceptedNameUsage -----
accep_sp <- species$scientificName 
accep_sp <- as.data.frame(accep_sp)
colnames(accep_sp) <- "scientificName"
accep_sp$acceptedNameUsage <- accep_sp$scientificName # assumindo que a maioria dos nomes ainda é válido

# Corrigindo sinonimias e adotando nomenclatura mais recente
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Akodon gr. cursor"] <- "Akodon cursor"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Alouatta guariba clamitans"] <- "Alouatta clamitans"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Alouatta guariba guariba"] <- "Alouatta guariba"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Artibeus lituratus lituratus"] <- "Artibeus lituratus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Galictis vittata"] <- "Galictis cuja"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis yaguarundi"] <- "Herpailurus yagouaroundi"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Puma yagouaroundi"] <- "Herpailurus yagouaroundi"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis pardalis"] <- "Leopardus pardalis"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis onca"] <- "Panthera onca"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis concolor"] <- "Puma concolor"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis macroura"] <- "Leopardus wiedii"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Felis brasiliensis"] <- "Leopardus pardalis" #Nascimento, 2010. Revisão taxonomica do gênero Leopardus. Tese doutorado, USP.
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Leopardus tigrinus"] <- "Leopardus guttulus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Hydrochoerus hydrochaeris hydrochaeris"] <- "Hydrochoerus hydrochaeris"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Lutra brasiliensis"] <- "Pteronura brasiliensis"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Oligoryzomys gr. nigripes"] <- "Oligoryzomys nigripes"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Rhipidomys cf. mastacalis"] <- "Rhipidomys mastacalis"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Rhynchiscus naso"] <- "Rhynchonycteris naso"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Trinomys iheringi denigratus"] <- "Trinomys iheringi"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Coelogenys paca"] <- "Agouti paca"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cuniculus paca"] <- "Agouti paca"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Dicotyles labiatus"] <- "Tayassu pecari"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Dusicyon thous azarae"] <- "Cerdocyon thous"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Sapajus nigritus nigritus"] <- "Sapajus nigritus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Sciurus aestuans ingrami"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Guerlinguetus brasiliensi"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Guerlinguetus aestuans"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Sciurus aestuans ingrami"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Sciurus aestuans"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Sciurus alphonsei"] <- "Guerlinguetus ingrami"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Mycetes ursinus"] <- "Alouatta guariba"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Tapirus"] <- "Tapirus terrestris"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Tamandua tetradactyla tetradactyla"] <- "Tamandua tetradactyla"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Myrmecophaga jubata"] <- "Myrmecophaga tetradatyla"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Myotis nigracans nigracans"] <- "Myotis nigracans"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Dasypus novemcinctus novemcinctus"] <- "Dasypus novemcinctus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus nigritus"] <- "Sapajus nigritus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus canthosternos"] <- "Sapajus xanthosternos"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus xanthosternos"] <- "Sapajus xanthosternos"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus variegatus"] <- "Sapajus xanthosternos" #Rylands, 2005, Notes on the taxonomy and distributions of the tufted capuchinmonkeys (Cebus, Cebidae) of South America
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus frontatus"] <- "Sapajus nigritus" #Rylands, 2005, Notes on the taxonomy and distributions of the tufted capuchinmonkeys (Cebus, Cebidae) of South America
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Cebus nigritus robustus"] <- "Sapajus robustus"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Mazama simplicicornis"] <- "Mazama gouazoubira"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Tamandua tetradactyla tetradactyla"] <- "Tamandua tetradactyla"
accep_sp$acceptedNameUsage[accep_sp$scientificName == "Tapirus americanus"] <- "Tapirus terrestris"

# genus -----
# Coluna gênero a partir da primeira palavra da coluna com nome atual das espécies
accep_sp$genus <- word(accep_sp$acceptedNameUsage, 1)

# Lista dos gêneros registrados
genus <- unique(word(accep_sp$acceptedNameUsage, 1))
genus <- as.data.frame(genus)

# family -----
genus$family <- NA
colnames(genus) <- c("genus", "family")

# Identificação das famílias dos gêneros
Atelidae <- genus %>% filter(genus == "Alouatta" | genus == "Ateles" | genus == "Brachyteles")
Atelidae$family <- 'Atelidae'

Bradypodidae <- genus %>% filter(genus == "Bradypus")
Bradypodidae$family <- 'Bradypodidae'

Callitrichidae <- genus %>% filter(genus == "Callithrix" | genus == 'Leontopithecus')
Callitrichidae$family <- 'Callitrichidae'

Canidae <- genus %>% filter(genus == 'Cerdocyon' | genus == 'Chrysocyon' | genus == 'Speothos')
Canidae$family <- 'Canidae'

Caviidae <- genus %>% filter(genus == 'Cavia' | genus == 'Hydrochoerus')
Caviidae$family <- 'Caviidae'

Cebidae <- genus %>% filter(genus == 'Callicebus' | genus == 'Cebus' | genus == 'Sapajus')
Cebidae$family <- 'Cebidae'

Cervidae <- genus %>% filter(genus == 'Mazama')
Cervidae$family <- 'Cervidae'

Chlamyphoridae <- genus %>% filter(genus == 'Cabassous' | genus == 'Euphractus' | genus == 'Priodontes')
Chlamyphoridae$family <- 'Chlamyphoridae'

Cricetidae <- genus %>% filter(genus == "Akodon" | genus == "Blarinomys" | genus == 'Bolomys' | genus == 'Calomys' | genus == 'Cerradomys' | genus == 'Delomys' | genus == 'Euryoryzomys' | genus == 'Hylaeamys' | genus == 'Juliomys' | genus == 'Natalus' | genus == 'Necromys' | genus == 'Nyctinomops' | genus == 'Oecomys' | genus == 'Oligoryzomys' | genus == 'Oryzomys' | genus == 'Rhinophylla' | genus == 'Sooretamys' | genus == 'Nectomys' | genus == 'Oxymycterus' | genus == 'Thaptomys' | genus == 'Rhipidomys')
Cricetidae$family <- 'Cricetidae'

Dasypodidae <- genus %>% filter(genus == 'Dasypus')
Dasypodidae$family <- 'Dasypodidae'

Dasyproctidae <- genus %>% filter(genus == 'Agouti' | genus == 'Dasyprocta')
Dasyproctidae$family <- 'Dasyproctidae'

Didelphidae <- genus %>% filter(genus == "Caluromys" | genus == 'Chironectes' | genus == 'Chironectes' | genus == 'Didelphis' | genus == 'Gracilinanus' | genus == 'Marmosa' | genus == 'Marmosops' | genus == 'Metachirus' | genus == 'Micoureus' | genus == 'Monodelphis')
Didelphidae$family <- 'Didelphidae'

Echimyidae <- genus %>% filter(genus == 'Kannabateomys' | genus == 'Thrichomys' | genus == 'Trinomys' | genus == 'Myocastor')
Echimyidae$family <- 'Echimyidae'

Emballonuridae <- genus %>% filter(genus == 'Centronycteris' | genus == 'Diclidurus' | genus == 'Rhynchonycteris' | genus == 'Peropteryx' | genus == 'Saccopteryx')
Emballonuridae$family <- 'Emballonuridae'

Erethizontidae <- genus %>% filter(genus == 'Coendou' | genus == 'Sphiggurus')
Erethizontidae$family <- 'Erethizontidae'

Felidae <- genus %>% filter(genus == 'Leopardus' | genus == 'Panthera' | genus == 'Herpailurus' | genus == 'Puma')
Felidae$family <- 'Felidae'

Leporidae <- genus %>% filter(genus == 'Sylvilagus')
Leporidae$family <- 'Leporidae'

Mephitidae <- genus %>% filter(genus == 'Conepatus')
Mephitidae$family <- 'Mephitidae'

Molossidae <- genus %>% filter(genus == 'Cynomops' | genus == 'Eumops' | genus == 'Molossus' | genus == 'Tadarida')
Molossidae$family <- 'Molossidae'

Mustelidae <- genus %>% filter(genus == 'Eira' | genus == 'Tayra' | genus == 'Galictis' | genus == "Lontra" | genus == 'Pteronura' | genus == 'Nasua')
Mustelidae$family <- 'Mustelidae'

Myrmecophagidae <- genus %>% filter(genus == 'Myrmecophaga' |  genus == "Tamandua")
Myrmecophagidae$family <- 'Myrmecophagidae'

Noctilionidae <- genus %>% filter(genus == 'Noctilio')
Noctilionidae$family <- 'Noctilionidae'

Phyllostomidae <- genus %>% filter(genus == 'Anoura' | genus == 'Artibeus' | genus == 'Carollia' | genus == 'Chiroderma' | genus == 'Choeroniscus' | genus == 'Chrotopterus' | genus == 'Dermanura' | genus == 'Desmodus' | genus == 'Diaemus' | genus == 'Diphylla' | genus == 'Dryadonycteris' | genus == 'Gardnerycteris' | genus == 'Glossophaga' | genus == 'Glyphonycteris' | genus == 'Lampronycteris' | genus == 'Lichonycteris' | genus == 'Lonchophylla' | genus == 'Lophostoma' | genus == 'Macrophyllum' | genus == 'Micronycteris' | genus == 'Mimon' | genus == 'Philander' | genus == 'Phyllomys' | genus == 'Phyllostomus' | genus == 'Proechimys' | genus == 'Pygoderma' | genus == 'Trinycteris' | genus == 'Uroderma' | genus == 'Phylloderma' | genus == 'Trachops' | genus == 'Sturnira' | genus == 'Platyrrhinus' | genus == 'Tonatia' | genus == 'Vampyressa' | genus == 'Vampyrodes' | genus == 'Lonchorhina')
Phyllostomidae$family <- 'Phyllostomidae'

Procyonidae <- genus %>% filter(genus == 'Potos' | genus == 'Procyon')
Procyonidae$family <- 'Procyonidae'

Sciuridae <- genus %>% filter(genus == 'Guerlinguetus')
Sciuridae$family <- 'Sciuridae'

Tapiridae <- genus %>% filter(genus == 'Tapirus')
Tapiridae$family <- 'Tapiridae'

Tayassuidae <- genus %>% filter(genus == 'Pecari' | genus == 'Tayassu')
Tayassuidae$family <- 'Tayassuidae'

Thyropteridae <- genus %>% filter(genus == 'Thyroptera')
Thyropteridae$family <- 'Thyropteridae'

Trichechidae <- genus %>% filter(genus == 'Trichechus')
Trichechidae$family <- 'Trichechidae'

Vespertilionidae <- genus %>% filter(genus == 'Rhogeessa' | genus == 'Myotis' | genus == 'Eptesicus' | genus == 'Histiotus' | genus == 'Lasiurus')
Vespertilionidae$family <- 'Vespertilionidae'

# Unindo as listas de famílias e gêneros numa única tabela
genus_family <- bind_rows(Atelidae, Bradypodidae, Callitrichidae, Caviidae, Canidae, Cebidae, Cervidae, Chlamyphoridae, Cricetidae, Dasypodidae, Dasyproctidae, Didelphidae, Echimyidae, Emballonuridae, Erethizontidae, Felidae, Leporidae, Mephitidae, Molossidae, Mustelidae, Myrmecophagidae, Noctilionidae, Phyllostomidae, Procyonidae, Sciuridae, Tapiridae, Tayassuidae, Thyropteridae, Trichechidae, Vespertilionidae)

#summary(arsenal::comparedf(genus, genus_family, by = 'genus')) # comparando dataframes

# Lista com todas as famílias registradas
family <- sort(unique(genus_family$family))
family <- as.data.frame(family)

# order ----
# Retirando dados errados
data_modif$order <- as.character(data_modif$order)
data_modif$order[data_modif$order == "notPrecise"] <- ""
data_modif$order[data_modif$order == "Precise"] <- ""

family$order <- NA

# Identificação das ordens das famílias
Primates <- family %>% filter(family == 'Atelidae' | family == 'Callitrichidae' | family == 'Cebidae')
Primates$order <- 'Primates'
Carnivora <- family %>% filter(family == 'Canidae' | family == 'Felidae' | family == 'Mustelidae' | family == 'Procyonidae' | family == 'Felidae' | family == 'Mephitidae')
Carnivora$order <- 'Carnivora'
Pilosa <- family %>% filter(family == 'Bradypodidae' | family == 'Myrmecophagidae')
Pilosa$order <- 'Pilosa'
Rodentia <- family %>% filter(family == 'Caviidae' | family == 'Cricetidae' | family == 'Dasyproctidae' | family == 'Echimyidae' | family == 'Erethizontidae' | family == 'Sciuridae')
Rodentia$order <- 'Rodentia'
Cetartiodactyla <- family %>% filter(family == 'Cervidae' | family == 'Tayassuidae')
Cetartiodactyla$order <- 'Cetartiodactyla'
Cingulata <- family %>% filter(family == 'Chlamyphoridae' | family == 'Dasypodidae')
Cingulata$order <- 'Cingulata'
Didelphimorphia <- family %>% filter(family == 'Didelphidae')
Didelphimorphia$order <- 'Cingulata'
Chiroptera <- family %>% filter(family == 'Emballonuridae' | family == 'Molossidae' | family == 'Noctilionidae' | family == 'Phyllostomidae' | family == 'Thyropteridae' | family == 'Vespertilionidae')
Chiroptera$order <- 'Chiroptera'
Lagomorpha <- family %>% filter(family == 'Leporidae')
Lagomorpha$order <- 'Lagomorpha'
Perissodactyla <- family %>% filter(family == 'Tapiridae')
Perissodactyla$order <- 'Perissodatyla'
Sirenia <- family %>% filter(family == 'Trichechidae')
Sirenia$order <- 'Sirenia'

# Unindo as listas de famílias e gêneros numa única tabela
order <- bind_rows(Primates, Carnivora, Pilosa, Rodentia, Cetartiodactyla, Cingulata, Didelphimorphia, Chiroptera, Lagomorpha, Perissodactyla, Sirenia)

# Taxon ID ----

# Unindo as listas com os dados taxonômicos
taxon <- merge(accep_sp, species, by = 'scientificName')
taxon <- merge(taxon, genus_family, by = 'genus')
taxon <- merge(taxon, order, by = 'family')
taxon <- taxon[, c('order', 'family', 'genus', 'acceptedNameUsage', 'scientificName')]

data_sp_less <- data_modif[1:29]
data_sp_less[30] <- data_modif[34]

# Passando dados taxonomicos para a planilha principal
data_modif <- merge(data_sp_less, taxon, by = 'scientificName')

# Reordenando as colunas da planilha principal
data_modif <- data_modif[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 1)]

# Corrigindo as datas -----
# publicationYear -----
unique(data_modif$PublicationYear)

# Corrigindo as datas de acordo com a referência ##### Se adicionar referência nova isso aqui tem que ser refeito
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[2]] <- ""
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[5]] <- "1999"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[10]] <- "1998"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[28]] <- ""
data_modif$eventYear[data_modif$reference == unique(data_modif$reference)[28]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[29]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[30]] <- "1991"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[31]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[33]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[34]] <- "1990"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[35]] <- "1994"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[36]] <- "1993"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[37]] <- "1991"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[38]] <- "2000"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[39]] <- "1988"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[40]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[41]] <- "1976"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[43]] <- "1964"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[44]] <- "1996"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[45]] <- "1982"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[46]] <- "1991"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[47]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[48]] <- "2013"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[49]] <- "1979"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[50]] <- "1990"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[51]] <- "2006"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[53]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[53]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[54]] <- "2005"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[55]] <- "1955"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[56]] <- "1985"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[57]] <- "1987"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[58]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[60]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[61]] <- "2013"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[62]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[63]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[64]] <- "2000"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[65]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[66]] <- "2001"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[67]] <- "1988"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[68]] <- "1993"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[70]] <- "1996"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[77]] <- "1958"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[81]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[84]] <- "2006"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[86]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[91]] <- "1998"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[94]] <- "1971"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[95]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[96]] <- "1982"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[97]] <- "2005"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[98]] <- "1977"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[99]] <- "2007"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[101]] <- "1987"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[102]] <- "1820"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[103]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[105]] <- "2005"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[106]] <- "2005"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[107]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[110]] <- "2005"
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[113]] <- ""
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[115]] <- "2006"
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[116]] <- ""
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[122]] <- ""
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[123]] <- ""
data_modif$eventYear[data_modif$reference == unique(data_modif$reference)[124]] <- "2011"
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[126]] <- ""
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[128]] <- "1999"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[129]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[130]] <- "1998"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[131]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[132]] <- "2000"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[133]] <- "1990"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[134]] <- "1999"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[135]] <- "1994"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[137]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[138]] <- "1989"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[139]] <- "1993"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[140]] <- "1997"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[141]] <- "1991"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[142]] <- "2002"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[143]] <- "1983"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[144]] <- "1977"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[145]] <- "1981"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[146]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[147]] <- "2011"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[148]] <- "1991"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[149]] <- "1999"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[150]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[151]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[152]] <- "1969"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[153]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[154]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[155]] <- "1980"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[156]] <- "1988"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[157]] <- "1988"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[158]] <- "1993"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[159]] <- "1996"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[160]] <- "2000"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[161]] <- "2016"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[162]] <- "2011"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[163]] <- "2000"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[164]] <- "2013"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[165]] <- "1995"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[166]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[168]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[169]] <- "2015"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[170]] <- "2017"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[171]] <- "2015"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[172]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[173]] <- "1992"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[174]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[175]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[196]] <- "2004"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[197]] <- "1992"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[198]] <- "2006"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[199]] <- "2007"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[200]] <- "2004"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[201]] <- "2007"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[202]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[203]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[204]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[205]] <- "2002"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[206]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[207]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[208]] <- "2011"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[209]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[210]] <- "2009"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[211]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[212]] <- "2010"
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[214]] <- ""
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[216]] <- ""
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[217]] <- ""
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[218]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[219]] <- "1983"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[220]] <- "2010"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[221]] <- "2008"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[222]] <- "2012"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[223]] <- "2015"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[224]] <- "2014"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[225]] <- "2011"
data_modif$PublicationYear[data_modif$reference == unique(data_modif$reference)[226]] <- "2009"
data_modif$eventDate[data_modif$reference == unique(data_modif$reference)[227]] <- ""

#head(select(filter(data_modif, data_modif$reference == unique(data_modif$reference)[228]), 6, 7, 15, 16))

# eventYear (2/2) ----
#View(filter(data_modif, data_modif$eventYear == ""))

# Separando registros sem referencia da tabela principal
data_modif_reference_less <- data_modif %>% filter(reference == "")

# Padronizando a data dos registros sem referencia
data_modif_reference_less$eventYear <- format(as.Date(data_modif_reference_less$eventDate, format = "%m/%d/%y"), "%Y") 

# removendo registros sem referência da tabela principal
data_modif <- data_modif %>% filter(!reference == "") 

# retornando os registros sem referencia com as datas corrigidas
data_modif <- rbind(data_modif, data_modif_reference_less) 

# Na ausência de ano de coleta, considerar o ano de publicação
for (i in 1:nrow(data_modif)) {
  if (data_modif$eventYear[i] == "" && is.na(data_modif$PublicationYear[i]) == FALSE) {
    data_modif$eventYear[i] <- data_modif$PublicationYear[i]
  }
}

# Removendo registros sem ano 
data_modif <- data_modif %>% filter(!eventYear == "") # removendo registros

# Coordenadas geográficas -----
View(select(data_modif, 23:27))

# Passando coordenadas que estão na coluna errada para a coluna correta
for (i in 1:nrow(data_modif)) {
  if (is.na(data_modif$decimalLatitude[i])) {
    data_modif$decimalLatitude[i] <- as.character(data_modif$geodeticDatum[i])
  }
}

# Retirando as coordenadas que estão na coluna errada
to_remove <- data_modif %>% select(geodeticDatum) %>% filter(!str_detect(geodeticDatum, "[[:alpha:] ]+"))
for (i in 1:nrow(data_modif)) {
  if (data_modif$geodeticDatum[i] %in% to_remove$geodeticDatum) {
    data_modif$geodeticDatum[i] <- NA
  }
}

# Retirando os dados errado da coluna certa
to_remove <- data_modif %>% select(decimalLatitude) %>% filter(str_detect(decimalLatitude, "[[:alpha:] ]+"))
for (i in 1:nrow(data_modif)) {
  if (data_modif$decimalLatitude[i] %in% to_remove$decimalLatitude) {
    data_modif$decimalLatitude[i] <- NA
  }
}

# Separando as coordenadas in graus e UTM
x_data_modif <- data_modif %>% select(verbatimLatitude, verbatimLongitude) %>% filter(str_detect(verbatimLatitude, "[[:alpha:] ]+"))

# Retirado as coordenadas em UTM
utm_data_modif <- data_modif %>% filter(verbatimLatitude == "240487949 N") # separando coordenadas em UTM
x_data_modif <- x_data_modif %>% filter(!verbatimLatitude %in% utm_data_modif$verbatimLatitude) # removendo as linhas com coordenadas em UTM

# Preparando as colunas para converter de graus para decimais
x <- x_data_modif

# Separado grau
x <- separate(as.data.frame(x), col = verbatimLatitude, into = c("grau_lat", "verbatimLatitude"), sep = "[º]")
x <- separate(as.data.frame(x), col = verbatimLongitude, into = c("grau_long", "verbatimLongitude"), sep = "[º]")

# Retirando caracteres especiais
x$verbatimLongitude <- str_replace_all(x$verbatimLongitude, "[[:alpha:] ]+", "")
x$verbatimLatitude <- str_replace_all(x$verbatimLatitude, "[[:alpha:] ]+", "")

# Separando minuto e segundo
x <- separate(as.data.frame(x), col = verbatimLatitude, into = c("min_lat", "seg_lat"), sep = "[^[:alnum:]]")
x <- separate(as.data.frame(x), col = verbatimLongitude, into = c("min_long", "seg_long"), sep = "[^[:alnum:]]")

# Retirando caracteres especiais
x$grau_lat <- str_replace_all(x$grau_lat, "[[:alpha:] ]+", "")
x$grau_long <- str_replace_all(x$grau_long, "[[:alpha:] ]+", "")

# Corrigindo alguns erros pontuais
x$seg_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "20"
x$min_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "16"
x$grau_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "18"

x$seg_long[x$grau_lat == unique(x$grau_lat)[2]] <- "55"
x$min_long[x$grau_lat == unique(x$grau_lat)[2]] <- "48"
x$grau_long[x$grau_lat == unique(x$grau_lat)[2]] <- "39"

x$seg_lat[x$seg_lat == ""] <- "0"
x$seg_long[x$seg_long == ""] <- "0"

# Convertendo as coordenadas em graus para UTM
lat_x <- dms2dd(as.numeric(x$grau_lat), as.numeric(x$min_lat), as.numeric(x$seg_lat), "S")
long_x <- dms2dd(as.numeric(x$grau_long), as.numeric(x$min_long), as.numeric(x$seg_long), "W")

coord <- data.frame(lat = as.data.frame(lat_x),
                    long = as.data.frame(long_x))

# Passando para a tabela de trabalho
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% x_data_modif$verbatimLatitude] <- coord$lat_x
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% x_data_modif$verbatimLatitude] <- coord$long_x

# Removendo dados sem registro de coordenada geográfica
to_remove <- data_modif %>% filter(verbatimLatitude == "", verbatimLongitude == "", is.na(decimalLatitude), is.na(decimalLongitude))
data_modif <- anti_join(data_modif, to_remove)

# Convertendo UTM para coordenadas decimais
utm_data_modif <- data_modif %>% select(verbatimLatitude, verbatimLongitude, decimalLatitude, decimalLongitude, geodeticDatum) %>% filter(is.na(decimalLatitude) & is.na(decimalLongitude))
unique(utm_data_modif$geodeticDatum)

d <- utm_data_modif %>% select(verbatimLatitude, verbatimLongitude)

# Os calculos das coordenadas foram feitar num site (acho que IBGE)
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[1]] <- "-16.324448"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[1]] <- "-39.121001"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[2]] <- "-14.018092"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[2]] <- "-39.143283"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[3]] <- "-17.106801"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[3]] <- "-39.339753"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[4]] <- "-14.424250"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[4]] <- "-39.060414"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[5]] <- "-13.525323"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[5]] <- "-39.035311"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[6]] <- "-15.172064"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[6]] <- "-39.061124"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[7]] <- "-17.169331"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[7]] <- "-39.841776"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[8]] <- "-15.155310"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[8]] <- "-39.526954"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[9]] <- "-15.927010"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[9]] <- "-39.635847"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[10]] <- "-17.292106"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[10]] <- "-39.673031"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[11]] <- "-13.578941"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[11]] <- "-39.706685"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[12]] <- "-15.619982"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[12]] <- "-39.161263"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[13]] <- "-15.973720"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[13]] <- "-39.373680"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[14]] <- "-16.512313"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[14]] <- "-39.303612"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[15]] <- "-13.701098"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[15]] <- "-39.232629"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[16]] <- "-15.166549"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[16]] <- "-39.059754"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[17]] <- "-13.952912"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[17]] <- "-39.451138"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[18]] <- "-16.599388"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[18]] <- "-39.913983"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[19]] <- "-13.864987"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[19]] <- "-39.672635"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[20]] <- "-16.286450"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[20]] <- "-39.424079"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[21]] <- "-15.197319"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[21]] <- "-39.391085"
utm_data_modif$decimalLatitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[22]] <- "-14.343671"
utm_data_modif$decimalLongitude[utm_data_modif$verbatimLatitude == unique(d$verbatimLatitude)[22]] <- "-39.086907"

# Passando para a tabela de trabalho
data_modif$decimalLatitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLatitude
data_modif$decimalLongitude[data_modif$verbatimLatitude %in% utm_data_modif$verbatimLatitude] <- utm_data_modif$decimalLongitude

# Removendo registros sem coordenadas geográfica
data_modif <- data_modif %>% filter(!is.na(decimalLatitude)) 

to_remove <- data_modif %>% filter(decimalLatitude == "")
data_modif <- anti_join(data_modif, to_remove)

# Corrigindo latitude/longitude trocada ------

# Separando dados trocados
corrigir_latlong <- data_modif %>% filter(decimalLongitude < -30)
long <- corrigir_latlong$decimalLatitude
lat <- corrigir_latlong$decimalLongitude

# Corrigindo a troca
correto_latlong <- corrigir_latlong
correto_latlong$decimalLatitude <- lat
correto_latlong$decimalLongitude <- long

# Removendo registros com os dados trocados
data_modif <- anti_join(data_modif, corrigir_latlong)

# Adicionando registros com os dados corrigidos
data_modif <- rbind(data_modif, correto_latlong)

# Checando
View(data_modif)

# Exportando -----

# Exportando tabela padronizada
write_csv(data_modif, 'data-clean.csv')

# Exportando lista das espécies registradas
write_csv(as.data.frame(unique(data_modif$acceptedNameUsage)), 'species.csv') #lista de espécies

