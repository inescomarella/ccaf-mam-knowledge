##################
####### Para fazer

#1. Organizar as datas
#2. Padronizar as cidades
#3. Padronizar as reservas
#4. Converter as coordenadas geograficas
#5. Plotar no mapa
#6. Remover os pontos fora do CCMA
#7. Salvar os registros limpos e padronizados, eles serão usados para o resto das análises
#
##################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("tidyverse")
library("CoordinateCleaner")
library("lubridate")


data_raw <- read.csv("raw-data.csv")
glimpse(data_raw)

# retirando registros marinhos
data_modif <- data_raw[!(data_raw$order == "Cetartiodactyla" | data_raw$order == "Cetacea"), ]

# basisOfRecord -----
unique(data_modif$basisOfRecord)
filter(data_modif, data_modif$basisOfRecord == "S")

# dataSetName -----
unique(data_modif$datasetName)

# Removendo caracteres especiais
data_modif$datasetName <- str_replace_all(data_modif$datasetName, "[^[:alnum:]]", " ")

data_modif <- data_modif[!(str_detect(data_modif$datasetName, "Lima  F   Beca  G   Muylaert  R L   Jenkins  C N   Perilli  M L L   Paschoal  A M O   Massara  R L   Paglia  A P   Chiarello  A G   Graipel  M E   Cherem  J J   Regolin  A L   Oliveira Santos  L G R   Brocardo  C R   Paviolo  A   Di Bitetti  M S   Scoss  L M   Rocha  F L   Fusco Costa  R   Rosa  C A   Da Silva  M X   Hufnagell  L   Santos  P M   Duarte  G T   Guimarães  L N   Bailey  L L   Rodrigues  F H G   Cunha  H M   Fantacini  F M   Batista  G O   Bogoni  J A   Tortato  M A   Luiz  M R   Peroni  N   De Castilho  P V   Maccarini  T B   Filho  V P   Angelo  C D   Cruz  P   Quiroga  V   Iezzi  M E   Varela  D   Cavalcanti  S M C   Martensen  A C   Maggiorini  E V   Keesen  F F   Nunes  A V   Lessa  G M   Cordeiro Estrela  P   Beltrão  M G   De Albuquerque  A C F   Ingberman  B   Cassano  C R   Junior  L C   Ribeiro  M C  and Galetti  M   2017   ATLANTIC CAMTRAPS  a dataset of medium and large terrestrial mammal communities in the Atlantic Forest of South America  Ecology  98  2979 2979  doi 10 1002 ecy 1998")), ] # removendo registros desse dataset pois contém muitos registros errados

# language -----
unique(data_modif$language)

# institutionCode -----
unique(data_modif$institutionCode)
data_modif$institutionCode <- as.character(data_modif$institutionCode)
data_modif$institutionCode[data_modif$institutionCode == "Berkeley Natural History Museum"] <- "BNHM"

# collectionCode -----
unique(data_modif$collectionCode)

data_modif$collectionCode <- as.character(data_modif$collectionCode)
data_modif$collectionCode[data_modif$collectionCode == "Coleção de Mamíferos  do Museu Nacional (MN)"] <- "MN - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o de Mam�feros, Universidade Estadual de Santa Cruz (UESC), Ilh�us, BA, Brazil"] <- "UESC - Mammal Collection"
data_modif$collectionCode[data_modif$collectionCode == "Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ - ALP Collection"
data_modif$collectionCode[data_modif$collectionCode == "Laboratorio de Diversidade de Morcegos (LDM) and Cole��o Adriano L�cio Peracchi (ALP), Universidade Federal Rural do Rio de Janeiro, Serop�dica, RJ, Brazil"] <- "UFRRJ -LDM and ALP Collections"
data_modif$collectionCode[data_modif$collectionCode == "Museum of Vertebrate Zoology"] <- "MVZ"
data_modif$collectionCode[data_modif$collectionCode == "Not located"] <- ""


#data_modif$collectionCode <- str_replace_all(data_modif$collectionCode, "[^[:alnum:]]", " ")

# reference -----
unique(data_modif$reference)
data_modif$reference <- as.character(data_modif$reference)

data_modif %>% filter(str_detect(reference, "entrevista"))
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

# publicationYear -----
unique(data_modif$PublicationYear)

# typeOfPublication -----
unique(data_modif$typeOfPublication)
data_modif$typeOfPublication[data_modif$typeOfPublication == "Unpublish"] <- "Unpublished"

# catalogNumber -----
unique(data_modif$catalogNumber)

# recordedBy -----
unique(data_modif$recordedBy)

# fieldNumber -----
unique(data_modif$fieldNumber)
data_modif$fieldNumber <- as.character(data_modif$fieldNumber)
data_modif$fieldNumber[data_modif$fieldNumber == "?"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Daniela Rossoni"] <- " "
data_modif$fieldNumber[data_modif$fieldNumber == " Valéria Fagundes"] <- " "

# preparations -----
unique(data_modif$preparations)
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

# associatedReferences -----
unique(data_modif$associatedReferences)
str(data_modif$associatedReferences)

# eventDate -----
unique(data_modif$eventDate)

# eventYear ----
unique(data_modif$eventYear)

# Corrigindo as datas no formato ano-mes-dia
data_trace <- data_modif %>% filter(str_detect(eventYear, "[-]")) # separando as linhas erradas
year_trace <- data_trace$eventYear # separando a coluna eventYear
year_trace <- as.data.frame(year_trace)
year_trace_sep <- format(as.Date(year_trace$year_trace, format = "%Y-%m-%d"), "%Y") # extraindo o ano das datas
year_trace_sep <- as.data.frame(year_trace_sep)
data_trace$eventYear <- year_trace_sep$year_trace_sep # retornando os anos corrigidos para a coluna

to_remove <- data_modif %>% filter(str_detect(eventYear, "[-]")) # separando as linhas erradas
data_trace_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) # removendo as linhas erradas
data_modif  <- rbind(data_trace_less, data_trace) # adicionando as linhas corrigidas

nrow(data_modif)
nrow(data_trace_less) + nrow(data_trace) # confirmando os tamanhos das tabelas

# Corrigindo as datas no formato ano/ano
data_bar <- data_modif %>% filter(str_detect(eventYear, "[/]")) # separando as linhas erradas
year_bar <- data_bar$eventYear # separando a coluna eventYear
year_bar <- as.data.frame(year_bar)
year_bar_sep <- separate(data = year_bar, col = year_bar, into = c("A", "B"), sep = "[/]") # separando primeiro/ultimo ano
year_bar_correct <- year_bar_sep$B # retirando o primeiro ano
year_bar_correct <- as.data.frame(year_bar_correct)
data_bar$eventYear <- year_bar_correct$year_bar_correct # retornando os anos corrigidos para a coluna

to_remove <- data_modif %>% filter(str_detect(eventYear, "[/]")) # separando as linhas erradas
data_bar_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) # removendo as linhas erradas
data_modif  <- rbind(data_bar_less, data_bar) # adicionando as linhas corrigidas

nrow(data_modif)
nrow(data_bar) + nrow(data_bar_less) # confirmando os tamanhos das tabelas

# country -----
unique(data_modif$country)
data_modif$country <- "Brazil"

# stateProvince -----
unique(data_modif$stateProvince)
data_modif$stateProvince <- as.character(data_modif$stateProvince)
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
data_modif$UC[data_modif$UC == "yes"] <- "Yes"
data_modif$UC[data_modif$UC == "no"] <- "No"
data_modif$UC[data_modif$UC == "Parque Estadual da Fonte Grande"] <- "Yes"
data_modif$UC[is.na(data_modif$UC)] <- ""

# georeferencePrecision ----
unique(data_modif$georeferencePrecision)
data_modif$georeferencePrecision <- as.character(data_modif$georeferencePrecision)
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "precise"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Localidade"] <- "Precise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "notPrecise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not-Precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "Not precise"] <- "NotPrecise"
data_modif$georeferencePrecision[data_modif$georeferencePrecision == "[no data]"] <- ""

# scientificName ----
# corrigindo o nome da coluna
col_names <- colnames(data_modif)
col_names[34] <- "scientificName"
colnames(data_modif) <- col_names

# Removendo caracteres especiais
data_modif$scientificName <- str_replace_all(data_modif$scientificName, "[^[:alnum:]]", " ")

# Removendo ponto em espécie tipo Genero.epiteto
data_modif$scientificName <- as.character(data_modif$scientificName)
data_modif$scientificName <- gsub("[.]", " ", data_modif$scientificName) # corrigido nomes separados por ponto
data_modif <- data_modif[!is.na(data_modif$scientificName), ] #removendo registros sem espécies
data_modif <- data_modif[!(str_detect(data_modif$scientificName, "sp.")), ] # removendo registros sem ID da espécie

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
               "Lycalopex gymnocercus", #Fora da área de ocorrência e todos os registros neste BD são do mesmo dataset CAMTRAP e alguns dados não batem com a referência, ou seja não são confiáveis
               "Ziphius cavirostris", #Marinho
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

####
data_modif$scientificName[data_modif$scientificName == "Sapajus libidinosus libidinosus"] <- "Sapajus libidinosus"
data_modif$scientificName[data_modif$scientificName == "Peropteryx cf. kappleri"] <- "Peropteryx kappleri"
data_modif$scientificName[data_modif$scientificName == "Peropteryx trinitatis trinitatis"] <- "Peropteryx trinitatis"
data_modif$scientificName[data_modif$scientificName == "Sciurus alphonsei alphonsei"] <- "Sciurus alphonsei"
###

species <- sort(unique(data_modif$scientificName))
species <- as.data.frame(species)
colnames(species) <- "scientificName"

# acceptedNameUsage -----
accep_sp <- species$scientificName 
accep_sp <- as.data.frame(accep_sp)
colnames(accep_sp) <- "scientificName"
accep_sp$acceptedNameUsage <- accep_sp$scientificName # assumindo que a maioria dos nomes ainda é válido

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

# genus -----

accep_sp$genus <- word(accep_sp$acceptedNameUsage, 1)
genus <- unique(word(accep_sp$acceptedNameUsage, 1))
genus <- as.data.frame(genus)

# family -----
genus$family <- NA
genus$family[1] <- "Dasyproctidae"
genus$family[2] <- "Cricetidae"
genus$family[3] <- "Atelidae"
genus$family[4] <- "Phyllostomidae"
genus$family[5] <- "Phyllostomidae"
genus$family[6] <- "Atelidae"
genus$family[7] <- "Cricetidae"
genus$family[8] <- "Cricetidae"
genus$family[9] <- "Atelidae"

genus$family[10] <- "Bradypodidae"
genus$family[11] <- "Chlamyphoridae"
genus$family[12] <- "Cebidae"
genus$family[13] <- "Callitrichidae"
genus$family[14] <- "Cricetidae"
genus$family[15] <- "Didelphidae"
genus$family[16] <- "Phyllostomidae"
genus$family[17] <- "Caviidae"
genus$family[18] <- "Cebidae"
genus$family[19] <- "Emballonuridae"

genus$family[20] <- "Canidae"
genus$family[21] <- "Cricetidae"
genus$family[22] <- "Phyllostomidae"
genus$family[23] <- "Didelphidae"
genus$family[24] <- "Phyllostomidae"
genus$family[25] <- "Phyllostomidae"
genus$family[26] <- "Canidae"
genus$family[27] <- "Erethizontidae"
genus$family[28] <- "Mephitidae"
genus$family[29] <- "Molossidae"

genus$family[30] <- "Dasyproctidae"
genus$family[31] <- "Dasypodidae"
genus$family[32] <- "Cricetidae"
genus$family[33] <- "Phyllostomidae"
genus$family[34] <- "Phyllostomidae"
genus$family[35] <- "Phyllostomidae"
genus$family[36] <- "Emballonuridae"
genus$family[37] <- "Tayassuidae"
genus$family[38] <- "Didelphidae"
genus$family[39] <- "Phyllostomidae"

genus$family[40] <- "Phyllostomidae"
genus$family[41] <- "Mustelidae"
genus$family[42] <- "Vespertilionidae"
genus$family[43] <- "Molossidae"
genus$family[44] <- "Chlamyphoridae"
genus$family[45] <- "Cricetidae"
genus$family[46] <- "Felidae"
genus$family[47] <- "Felidae"
genus$family[48] <- "Felidae"
genus$family[49] <- "Felidae"

genus$family[50] <- "Mustelidae"
genus$family[51] <- "Phyllostomidae"
genus$family[52] <- "Phyllostomidae"
genus$family[53] <- "Phyllostomidae"
genus$family[54] <- "Didelphidae"
genus$family[55] <- "Sciuridae"
genus$family[56] <- "Vespertilionidae"
genus$family[57] <- "Caviidae"
genus$family[58] <- "Cricetidae"
genus$family[59] <- "Cricetidae"

genus$family[60] <- "Echimyidae"
genus$family[61] <- "Phyllostomidae"
genus$family[62] <- "Vespertilionidae"
genus$family[63] <- "Callitrichidae"
genus$family[64] <- "Phyllostomidae"
genus$family[65] <- "Phyllostomidae"
genus$family[66] <- "Phyllostomidae"
genus$family[67] <- "Mustelidae"
genus$family[68] <- "Phyllostomidae"
genus$family[69] <- "Mustelidae"

genus$family[70] <- "Phyllostomidae"
genus$family[71] <- "Didelphidae"
genus$family[72] <- "Didelphidae"
genus$family[73] <- "Cervidae"
genus$family[74] <- "Didelphidae"
genus$family[75] <- "Didelphidae"
genus$family[76] <- "Phyllostomidae"
genus$family[77] <- "Phyllostomidae"
genus$family[78] <- "Molossidae"
genus$family[79] <- "Didelphidae"

genus$family[80] <- "Vespertilionidae"
genus$family[81] <- "Myrmecophagidae"
genus$family[82] <- "Procyonidae"
genus$family[83] <- "Natalidae"
genus$family[84] <- "Cricetidae"
genus$family[85] <- "Cricetidae"
genus$family[86] <- "Noctilionidae"
genus$family[87] <- "Molossidae"
genus$family[88] <- "Cricetidae"
genus$family[89] <- "Cricetidae"

genus$family[90] <- "Cricetidae"
genus$family[91] <- "Cricetidae"
genus$family[92] <- "Tayassuidae"
genus$family[93] <- "Emballonuridae"
genus$family[94] <- "Didelphidae"
genus$family[95] <- "Phyllostomidae"
genus$family[96] <- "Echimyidae"
genus$family[97] <- "Phyllostomidae"
genus$family[98] <- "Phyllostomidae"
genus$family[99] <- "Procyonidae"

genus$family[100] <- "Dasypodidae"
genus$family[101] <- "Procyonidae"
genus$family[102] <- "Echimyidae"
genus$family[103] <- "Phyllostomidae"
genus$family[104] <- "Phyllostomidae"
genus$family[105] <- "Cricetidae"
genus$family[106] <- "Vespertilionidae"
genus$family[107] <- "Emballonuridae"
genus$family[108] <- "Emballonuridae"
genus$family[109] <- "Cebidae"

genus$family[110] <- "Canidae"
genus$family[111] <- "Erethizontidae"
genus$family[112] <- "Phyllostomidae"
genus$family[113] <- "Leporidae"
genus$family[114] <- "Molossidae"
genus$family[115] <- "Myrmecophagidae"
genus$family[116] <- "Tapiridae"
genus$family[117] <- "Mustelidae"
genus$family[118] <- "Cricetidae"
genus$family[119] <- "Echimyidae"

genus$family[120] <- "Thyropteridae"
genus$family[121] <- "Phyllostomidae"
genus$family[122] <- "Phyllostomidae"
genus$family[123] <- "Trichechidae"
genus$family[124] <- "Echimyidae"
genus$family[125] <- "Phyllostomidae"
genus$family[126] <- "Phyllostomidae"
genus$family[127] <- "Phyllostomidae"
genus$family[128] <- "Phyllostomidae"

colnames(genus) <- c("genus", "family")

family <- sort(unique(genus$family))
family <- as.data.frame(family)

# order ----
# Retirando dados errados
data_modif$order <- as.character(data_modif$order)
data_modif$order[data_modif$order == "notPrecise"] <- ""
data_modif$order[data_modif$order == "Precise"] <- ""

family$order <- NA

family$order[1] <- "Primates"
family$order[2] <- "Pilosa"
family$order[3] <- "Primates"
family$order[4] <- "Carnivora"
family$order[5] <- "Rodentia"
family$order[6] <- "Primates"
family$order[7] <- "Cetartiodactyla"
family$order[8] <- "Cingulata"
family$order[9] <- "Rodentia"
family$order[10] <- "Cingulata"
family$order[11] <- "Rodentia"
family$order[12] <- "Didelphimorphia"
family$order[13] <- "Rodentia"
family$order[14] <- "Chiroptera"
family$order[15] <- "Rodentia"
family$order[16] <- "Carnivora"
family$order[17] <- "Lagomorpha"
family$order[18] <- "Carnivora"
family$order[19] <- "Chiroptera"
family$order[20] <- "Carnivora"
family$order[21] <- "Pilosa"
family$order[22] <- "Chiroptera"
family$order[23] <- "Chiroptera"
family$order[24] <- "Chiroptera"
family$order[25] <- "Carnivora"
family$order[26] <- "Rodentia"
family$order[27] <- "Perissodactyla"
family$order[28] <- "Cetartiodactyla"
family$order[29] <- "Chiroptera"
family$order[30] <- "Sirenia"
family$order[31] <- "Chiroptera"

# Taxon ID ----
taxon <- merge(accep_sp, species, by = 'scientificName')
taxon <- merge(taxon, genus, by = 'genus')
taxon <- merge(taxon, family, by = 'family')
taxon <- taxon[, c('order', 'family', 'genus', 'acceptedNameUsage', 'scientificName')]

data_sp_less <- data_modif[1:29]
data_sp_less[30] <- data_modif[34]
data_modif <- merge(data_sp_less, taxon, by = 'scientificName')

# Reordenando as colunas
data_modif <- data_modif[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 1)]

# CORRIGINDO AS DATAS -----

datas <- as.data.frame(data_modif$reference)
datas[2] <- data_modif$PublicationYear
datas[3] <- data_modif$eventDate
datas[4] <- data_modif$eventYear
colnames(datas) <- c("reference", "PublicationYear", "eventDate", "eventYear")

#Corrigindo as datas de acordo com a referência
datas$PublicationYear[datas$reference == unique(datas$reference)[1]] <- "1999"
datas$eventDate[datas$reference == unique(datas$reference)[3]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[10]] <- "1998"
datas$eventDate[datas$reference == unique(datas$reference)[12]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[29]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[30]] <- "1991"
datas$PublicationYear[datas$reference == unique(datas$reference)[32]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[33]] <- "1997"
datas$PublicationYear[datas$reference == unique(datas$reference)[34]] <- "1997"
datas$PublicationYear[datas$reference == unique(datas$reference)[35]] <- "1991"
datas$PublicationYear[datas$reference == unique(datas$reference)[36]] <- "1990"
datas$PublicationYear[datas$reference == unique(datas$reference)[37]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[38]] <- "2001"
datas$PublicationYear[datas$reference == unique(datas$reference)[39]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[40]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[41]] <- "1991"
datas$PublicationYear[datas$reference == unique(datas$reference)[43]] <- "1988"
datas$PublicationYear[datas$reference == unique(datas$reference)[44]] <- "1982"
datas$PublicationYear[datas$reference == unique(datas$reference)[45]] <- "1988"
datas$PublicationYear[datas$reference == unique(datas$reference)[46]] <- "1976"
datas$PublicationYear[datas$reference == unique(datas$reference)[49]] <- "2006"
datas$PublicationYear[datas$reference == unique(datas$reference)[50]] <- "1996"
datas$PublicationYear[datas$reference == unique(datas$reference)[51]] <- "1979"
datas$PublicationYear[datas$reference == unique(datas$reference)[52]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[53]] <- "1994"
datas$PublicationYear[datas$reference == unique(datas$reference)[55]] <- "1958"
datas$PublicationYear[datas$reference == unique(datas$reference)[57]] <- "1987"
datas$PublicationYear[datas$reference == unique(datas$reference)[58]] <- "1964"
datas$PublicationYear[datas$reference == unique(datas$reference)[61]] <- "1985"
datas$PublicationYear[datas$reference == unique(datas$reference)[63]] <- "1955"
datas$PublicationYear[datas$reference == unique(datas$reference)[65]] <- "1996"
datas$PublicationYear[datas$reference == unique(datas$reference)[66]] <- "1993"
datas$PublicationYear[datas$reference == unique(datas$reference)[67]] <- "1990"
datas$PublicationYear[datas$reference == unique(datas$reference)[74]] <- "1997"
datas$eventDate[datas$reference == unique(datas$reference)[75]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[77]] <- "2006"
datas$eventYear[datas$reference == unique(datas$reference)[77]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[79]] <- "2009"
datas$eventDate[datas$reference == unique(datas$reference)[80]] <- ""
datas$eventDate[datas$reference == unique(datas$reference)[81]] <- ""
datas$eventDate[datas$reference == unique(datas$reference)[82]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[84]] <- "1971"
datas$PublicationYear[datas$reference == unique(datas$reference)[85]] <- "1998"
datas$eventDate[datas$reference == unique(datas$reference)[86]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[88]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[91]] <- "2007"
datas$PublicationYear[datas$reference == unique(datas$reference)[92]] <- "1977"
datas$PublicationYear[datas$reference == unique(datas$reference)[93]] <- "2005"
datas$PublicationYear[datas$reference == unique(datas$reference)[94]] <- "2005"
datas$PublicationYear[datas$reference == unique(datas$reference)[95]] <- "1820"
datas$PublicationYear[datas$reference == unique(datas$reference)[96]] <- "1986"
datas$PublicationYear[datas$reference == unique(datas$reference)[97]] <- "1987"
datas$PublicationYear[datas$reference == unique(datas$reference)[99]] <- "2005"
datas$PublicationYear[datas$reference == unique(datas$reference)[100]] <- "2005"
datas$PublicationYear[datas$reference == unique(datas$reference)[101]] <- "1982"
datas$PublicationYear[datas$reference == unique(datas$reference)[102]] <- "2006"
datas$PublicationYear[datas$reference == unique(datas$reference)[104]] <- "1997"
datas$eventDate[datas$reference == unique(datas$reference)[105]] <- ""
datas$eventDate[datas$reference == unique(datas$reference)[107]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[108]] <- "2009"
datas$PublicationYear[datas$reference == unique(datas$reference)[121]] <- "1999"
datas$PublicationYear[datas$reference == unique(datas$reference)[122]] <- "1998"
datas$PublicationYear[datas$reference == unique(datas$reference)[123]] <- "1997"
datas$PublicationYear[datas$reference == unique(datas$reference)[124]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[125]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[126]] <- "1994"
datas$PublicationYear[datas$reference == unique(datas$reference)[127]] <- "2012"
datas$PublicationYear[datas$reference == unique(datas$reference)[129]] <- "1999"
datas$PublicationYear[datas$reference == unique(datas$reference)[130]] <- "1990"
datas$PublicationYear[datas$reference == unique(datas$reference)[131]] <- "1989"
datas$PublicationYear[datas$reference == unique(datas$reference)[132]] <- "1993"
datas$PublicationYear[datas$reference == unique(datas$reference)[133]] <- "1991"
datas$PublicationYear[datas$reference == unique(datas$reference)[134]] <- "1997"
datas$PublicationYear[datas$reference == unique(datas$reference)[135]] <- "2002"
datas$PublicationYear[datas$reference == unique(datas$reference)[136]] <- "1983"
datas$PublicationYear[datas$reference == unique(datas$reference)[137]] <- "1977"
datas$PublicationYear[datas$reference == unique(datas$reference)[138]] <- "1981"
datas$PublicationYear[datas$reference == unique(datas$reference)[139]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[140]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[141]] <- "1991"
datas$PublicationYear[datas$reference == unique(datas$reference)[142]] <- "2011"
datas$PublicationYear[datas$reference == unique(datas$reference)[143]] <- "2012"
datas$PublicationYear[datas$reference == unique(datas$reference)[144]] <- "1999"
datas$PublicationYear[datas$reference == unique(datas$reference)[145]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[146]] <- "1969"
datas$PublicationYear[datas$reference == unique(datas$reference)[147]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[148]] <- "1988"
datas$PublicationYear[datas$reference == unique(datas$reference)[149]] <- "1980"
datas$PublicationYear[datas$reference == unique(datas$reference)[150]] <- "1988"
datas$PublicationYear[datas$reference == unique(datas$reference)[151]] <- "1993"
datas$PublicationYear[datas$reference == unique(datas$reference)[152]] <- "1995"
datas$PublicationYear[datas$reference == unique(datas$reference)[153]] <- "2011"
datas$PublicationYear[datas$reference == unique(datas$reference)[154]] <- "2013"
datas$PublicationYear[datas$reference == unique(datas$reference)[155]] <- "1996"
datas$PublicationYear[datas$reference == unique(datas$reference)[156]] <- "2016"
datas$PublicationYear[datas$reference == unique(datas$reference)[158]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[159]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[160]] <- "2009"
datas$PublicationYear[datas$reference == unique(datas$reference)[161]] <- "2000"
datas$PublicationYear[datas$reference == unique(datas$reference)[162]] <- "2015"
datas$PublicationYear[datas$reference == unique(datas$reference)[163]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[164]] <- "2015"
datas$PublicationYear[datas$reference == unique(datas$reference)[165]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[166]] <- "1992"
datas$PublicationYear[datas$reference == unique(datas$reference)[167]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[168]] <- "2017"
datas$eventDate[datas$reference == unique(datas$reference)[176]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[177]] <- "1955"
datas$eventDate[datas$reference == unique(datas$reference)[178]] <- ""
datas$eventDate[datas$reference == unique(datas$reference)[181]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[182]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[189]] <- "1992"
datas$PublicationYear[datas$reference == unique(datas$reference)[190]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[191]] <- "2009"
datas$PublicationYear[datas$reference == unique(datas$reference)[192]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[193]] <- "2007"
datas$PublicationYear[datas$reference == unique(datas$reference)[194]] <- "2004"
datas$PublicationYear[datas$reference == unique(datas$reference)[195]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[196]] <- "2007"
datas$PublicationYear[datas$reference == unique(datas$reference)[197]] <- "2004"
datas$PublicationYear[datas$reference == unique(datas$reference)[198]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[199]] <- "2002"
datas$PublicationYear[datas$reference == unique(datas$reference)[200]] <- "2009"
datas$PublicationYear[datas$reference == unique(datas$reference)[201]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[202]] <- "2006"
datas$PublicationYear[datas$reference == unique(datas$reference)[203]] <- "2011"
datas$PublicationYear[datas$reference == unique(datas$reference)[204]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[205]] <- "2009"
datas$eventDate[datas$reference == unique(datas$reference)[208]] <- ""
datas$eventYear[datas$reference == unique(datas$reference)[208]] <- "2012"
datas$eventDate[datas$reference == unique(datas$reference)[209]] <- ""
datas$PublicationYear[datas$reference == unique(datas$reference)[211]] <- "1983"
datas$PublicationYear[datas$reference == unique(datas$reference)[212]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[213]] <- "2008"
datas$PublicationYear[datas$reference == unique(datas$reference)[214]] <- "2010"
datas$PublicationYear[datas$reference == unique(datas$reference)[215]] <- "2014"
datas$PublicationYear[datas$reference == unique(datas$reference)[216]] <- "2012"
datas$PublicationYear[datas$reference == unique(datas$reference)[217]] <- "2015"
datas$PublicationYear[datas$reference == unique(datas$reference)[218]] <- "2009"
datas$PublicationYear[datas$reference == unique(datas$reference)[219]] <- "2011"
datas$eventDate[datas$reference == unique(datas$reference)[220]] <- ""


 ########## ADICIONAR O ANO ###########
View(filter(datas, datas$reference == unique(datas$reference)[13]))
datas_reference_less <- datas %>% filter(reference == unique(datas$reference)[13])
datas_reference_less$eventYear <- format(as.Date(datas_reference_less$eventDate, format = "%m/%d/%y"), "%Y") 

datas <- datas %>% filter(!reference == unique(datas$reference)[13]) # removendo registros
datas <- rbind(datas, datas_reference_less)

for (i in 1:nrow(datas)) {
  if (datas$eventYear[i] == "" && is.na(datas$PublicationYear[i]) == FALSE) {
    datas$eventYear[i] <- datas$PublicationYear[i]
  }
}

View(datas)

#if(eventYear = ""){eventYear <- PublicationYear}
#filter(datas, datas$reference == unique(datas$reference)[69]) ## eventDate =  mes/dia/ano --> ano-mes-dia

datas$eventYear <- as.numeric(as.character(datas$eventYear))
################################################################################################################

# DANI
# O QUE FAZER COM O *ATELEX HYPOXANTHUS*?



write_csv(data_modif, 'data_0520.csv')
