##################
####### Para fazer

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

# datasetName -----
unique(data_modif$datasetName)

# Removendo caracteres especiais
data_modif$data_modifetName <- str_replace_all(data_modif$datasetName, "[^[:alnum:]]", "")

data_modif <- data_modif[!(str_detect(data_modif$datasetName, "Lima  F   Beca  G   Muylaert  R L   Jenkins  C N   Perilli  M L L   Paschoal  A M O   Massara  R L   Paglia  A P   Chiarello  A G   Graipel  M E   Cherem  J J   Regolin  A L   Oliveira Santos  L G R   Brocardo  C R   Paviolo  A   Di Bitetti  M S   Scoss  L M   Rocha  F L   Fusco Costa  R   Rosa  C A   Da Silva  M X   Hufnagell  L   Santos  P M   Duarte  G T   Guimarães  L N   Bailey  L L   Rodrigues  F H G   Cunha  H M   Fantacini  F M   Batista  G O   Bogoni  J A   Tortato  M A   Luiz  M R   Peroni  N   De Castilho  P V   Maccarini  T B   Filho  V P   Angelo  C D   Cruz  P   Quiroga  V   Iezzi  M E   Varela  D   Cavalcanti  S M C   Martensen  A C   Maggiorini  E V   Keesen  F F   Nunes  A V   Lessa  G M   Cordeiro Estrela  P   Beltrão  M G   De Albuquerque  A C F   Ingberman  B   Cassano  C R   Junior  L C   Ribeiro  M C  and Galetti  M   2017   ATLANTIC CAMTRAPS  a data_modifet of medium and large terrestrial mammal communities in the Atlantic Forest of South America  Ecology  98  2979 2979  doi 10 1002 ecy 1998")), ] # removendo registros desse data_modifet pois contém muitos registros errados

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

# eventYear (1/2) ----
unique(data_modif$eventYear)

# Corrigindo as data_modif no formato ano-mes-dia
data_trace <- data_modif %>% filter(str_detect(eventYear, "[-]")) # separando as linhas erradas
year_trace <- data_trace$eventYear # separando a coluna eventYear
year_trace <- as.data.frame(year_trace)
year_trace_sep <- format(as.Date(year_trace$year_trace, format = "%Y-%m-%d"), "%Y") # extraindo o ano das data_modif
year_trace_sep <- as.data.frame(year_trace_sep)
data_trace$eventYear <- year_trace_sep$year_trace_sep # retornando os anos corrigidos para a coluna

to_remove <- data_modif %>% filter(str_detect(eventYear, "[-]")) # separando as linhas erradas
data_trace_less <- data_modif %>% filter(!eventYear %in% to_remove$eventYear) # removendo as linhas erradas
data_modif  <- rbind(data_trace_less, data_trace) # adicionando as linhas corrigidas

nrow(data_modif)
nrow(data_trace_less) + nrow(data_trace) # confirmando os tamanhos das tabelas

# Corrigindo as data_modif no formato ano/ano
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
               "Lycalopex gymnocercus", #Fora da área de ocorrência e todos os registros neste BD são do mesmo data_modifet CAMTRAP e alguns dados não batem com a referência, ou seja não são confiáveis
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

# Corrigindo as datas -----
# publicationYear -----
unique(data_modif$PublicationYear)


#Corrigindo as datas de acordo com a referência
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
data_modif_reference_less <- data_modif %>% filter(reference == "")
data_modif_reference_less$eventYear <- format(as.Date(data_modif_reference_less$eventDate, format = "%m/%d/%y"), "%Y") 

data_modif <- data_modif %>% filter(!reference == "") # removendo registros da tabela principal
data_modif <- rbind(data_modif, data_modif_reference_less) # retornando os registros corrigidos

# Na ausência de ano de coleta, considerar o ano de publicação
for (i in 1:nrow(data_modif)) {
  if (data_modif$eventYear[i] == "" && is.na(data_modif$PublicationYear[i]) == FALSE) {
    data_modif$eventYear[i] <- data_modif$PublicationYear[i]
  }
}

# Removendo registros sem ano 
data_modif <- data_modif %>% filter(!eventYear == "") # removendo registros
View(data_modif)

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

# Preparando as colunas para convertes graus para decimais
x <- x_data_modif
x <- separate(as.data.frame(x), col = verbatimLatitude, into = c("grau_lat", "verbatimLatitude"), sep = "[º]")
x <- separate(as.data.frame(x), col = verbatimLongitude, into = c("grau_long", "verbatimLongitude"), sep = "[º]")

x$verbatimLongitude <- str_replace_all(x$verbatimLongitude, "[[:alpha:] ]+", "")
x$verbatimLatitude <- str_replace_all(x$verbatimLatitude, "[[:alpha:] ]+", "")

x <- separate(as.data.frame(x), col = verbatimLatitude, into = c("min_lat", "seg_lat"), sep = "[^[:alnum:]]")
x <- separate(as.data.frame(x), col = verbatimLongitude, into = c("min_long", "seg_long"), sep = "[^[:alnum:]]")

x$grau_lat <- str_replace_all(x$grau_lat, "[[:alpha:] ]+", "")
x$grau_long <- str_replace_all(x$grau_long, "[[:alpha:] ]+", "")

x$seg_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "20"
x$min_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "16"
x$grau_lat[x$grau_lat == unique(x$grau_lat)[2]] <- "18"

x$seg_long[x$grau_lat == unique(x$grau_lat)[2]] <- "55"
x$min_long[x$grau_lat == unique(x$grau_lat)[2]] <- "48"
x$grau_long[x$grau_lat == unique(x$grau_lat)[2]] <- "39"

x$seg_lat[x$seg_lat == ""] <- "0"
x$seg_long[x$seg_long == ""] <- "0"

# Convertendo as coordenadas em graus para UTM
lat_x <- biogeo::dms2dd(as.numeric(x$grau_lat), as.numeric(x$min_lat), as.numeric(x$seg_lat), "S")
long_x <- biogeo::dms2dd(as.numeric(x$grau_long), as.numeric(x$min_long), as.numeric(x$seg_long), "W")

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

#d$verbatimLatitude <- as.numeric(as.character(d$verbatimLatitude))
#d$verbatimLongitude <- as.numeric(as.character(d$verbatimLongitude))

#library('sp')
#coordinates(d) <- ~verbatimLongitude + verbatimLatitude
#sputm <- SpatialPoints(d, proj4string = CRS("+proj=utm +zone=24 +datum=SAD69"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
#coordinates(spgeo)
################################################################################################################

# DANI
# O QUE FAZER COM O *ATELEX HYPOXANTHUS*?



write_csv(data_modif, 'data_clean.csv')
