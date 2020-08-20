x <- c("tidyverse", "rgbif")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Inputs
data_paper <- read.csv('data-papers.csv')
data_splink <- read.csv('data-spLink.csv')
data_gbif <- read.csv('data-gbif.csv')

# Mudando coluna do gbif
data_gbif$scientificName <- data_gbif$species

# Unindo dados
data_all_raw <- plyr::rbind.fill(data_paper, data_splink, data_gbif)

# Mantendo apenas as colunas da planilha qu eelaborei
data_all <- select(data_all_raw, colnames(data_paper))

# Removendo dados sem identificação da especie
data_all$scientificName <- as.character(data_all$scientificName)
data_all <- data_all %>% filter(!is.na(scientificName))
data_all <- data_all %>% filter(!str_detect(scientificName, " ") == FALSE)
data_all <- data_all %>% filter(!str_detect(scientificName, 'sp.'))
data_all$scientificName <- str_replace_all(data_all$scientificName, "[.]", "") # retirando os pontos para remover especies com nomes usando caracteres especies
data_all <- data_all %>% filter(!str_detect(scientificName, "[[:punct:]]"))

x <- data.frame()
for (i in 1:length(unique(data_all$scientificName)))
  x <- bind_rows(x, name_backbone(unique(data_all$scientificName)[i]))

length(unique(data_all$scientificName))
nrow(x)
View(x)

unique(x$rank)

#########################33
#              "Felis catus",
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
