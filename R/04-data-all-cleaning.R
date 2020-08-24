x <- c("tidyverse", "rgbif", "plyr")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Inputs
data_paper <- read.csv('data-papers.csv')
data_splink <- read.csv('data-spLink.csv')
data_gbif <- read.csv('data-gbif.csv')

# Mudando colunas
data_gbif$scientificName <- data_gbif$species
colnames(data_gbif)[34] <- 'eventYear'
colnames(data_splink)[11] <- 'eventYear'

# Unindo dados
data_all_raw <- rbind.fill(data_paper, data_splink, data_gbif)

# Mantendo apenas as colunas da planilha que elaborei
data_all <- select(data_all_raw, colnames(data_paper))

# Removendo dados sem identificação da especie
data_all$scientificName <- as.character(data_all$scientificName)
to_remove <- data_all %>% filter(is.na(scientificName),
                                 str_detect(scientificName, " ") == FALSE,
                                 str_detect(scientificName, 'sp.'))
data_all <- anti_join(data_all, to_remove)
data_all <- data_all %>% filter(!is.na(scientificName))

# Retirando os pontos para remover especies com nomes usando caracteres especiais (e.g. '?')
data_all$scientificName <- str_replace_all(data_all$scientificName, "[.]", "")
data_all <- data_all %>% filter(!str_detect(scientificName, "[[:punct:]]"))

# Retirando híbridos
data_all <- data_all %>% filter(!str_detect(scientificName, "íbrido"))

# Removendo nomes antigos de especies fora da área de ocorrência
data_all <- data_all %>% filter(!str_detect(scientificName, "iseoargenteus"))
data_all <- data_all %>% filter(!str_detect(scientificName, "Samiris sciurea"))
data_all <- data_all %>% filter(!str_detect(scientificName, "azzarae"))

# Retirando identificações que possam atrapalhar o reconhecimento da espécie
data_all$scientificName <- str_replace(data_all$scientificName, pattern = " gr", replacement = " ")
data_all$scientificName <- str_replace(data_all$scientificName, pattern = "cf ", replacement = " ")
data_all$scientificName <- str_replace(data_all$scientificName, pattern = "Cf ", replacement = "")
data_all$scientificName <- str_replace(data_all$scientificName, pattern = " j ", replacement = " ")

# Resolvendo problema de nomes antigos não reconhecidos pelo GBIF
sp_data_all <- data.frame('especie_data_all' = unique(data_all$scientificName),
                          'sinonimo_atual' = unique(data_all$scientificName),
                          stringsAsFactors = FALSE)

sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Cebus variegatus"] <- "Sapajus xanthosternos" #Rylands, 2005, Notes on the taxonomy and distributions of the tufted capuchinmonkeys (Cebus, Cebidae) of South America
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Mycetes ursinus"] <- "Alouatta guariba"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Felis brasiliensis"] <- "Leopardus pardalis" #Nascimento, 2010. Revisão taxonomica do gênero Leopardus. Tese doutorado, USP.
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Mazama simplicicornis"] <- "Mazama gouazoubira"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Dicotyles labiatus"] <- "Tayassu pecari"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Guerlinguetus brasiliensi"] <- "Guerlinguetus ingrami"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Rhynchiscus naso"] <- "Rhynchonycteris naso"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Sciurus alphonsei alphonsei"] <- "Guerlinguetus ingrami"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Ateles hypoxanthus"] <- "Brachyteles hypoxanthus"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Dasyprocta agouti"] <- "Dasyprocta aguti"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Cavia aguti"] <- "Agouti paca"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Cavia agouti"] <- "Agouti paca"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Oryzomys trinitatis"] <- "Oecomys trinitatis"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Callithrix leucocephalus"] <- "Callithrix geoffroyi"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Lagothrix infumata"] <- "Lagothrix lagotricha"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Synetheres prehensilis"] <- "Coendou prehensilis"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Trinomys atiosus"] <- "Trinomys gratiosus"
sp_data_all$sinonimo_atual[sp_data_all$sinonimo_atual == "Epitesicus furinalis"] <- "Eptesicus furinalis"

# Classificação taxonomica
sp_backbone <- data.frame()

for (i in 1:nrow(sp_data_all))
  sp_backbone <- bind_rows(sp_backbone, name_backbone(sp_data_all$sinonimo_atual[i]))

sp_taxon <- cbind(sp_data_all, sp_backbone)
colnames(sp_taxon)[1] <- "Especie"

sp_taxon_clean <- sp_taxon %>% 
  select(Especie, order, family, genus, species)
colnames(sp_taxon_clean)[1] <- 'scientificName'

# Identificando espécies não reconhecidas pelo GBIF --> assumindo o scientificName
sp_taxon_to_correct <- sp_taxon_clean %>% 
  filter(is.na(species)) %>% 
  select(scientificName, order, family, genus, species)
sp_taxon_clean <- anti_join(sp_taxon_clean, sp_taxon_to_correct)
sp_taxon_to_correct$species <- sp_taxon_to_correct$scientificName
sp_taxon_clean_correct <- bind_rows(sp_taxon_to_correct, sp_taxon_clean)

# Identificação taxonomica das especies na tabela principal
data_all <- data_all %>% 
  select(-c(order, family, genus))
data_all_sp_added <- merge(data_all, sp_taxon_clean_correct, by = 'scientificName')

# Corrigindo a ordem das colunas
data_all_sp_added <- data_all_sp_added %>% 
  relocate(scientificName, .after = species)

# Retirando a coluna acceptedNameUsage, assumindo a coluna species como o nome mais atual
data_all_sp_added <- data_all_sp_added %>% 
  select(-acceptedNameUsage)

exotic_sp_list <- data.frame('species' = c('Canis lupus',
                                           'Rattus rattus',
                                           'Mus musculus',
                                           'Felis catus',
                                           'Alouatta caraya',
                                           'Artibeus jamaicensis',
                                           'Rattus norvegicus',
                                           'Eumops patagonicus',
                                           'Glyphonycteris daviesi',
                                           'Conepatus chinga',
                                           'Cynomops abrasus',
                                           'Histiotus montanus',
                                           'Lonchophylla mordax',
                                           'Lophostoma silvicolum',
                                           'Molossops neglectus',
                                           'Myotis dinellii',
                                           'Myotis izecksohni',
                                           'Myotis lavali',
                                           'Peropteryx leucoptera',
                                           'Platyrrhinus incarum',
                                           'Dasypus hybridus',
                                           'Monodelphis domestica',
                                           'Leopardus braccatus',
                                           'Furipterus horrens',
                                           'Molossops temminckii',
                                           'Noctilio albiventris',
                                           'Promops nasutus',
                                           'Canis griseoargenteus',
                                           'Lepus europaeus',
                                           'Pseudalopex vetulus',
                                           'Ziphius cavirostris',
                                           'Mazama nana',
                                           'Cebus apella',
                                           'Sapajus apella',
                                           'Cebus flavius',
                                           'Cebus libidinosus',
                                           'Sapajus libidinosus',
                                           'Sapajus flavius'))

# Removendo registros de especies exoticas
data_all_sp_clean <- data_all_sp_added %>% 
  filter(!species %in% exotic_sp_list$species)

# Removendo registros marinhos
data_all_sp_clean <- data_all_sp_clean %>% 
  filter(!str_detect(order, 'Cetacea'))

# Output
write.csv(data_all_sp_clean, './data-all-clean.csv')
