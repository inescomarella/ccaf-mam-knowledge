
library(rredlist)
rlkey = '6abf9b5a0010ab26140c401c1c394a22c43c0a555d9dee8c72976d3c71c5e402'


rl.synonyms <- function(x){
  synym <- rl_synonyms(x,key=rlkey)
  result <- synym$result
  scientificName <- synym$name
  results <- cbind(scientificName,result)
  return(results)
}

name.backbone <- function(x){
  bckbn <- name_backbone(x)
  bckbn$scientificName <- x
  return(bckbn)
}

data_test <- data_all

sp_list_all <- sort(unique(data_test$scientificName))

apply_synonyms <- lapply(sp_list_all,rl.synonyms)

synonyms_df <- ldply(apply_synonyms, data.frame)

synonyms_df_corrected <- synonyms_df

#colnames(synonyms_df_corrected)[1] <- 'scientificName'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Anoura geoffroyi'] <-
  'Anoura geoffroyi'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Lagothrix lagotricha'] <-
  'Lagothrix lagotricha'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Mimon crenulatum'] <-
  'Mimon crenulatum'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Natalus macrourus'] <-
  'Natalus macrourus'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Natalus stramineus'] <-
  'Natalus stramineus'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Nectomys squamipes'] <-
  'Nectomys squamipes'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Saguinus bicolor'] <-
  'Saguinus martinsi'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Micoureus paraguayanus'] <-
  'Marmosa paraguayana'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Micoureus travassosi'] <-
  'Micoureus travassosi'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Anoura caudifer'] <-
  'Anoura caudifer'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Alouatta fusca'] <-
  'Alouatta guariba'
synonyms_df_corrected$accepted_name[synonyms_df_corrected$scientificName == 'Callicebus personatus'] <-
  'Callicebus personatus'

# Removendo Cebus spp. para identificar separadamente com base no local de registro
synonyms_df_corrected <- synonyms_df_corrected %>% filter(!str_detect(scientificName, 'Cebus'))

# Separa os dados com sinonimo revisado e não revisado pela iucn
iucn_synonyms_df <- synonyms_df_corrected %>% filter(!is.na(accepted_name))
no_iucn_synonyms_df <- synonyms_df_corrected %>% filter(is.na(accepted_name))

# Taxonomy backbone sinonimia revisada pela iucn
apply_backbone_iucn <- lapply(iucn_synonyms_df$accepted_name,name.backbone)
backbone_iucn_df <- ldply(apply_backbone_iucn, data.frame)

# Taxonomy backbone não revisado
apply_backbone_gbif <- lapply(no_iucn_synonyms_df$scientificName,name.backbone)
backbone_gbif_df <- ldply(apply_backbone_gbif, data.frame)

backbone_gbif_df$species[backbone_gbif_df$scientificName == 'Puma yagouaroundi'] <- 'Herpailurus yagouaroundi'
backbone_gbif_df$species[backbone_gbif_df$scientificName == 'Puma yaguarondi'] <- 'Herpailurus yagouaroundi'
backbone_gbif_df$species[backbone_gbif_df$scientificName == 'Lutra brasiliensis'] <- 'Pteronura brasiliensis'
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Mycetes ursinus"] <- "Alouatta guariba"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Mazama simplicicornis"] <- "Mazama gouazoubira"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Dicotyles labiatus"] <- "Tayassu pecari"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Ateles hypoxanthus"] <- "Brachyteles hypoxanthus"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Dasyprocta agouti"] <- "Dasyprocta aguti"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Cavia aguti"] <- "Cuniculus paca"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Cavia agouti"] <- "Cuniculus paca"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Oryzomys trinitatis"] <- "Oecomys trinitatis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Callithrix leucocephalus"] <- "Callithrix geoffroyi"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Lagothrix infumata"] <- "Lagothrix lagotricha"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Synetheres prehensilis"] <- "Coendou prehensilis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Trinomys atiosus"] <- "Trinomys gratiosus"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Epitesicus furinalis"] <- "Eptesicus furinalis"
backbone_gbif_df$species[backbone_gbif_df$scientificName == "Felis brasiliensis"] <- "Leopardus pardalis" #Nascimento, 2010. Revisão taxonomica do gênero Leopardus. Tese doutorado, USP.


head(backbone_gbif_df)



# Sapajus ------
S_nigritus <- data_all %>% filter(str_detect(scientificName, 'Cebus'), str_detect(stateProvince, 'Santo'), decimalLatitude < -19.5 )
S_robustus_es <- data_all %>% filter(str_detect(scientificName, 'Cebus'), str_detect(stateProvince, 'Santo'), decimalLatitude > -19.5 )
S_robustus_ba <- data_all %>% filter(str_detect(scientificName, 'Cebus'), str_detect(stateProvince, 'Bahia'), decimalLatitude < -15.8 )
S_xanthosternos <- data_all %>% filter(str_detect(scientificName, 'Cebus'), str_detect(stateProvince, 'Bahia'), decimalLatitude > -15.8 )

S_nigritus$acceptedNameUsage <- 'Sapajus nigritus'
S_robustus_es$acceptedNameUsage <- 'Sapajus robustus'
S_robustus_ba$acceptedNameUsage <- 'Sapajus robustus'
S_xanthosternos$acceptedNameUsage <- 'Sapajus xanthosternos'

sapajus_df <- bind_rows(S_nigritus, S_robustus_es, S_robustus_ba, S_xanthosternos)

apply_backbone_sapajus <- lapply(sapajus_df$acceptedNameUsage,name.backbone)
backbone_sapajus_df <- ldply(apply_backbone_sapajus, data.frame)

# Removendo registros de Sapajus adicionar o backbone separadamente, já que a identificação não é com base no scientificName (não dá para usar o merge)
data_all_without_sapajus <- data_all %>% filter(!str_detect(scientificName, 'Cebus'))

# Adicionando Sapajus backbone para o data.frame de sapajus
backbone_sapajus_df_selected <- backbone_sapajus_df %>% select(order, family, species)
sapajus_df_final <- bind_cols(sapajus_df, backbone_sapajus_df_selected) ################## Corrigir colunas 


####################################################################
sp_iucn_backbone_df_selected <- sp_iucn_backbone %>% select(order, family)
sp_gbif_backbone_df_selected <- sp_gbif_backbone %>% select(order, family, species)
sp_iucn_backbone_df_selected <- cbind(sp_iucn_backbone_df_selected, df[3:4])
sp_gbif_backbone_df_selected$scientificName <- data.frame(scientificName = sp_gbif_list)
nrow(df)
sp_gbif_backbone_df_selected$species[sp_gbif_backbone_df_selected$species == 'Puma yagouaroundi'] <- 'Herpailurus yagouaroundi'
sp_gbif_backbone_df_selected$species[sp_gbif_backbone_df_selected$species == 'Puma yaguarondi'] <- 'Herpailurus yagouaroundi'
sp_gbif_backbone_df_selected$species[sp_gbif_backbone_df_selected$species == 'Lutra brasiliensis'] <- 'Pteronura brasiliensis'


View(sp_gbif_backbone_df_selected %>% select(canonicalName, species, synonym) %>% filter(synonym == 'FALSE'))
View(sp_iucn_backbone %>% select(canonicalName, species, synonym) %>% filter(str_detect(species, 'paca')))
