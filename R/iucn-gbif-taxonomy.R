
library(rredlist)
rlkey = '6abf9b5a0010ab26140c401c1c394a22c43c0a555d9dee8c72976d3c71c5e402'


rl.synonyms <- function(x){
  synym = rl_synonyms(x,key=rlkey)
  result = synym$result
  taxonid = synym$name
  results = cbind(taxonid,result)
  return(results)
}

data_test <- data_all

sp_list_all <- sort(unique(data_test$scientificName))

apply.synonyms <- lapply(sp_list_all,rl.synonyms)

synonyms_df <- ldply(apply.synonyms, data.frame)

colnames(synonyms_df)


sp_synonym_iucn_df <- data.frame()
for (i in 1:(length(sp_list_all)/4)) {
  Sys.sleep(10)
  rl_output <- rl_synonyms(sp_list_all[i], key = '6abf9b5a0010ab26140c401c1c394a22c43c0a555d9dee8c72976d3c71c5e402')
  
  if (rl_output$count >= 1) {
    rl_result <- as.data.frame(rl_output$result)
    rl_result_selected <- rl_result %>% select(accepted_name)
    rl_result_selected$scientificName <- sp_list_all[i]
    
    sp_synonym_iucn_df <- bind_rows(sp_synonym_iucn_df, rl_result_selected[1,])
  }
  Sys.sleep(10)
  
}

colnames(df)[3:4] <- c('species', 'scientificName')

df$species[df$scientificName == 'Anoura geoffroyi'] <- 'Anoura geoffroyi'
df$species[df$scientificName == 'Lagothrix lagotricha'] <- 'Lagothrix lagotricha'
df$species[df$scientificName == 'Mimon crenulatum'] <- 'Mimon crenulatum'
df$species[df$scientificName == 'Natalus macrourus'] <- 'Natalus macrourus'
df$species[df$scientificName == 'Natalus stramineus'] <- 'Natalus stramineus'
df$species[df$scientificName == 'Nectomys squamipes'] <- 'Nectomys squamipes'



data_species_iucn <- merge(data_all, df[3:4], by.x = 'scientificName', all.x = FALSE)
data_without_species_iucn <- anti_join(data_all, data_species_iucn)

sp_iucn_list <- sort(unique(data_species_iucn$species))
sp_iucn_backbone <- data.frame()
for (i in 1:length(sp_iucn_list))
  sp_iucn_backbone <-
  bind_rows(sp_iucn_backbone, name_backbone(sp_iucn_list[i])) ### just keep order and family


sp_gbif_list <- sort(unique(data_without_species_iucn$scientificName))
sp_gbif_backbone <- data.frame()
for (i in 1:length(sp_gbif_list))
  sp_gbif_backbone <-
  bind_rows(sp_gbif_backbone, name_backbone(sp_gbif_list[i]))
nrow(sp_gbif_backbone)

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
