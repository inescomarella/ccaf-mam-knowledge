x <- c("tidyverse", "rgbif")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Get occurence data from GBIF 1500-2020
data_downloaded <- data.frame()
for (i in seq(1500, 2020, 1))
  data_downloaded <-
  bind_rows(data_downloaded,
    occ_data(country = 'BR', #apenas no BR
      hasCoordinate = TRUE, #com coordenadas geograficas
      classKey = '359', #mamiferos
      year = i)$data) #apenas os dados

# Apenas registros no Espirito Santo e Bahia
data_ba_es <- data_downloaded %>% filter(is.na(stateProvince) | str_detect(stateProvince, 'anto') | str_detect(stateProvince, 'ahia'))

# Removendo registros fosseis
to_remove <- data_ba_es %>% filter(basisOfRecord == "FOSSIL_SPECIMEN")
data_clean <- anti_join(data_ba_es, to_remove)

# Removendo colunas com códigos do GBIF
data_clean <- select(data_clean, -ends_with("Key"))

# Removendo colunas com formato incompativel
data_clean <- select(data_clean, -c(identifiedByIDs, recordedByIDs, dynamicProperties))
data_clean <- select(data_clean, -starts_with("http"))



try <- data.frame()
for (i in 1:ncol(data_clean))
  print(vapply(data_clean[,i], paste, collapse = ", ", character(1L)))

colnames(data_clean)

# Output
write.csv(data_clean, './data-gbif.csv')
