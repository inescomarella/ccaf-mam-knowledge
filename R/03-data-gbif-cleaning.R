x <- c("tidyverse", "rgbif")
lapply(x, library, character.only = TRUE)

setwd('./data')

# Solicitando dados de ocorrencia de mamiferos no anos 1500-2020
# Acessar o site do GBIF e baixar
res <- occ_download(user = 'inescomarella',
                    pwd = '*****',
                    email = 'inesmottacomarella@gmail.com',
                    format = "SIMPLE_CSV",
                    pred('country', 'BR'),
                    pred('taxonKey', 359), #apenas mamiferos
                    pred("hasCoordinate", TRUE),
                    pred_gte("year", 1500), #greaterThanOrEquals
                    pred_lte("year", 2020) #lessThanOrEquals
                    )

# Input
data_downloaded <-
  read.table(
    './results/0042756-200613084148143.csv',
    sep = "\t",
    header = TRUE,
    comment.char = "#",
    na.strings = ".",
    stringsAsFactors = FALSE,
    quote = "",
    fill = TRUE
  )

# Apenas registros no Espirito Santo e Bahia
data_ba_es <-
  data_downloaded %>% filter(
    is.na(locality) |
      str_detect(locality, 'anto') |
      str_detect(locality, 'ahia') |
      is.na(stateProvince) |
      str_detect(stateProvince, 'anto') |
      str_detect(stateProvince, 'ahia')
  )

# Removendo registros fosseis e do iNaturalist
to_remove <-
  data_ba_es %>% filter(basisOfRecord == "FOSSIL_SPECIMEN" |
                          str_detect(institutionCode, "iNaturalist"))
data_clean <- anti_join(data_ba_es, to_remove)

# Identificando stateProvince
for (i in 1:nrow(data_clean)) {
  if (str_detect(data_clean$locality[i], 'anto'))
    data_clean$stateProvince[i] <- 'Espirito Santo'
  
  if (str_detect(data_clean$locality[i], 'ahia'))
    data_clean$stateProvince[i] <- 'Bahia'
}

# Output
write.csv(data_clean, './data-gbif.csv')
