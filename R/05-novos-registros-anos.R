library(tidyverse)
library(ggplot2)

setwd('./data')

# Input
data <- read.csv("data-ccma.csv")

# Ordena por ordem de ano
data_sorted <- arrange(data, eventYear)

# Mantem apenas o primeiro registro da especie
earlier_register <- data_sorted[!duplicated(data_sorted$species, fromLast = FALSE),]

# Anos de registro
registered_per_year <- data.frame('year' = as.character(unique(earlier_register$eventYear)),
                         'n.species' = "",
                         stringsAsFactors = FALSE)

# Adicionando numero de registros novos em cada ano
for (i in 1:nrow(earlier_register))
  registered_per_year$n.species[i] <- nrow(earlier_register %>% 
                                             filter(eventYear == registered_per_year$year[i]))

years <- as.character(seq(min(earlier_register$eventYear), max(earlier_register$eventYear), 1))
registers_through_years <- data.frame('year' = years,
                                  stringsAsFactors = FALSE)

registers_through_years <- merge(registers_through_years, registered_per_year, by = 'year', all.x = TRUE)

# Output
jpeg('../figs/registros-anos.jpg')

barplot(
  height = as.numeric(registers_through_years$n.species),
  names = registers_through_years$year,
  col = "#69b3a2",
  horiz = F ,
  las = 1
)

dev.off()
