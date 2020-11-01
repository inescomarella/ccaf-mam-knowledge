library(tidyverse)
library(ggplot2)

setwd('./data')

# Input
data <- read.csv("data-all-clean.csv")

# Ordena por ordem de ano
data_sorted <- arrange(data, as.numeric(eventYear))

# Mantem apenas o primeiro registro da especie
earlier_register <- data_sorted[!duplicated(data_sorted$species, fromLast = FALSE),]

# Mantem apenas o ultimo registro da especie
last_register <- data_sorted[!duplicated(data_sorted$species, fromLast = TRUE),]

# Unindo primeiro e ultimo registro em um unico data.frame
df <- data.frame('earlier' = earlier_register$eventYear,
                 'last' = last_register$eventYear)

# Plotando
plot <- ggplot(df) + 
  geom_bar(aes(earlier, fill = 'Primeiro registro')) + 
  geom_bar(aes(last, fill = 'Ultimo registro')) +
  labs(y = "Número de espécies", 
       x = "Anos",
       fill = "") +
  theme_bw() +
  theme(legend.position = "bottom")

plot

# Tabela - frequencia de registros
freq_table <- data.frame(table(earlier_register$eventYear))
colnames(freq_table) <- c('year', 'N species')

# Tabela - Sequencia completa de anos desde o primeiro registro até o último
years <-
  as.character(seq(
    as.numeric(as.character(freq_table$year[1])),
    2020,
    1
  ))
registers_through_years <- data.frame('year' = years,
                                  stringsAsFactors = FALSE)
registers_through_years <-
  merge(registers_through_years,
        freq_table,
        by = 'year',
        all.x = TRUE)


