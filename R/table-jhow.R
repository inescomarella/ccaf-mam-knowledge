library(tidyverse)
library(ggplot2)

setwd('./data')

data_modif <- read.csv("data-clean.csv")

data_modif_sorted <- arrange(data_modif, eventYear)

earlier_register <- data_modif_sorted[!duplicated(data_modif_sorted$acceptedNameUsage, fromLast = FALSE),]

table_jhow <- data.frame('year' = unique(earlier_register$eventYear),
                         'n.species' = NA)

for (i in 1:nrow(earlier_register))
  table_jhow$n.species[i] <- nrow(earlier_register %>% filter(eventYear == table_jhow$year[i]))

bar_plot <- ggplot(table_jhow, aes(year, n.species)) +
  geom_col(width = 1)

jpeg('../figs/registros-anos.jpg')
bar_plot
dev.off()
