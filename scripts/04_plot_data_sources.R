# File purpose: Plot collections graphs and species first-last record graph
# Data: 17/11/2020

# Load in libraries
library(tidyverse)
library(cowplot)

# Load data --------------------------------------------------------

data <-
  read.csv("data/processed/clean_database.csv") %>%
  select(-X)

# Pre-process data -------------------------------------------------

records_df <- data %>%
  mutate(Collection = ifelse(institutionCode == "UFES" | str_detect(institutionCode, "CEPLAC") | institutionCode == "MEL" | institutionCode == "UESC" | institutionCode == "MBML",
    "Local\ncollections",
    ifelse(institutionCode == "",
      "Scientific\nliterature",
      ifelse(institutionCode == "KU" | institutionCode == "LACM" | institutionCode == "USNM" | institutionCode == "BNHM" | institutionCode == "MCZ" | institutionCode == "ROM" | institutionCode == "FMNH" | institutionCode == "CLO" | institutionCode == "UMMZ",
        "International\ncollections",
        ifelse(str_detect(institutionCode, "iNat"),
          "iNaturalist",
          "National\ncollections"
        )
      )
    )
  ))

level_order <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Collection)

colection_contri <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  mutate(per = n / sum(n))

colection_contri$label <- scales::percent(colection_contri$per)
colection_contri %>%
  filter(per < 0.2) %>%
  select(per) %>%
  sum()

# Plot ------------------------------------------------------------

line_graph_collections <- records_df %>%
  filter(year != "NA") %>%
  mutate(fk_group = "a") %>%
  mutate(year = as.Date(as.character(year), "%Y")) %>%
  group_by(fk_group, Collection, year) %>%
  summarise(n = n()) %>%
  mutate(ncum = cumsum(n)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = ncum, color = factor(Collection, levels = level_order$Collection))) +
  theme_light() +
  labs(
    color = element_blank(),
    x = element_blank(),
    y = "Cumulative number of records"
  ) +
  theme(legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15))

frst_lst_rcrd_df <- data %>%
  filter(!is.na(year), year != "", year != "NA") %>%
  mutate(year = as.Date(as.character(year), "%Y")) %>%
  group_by(species) %>%
  summarise(
    first_record = min(year),
    last_record = max(year)
  ) %>%
  pivot_longer(
    cols = -c(species),
    names_to = "record",
    values_to = "year"
  ) %>%
  group_by(year, record) %>%
  summarise(n = n()) %>%
  mutate(n = ifelse(record == "last_record", -n, n))

yseq <- seq(1810, 2030, 10)

frst_lst_rcrd_df_10y <- data.frame()

for (i in 1:length(yseq)) {
    df_10y <- frst_lst_rcrd_df %>%
      filter(year >= as.Date(as.character(yseq[i]), "%Y") && year < as.Date(as.character(yseq[i + 1]), "%Y")) %>%
      group_by(record) %>%
      summarise(n = sum(n)) %>%
      mutate(year = as.Date(as.character(yseq[i]), "%Y"))

    frst_lst_rcrd_df_10y <- bind_rows(frst_lst_rcrd_df_10y, df_10y)
}

breaks_values <- 
  pretty(frst_lst_rcrd_df_10y$n, n = 10)

frst_lst_rcrd_graph <- frst_lst_rcrd_df_10y %>%
  ggplot(aes(x = year, y = n, fill = record)) +
  scale_x_date(date_labels = "%Y") +
  geom_hline(yintercept = 0)+
  scale_fill_discrete(labels = c("First record", "Last record")) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values)) +
  theme_light() +
  ylab("Number of species") +
  xlab(element_blank()) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)
  )

# Save ------------------------------------------------------------
# Plot
frst_lst_rcrd_graph
ggsave("figs/04_first_last_record.png",
  width = 8,
  height = 6
)

line_graph_collections
ggsave("figs/04_data_sources.png",
       width = 12,
       height = 6
)

# Save workspace ----
save.image("~/tcc-ccma/workspaces/explore_data.RData")
