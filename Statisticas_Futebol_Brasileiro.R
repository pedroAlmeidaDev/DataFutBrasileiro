
#Intalando pacotes necessários para o projeto
install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

library(readr)

library(dplyr)


#Criando DataFrames a partir dos arquivos .CSV carregados 
campeonato_brasileiro_cartoes <- read.csv("campeonato-brasileiro-cartoes.csv")
campeonato_brasileiro_estatisticas_full <- read.csv("campeonato-brasileiro-estatisticas-full.csv")
campeonato_brasileiro_full <- read.csv("campeonato-brasileiro-full.csv")
campeonato_brasileiro_gols <- read.csv("campeonato-brasileiro-gols.csv")

#Convertendo campo tipo caracter para Date
campeonato_brasileiro_full$data <- as.Date(campeonato_brasileiro_full$data, format = "%d/%m/%Y")

#Média de gols por partida dos clubes em 2024
MediaGolsMarcados <- campeonato_brasileiro_gols %>%
  inner_join(campeonato_brasileiro_full, by = c("partida_id" = "ID")) %>%
  filter(format(data, "%Y") == "2024") %>%
  group_by(clube, partida_id) %>%
  summarize(gols_por_partida = n(), .groups = "drop") %>%
  group_by(clube) %>%
  summarize(mean_gols = mean(gols_por_partida), .groups = "drop") %>%
  arrange(desc(mean_gols))

#Gráfico em barras da Média de gols por partida dos clubes em 2024
ggplot(data = MediaGolsMarcados, aes(x = clube, y = mean_gols, fill = clube)) + 
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(mean_gols, 2)),
    vjust = -0.8
  ) + 
  labs(
    title = "Média de gols por time em 2024",
    subtitle = "Valores médios calculados por equipe",
    x = "Time",
    y = "Média de gols"
  ) +
  theme_minimal()


