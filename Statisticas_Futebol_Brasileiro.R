
#Intalando pacotes necessários para o projeto
install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

library(readr)

library(dplyr)

library(lubridate)


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
  filter(year(data) == 2024) %>%
  group_by(clube, partida_id) %>%
  summarize(gols_por_partida = n(), .groups = "drop") %>%
  group_by(clube) %>%
  summarize(mean_gols = round(mean(gols_por_partida),2), .groups = "drop") %>%
  arrange(desc(mean_gols))

#Gráfico em barras da Média de gols por partida dos clubes em 2024
ggplot(data = MediaGolsMarcados, aes(x = clube, y = mean_gols, fill = clube)) + 
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = mean_gols),
    vjust = -0.8
  ) + 
  labs(
    title = "Média de gols por time em 2024",
    subtitle = "Valores médios calculados por equipe",
    x = "Time",
    y = "Média de gols"
  ) +
  theme_minimal()

########################################################################

#Média de gols por partida do Flamengo nos anos de 2020 a 2024
MediaGolsMarcadosFlamengo <- campeonato_brasileiro_gols %>%
  inner_join(campeonato_brasileiro_full, by = c("partida_id" = "ID")) %>%
  filter(year(data) %in% 2020:2024 & clube == "Flamengo") %>%
  group_by(clube, partida_id, ano = year(data)) %>%
  summarize(gols_por_partida = n(), .groups = "drop") %>%
  group_by(clube, ano) %>%
  summarize(mean_gols = round(mean(gols_por_partida),2), .groups = "drop") %>%
  arrange(desc(mean_gols))

#Gráfico em linha da Média de gols por partida do Flamengo nos anos de 2020 a 2024
ggplot(data = MediaGolsMarcadosFlamengo, aes(x = factor(ano), y = mean_gols, group = 1)) + 
  geom_line(color = "black", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = mean_gols), 
            vjust = -1.0, 
            size = 4) +
  labs(
    title = "Média de gols por partida do Flamengo",
    subtitle = "Valores médios de gols por partida, de 2020 a 2024",
    x = "Ano",
    y = "Média de gols"
  ) +
  theme_minimal()

########################################################################

#Times que recebem mais cartões vermelhos vencem menos partidas?

#Cartões vermelhos por time em 2024
Vermelhos_por_time <- campeonato_brasileiro_cartoes %>%
  inner_join(campeonato_brasileiro_full, by = c("partida_id" = "ID")) %>%
  filter(cartao == "Vermelho" & year(data) == 2024) %>%
  group_by(clube) %>%
  summarise(total_vermelhos = n(), .groups = "drop") %>%
  arrange(desc(total_vermelhos))

#Vitórias por time em 2024
Vitorias_por_time <- campeonato_brasileiro_full %>%
  filter(vencedor != "-" & year(data) == 2024) %>%
  group_by(vencedor) %>%
  summarise(total_vitorias = n(), .groups = "drop") %>%
  arrange(desc(total_vitorias)) %>%
  rename(clube = vencedor)

#Juntar cartões e vitórias
Dados_resumo <- Vermelhos_por_time %>%
  inner_join(Vitorias_por_time, by = "clube")

#Gráfico de dispersão
ggplot(Dados_resumo, aes(x = total_vermelhos, y = total_vitorias)) +
  geom_point(color = "red", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  geom_text(aes(label = clube), 
            vjust = -1.0, 
            size = 4) +
  labs(
    title = "Em 2024, os times que receberam mais cartões vermelhos venceram menos partidas?",
    x = "Total de Cartões Vermelhos",
    y = "Total de Vitórias"
  ) +
  theme_minimal()