library(tidyverse)
library(igraph)

# baixe os arquivos do link abaixo para seu computador
# https://www.kaggle.com/secareanualin/football-events

# dataframe de eventos ocorridos nas partidas
df <- read_csv("/home/sillas/R/data/futebol/events.csv")
#  dataframe de metadados das partidas
df_inf <- read_csv("/home/sillas/R/data/futebol/ginf.csv")
head(df)
head(df_inf)
# selecionar colunas importantes
df_inf <- df_inf[, c("id_odsp", "date", "season")]
# unir os dois dataframes
df <- inner_join(df, df_inf, by = 'id_odsp')
head(df)
# filtrar temporada mais recente mais completa
table(df$season)
df_2016 <- df %>% filter(season == 2016)
# filtrar eventos de gols marcados
df_2016 <- df_2016 %>% filter(is_goal == 1)
nrow(df_2016)
# conferir times mais presentes no dataset
sort(table(df_2016$event_team))

# restringir analise a real madrid e barcelona
times_analise <- c("Real Madrid", "Barcelona")
df_2016 <- df_2016 %>% filter(event_team %in% times_analise)

# montar dataset apenas com colunas importantes para a ARS
df_edges <- df_2016 %>% select(from = player2, to = player, team = event_team)
df_edges
# remover gols sem assistencia
df_edges <- df_edges %>% filter(!is.na(from))
# remover redundancias
df_edges <- df_edges %>% count(from, to, team, sort = TRUE)
df_edges

# separar os dataframes em times 
lst_df_por_time <- split(df_edges, df_edges$team)
lst_df_por_time[[1]]
lst_df_por_time[[2]]
# construir um grafo para cada time
lst_grafo_por_time <- lapply(lst_df_por_time, graph_from_data_frame, directed = TRUE)
lst_grafo_por_time[[1]]
lst_grafo_por_time[[2]]

# plotar grafos
plotar_grafo <- function(grafo, titulo){
  plot(grafo, edge.arrow.size = 0.1, main = titulo)
}
par(mfrow = c(1,2))
plotar_grafo(lst_grafo_por_time$Barcelona, "Barcelona")
plotar_grafo(lst_grafo_por_time$`Real Madrid`, "Real Madrid")
dev.off()

# Quem é mais dependente: O Real Madrid do C. Ronaldo ou o Barcelona do Messi?

extrair_centralidade_jogador <- function(grafo, jogador){
  dg <- igraph::degree(grafo, v = jogador, mode = "in", normalized = TRUE)
  btn <- igraph::betweenness(grafo, v = jogador, weights = E(grafo)$n)
  cln <- igraph::closeness(grafo, vids = jogador, mode = "in",
                           weights = E(grafo)$n, normalized = TRUE)
  # retornar vetor com as metricas
  out <- c(dg, btn, cln)
  names(out) <- c("degree", "betweenness", "closeness")
  return(out)
}

extrair_centralidade_jogador(lst_grafo_por_time$Barcelona, "lionel messi")
extrair_centralidade_jogador(lst_grafo_por_time$`Real Madrid`, "cristiano ronaldo")

# Temos entao a resposta: usando a abordagem de ARS, pode-se concluir que o Real Madrid
# é mais dependente de Ronaldo
