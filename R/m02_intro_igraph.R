#### Aula 01: Como criar uma rede no R ####
library(tidyverse)
library(igraph)

# Supondo uma rede simples de relacionamentos
df <- tibble(
  pessoa1 = sample(LETTERS[1:10], 30, replace = TRUE),
  pessoa2 = sample(LETTERS[1:10], 30, replace = TRUE)
)

df <- df %>% filter(pessoa1 != pessoa2)
df <- unique(df)

graph_from_data_frame(df) %>% plot(vertex.size = 10, edge.arrow.size = 0.5)