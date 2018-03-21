#### Aula 01: Como criar uma rede no R ####
library(tidyverse)
library(igraph)

#### Introducao ####

# Uma rede social no R pode ser criada de diferentes maneiras.
# A mais comum e facil de modelar um grafo é definir uma edgelist, ou seja,
# uma tabela de duas colunas corresponde a uma interacao entre o nó da primeira com o
# da segunda.

#### Importacao dos dados ####

# Os dados abaixo se referem a uma rede social de amizades de uma turma de faculdade.
# Os nomes das pessoas foram anonimizados

df_vertices <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_nos.csv")
df_vertices <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")
# O dataframe df_nos traz dados referentes às interacoes entre nos/pessoas
head(df_vertices)
# O dataframe df_nos informa metadados sobre os nós/agentes da rede
head(df_nos)

# Existe uma funcao pronta para transformar os dados acima em rede: se chama...
?graph_from_data_frame
g <- graph_from_data_frame(d = df_vertices,
                           directed = FALSE,  # a rede nao é direcionada )
                           vertices = 
