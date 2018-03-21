#### Aula 01: Como criar uma rede no R ####
library(tidyverse)
library(igraph)

#### Importacao dos dados
df_nos <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_nos.csv")
df_vertices <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")