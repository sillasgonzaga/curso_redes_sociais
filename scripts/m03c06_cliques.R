library(igraph)
library(tidyverse)
# carregando o dataset de amizades na universidade
df_arestas <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_arestas.csv")
df_vertices <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")

g <- graph_from_data_frame(d = df_arestas,
                           directed = FALSE,  # a rede nao é direcionada )
                           vertices = df_vertices)

#### funcoes de cliques ####

# cliques() retorna todos os possiveis subgrafos na rede com um tamanho minimo e maximo de vertices
clq <- igraph::cliques(g, min = 3)
clq
length(clq)

# largest_cliques() retorna todos os largest cliques em uma rede.
# um clique é "largest" se nao possui nenhum outro clique com mais vertices
clq <- igraph::largest_cliques(g)
clq
clq <- clq[[1]]
igraph::as_ids(clq)

# max_cliques() retorna todos os maximal cliques na rede.
# um clique é maximal se nao pode ser extendido para um clique maior
igraph::max_cliques(g)

#### destacar maior clique no grafico ####
E(g)$id <- 1:ecount(g) 
sub <- induced_subgraph(g, vids = igraph::as_ids(clq))
# fazer grafo apenas do clique
plot(sub)

plot(g, edge.color = ifelse(E(g)$id %in% E(sub)$id, "red", "gray"),
     vertex.color = ifelse(V(g)$name %in% V(sub)$name, "red", "gray"))
