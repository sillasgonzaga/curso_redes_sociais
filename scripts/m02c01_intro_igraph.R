#### Aula 01: Como criar uma rede no R ####
library(tidyverse)
library(igraph)
library(igraphdata)

#### Introducao

# Uma rede social no R pode ser criada de diferentes maneiras.
# A mais comum e facil de modelar um grafo é definir uma edgelist, ou seja,
# uma tabela de duas colunas corresponde a uma interacao entre o nó da primeira com o
# da segunda.

#### Importacao dos dados

# Os dados abaixo se referem a uma rede social de amizades de uma turma de faculdade.
# Os nomes das pessoas foram anonimizados

df_arestas <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_arestas.csv")
df_vertices <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")
# O dataframe df_arestas traz dados referentes às interacoes entre nos/pessoas
head(df_arestas)
# O dataframe df_vertices informa metadados sobre os nós/agentes da rede
head(df_vertices)

# Existe uma funcao pronta para transformar os dados acima em rede: se chama...
?graph_from_data_frame
g <- graph_from_data_frame(d = df_arestas,
                           directed = FALSE,  # a rede nao é direcionada )
                           vertices = df_vertices)

### Funcoes basicas ####
plot(g)
g
E(g) # arestas da rede
V(g) # vertices da rede
# analisar vizinhança de um vertice
neighbors(g, "P11")
adjacent_vertices(g, "P11")
neighborhood(g, order = 2, "P11")
are.connected(g, "P12", "P34")
are.connected(g, "P12", "P23")
# adicionar um atributo
V(g)
V(g)$cor <- ifelse(V(g)$sexo  == "M", "lightblue", "pink")
# subset em uma rede
V(g)[sexo  != "M"]
V(g)[cor  == "pink"]
# subgrafo
E(g)[inc("P11")]
sub <- subgraph.edges(g, E(g)[inc("P11")])
plot(sub) # repetir plot N vezes

#### Video 02 #### 

plot(g, vertex.color = V(g)$cor)
plot(g, vertex.size = 10)
plot(g, vertex.label = NA)
plot(g, edge.color = "gray")
plot(g, vertex.label.color = "black")
plot(g, edge.lty = "dashed")
plot(g, frame = TRUE)
plot(g, main = "Grafo de uma turma de faculdade",
     sub = "Curso de redes do IBPAD")

# destacar apenas o mais e o menos central
sort(degree(g))
V(g)$name
plot(g, vertex.label = ifelse(V(g)$name %in% c("P10", "P12"), V(g)$name, NA))

plot(g)
plot(g, vertex.color = V(g)$cor,vertex.size = 10, vertex.label = NA,
     edge.color = "gray", vertex.label.color = "black", edge.lty = "dashed",
     main = "Grafo de uma turma de faculdade", sub = "Curso de redes do IBPAD"
     )

# salvar o grafo
png("meu_grafo.png")
set.seed(123)
plot(g, vertex.color = V(g)$cor,vertex.size = 10, vertex.label = NA,
     edge.color = "gray", vertex.label.color = "black", edge.lty = "dashed",
     main = "Grafo de uma turma de faculdade", sub = "Curso de redes do IBPAD"
)
dev.off()
#### Video 03 ####

#### layout de uma rede
# http://igraph.org/r/doc/

plot(g, vertex.color = V(g)$cor,vertex.size = 10, vertex.label = NA)

l <- layout_nicely(g)
plot(g, vertex.color = V(g)$cor,vertex.size = 10, vertex.label = NA,
     layout = l)

l <- layout_as_tree(g)
l <- layout_with_fr(g)
l <- layout_with_gem(g)

# como automatizar o processo
mudar_layout <- function(funcao_layout){
  plot(g, vertex.color = V(g)$cor,vertex.size = 10, vertex.label = NA,
       layout = funcao_layout(g))
}

mudar_layout(layout.davidson.harel)
mudar_layout(layout_with_graphopt)
mudar_layout(layout_with_kk)
mudar_layout(layout_on_sphere)
mudar_layout(layout_in_circle)

