#### carregar dados ####
library(igraph)
library(tidyverse)
df_arestas <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_arestas.csv")
df_vertices <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")

g <- graph_from_data_frame(d = df_arestas,
                           directed = FALSE,  # a rede nao é direcionada )
                           vertices = df_vertices)
plot(g)
#### redes direcionadas e nao direcionadas ####
# checar se um grafo é direcionado é muito simples:
igraph::is.directed(g)

#### grau medio e distribuicao de graus ####
# primeiro, extraindo o grau de cada vertice do grafo
graus <- igraph::degree(g)
graus
sort(graus)
rev(sort(graus))
# para calcular o grau medio, basta aplicar a funcao mean() no vetor de graus
grau_medio <- mean(graus)
grau_medio
# tambem é muito util analizar a distribuicao de graus por meio de um histograma
hist(graus)

#### densidade ####
# o igraph tem uma funcao pronta para calcular a densidade de um grafo
igraph::graph.density(g)
# interpretacao: existem apenas 37,5% de todas as conexões que poderiam existir
# na rede

#### coesao ####
coesao <- igraph::cohesion(g)

# interpretacao: basta remover 3 pessoas dessa rede para a tornar menos coesa
maiores_graus <- rev(sort(graus))[1:coesao]
igraph::graph.density(g.fraco)

# plotar as duas redes
par(mfrow = c(1, 2)) # criar layout de duas colunas de graficos
#hist(rnorm(500))
plot(g, main = "Rede completa")
plot(g.fraco, main = "Rede menor")
dev.off()

#### comprimento medio do trajeto ou caminho ####
igraph::mean_distance(g)

#### diametro ####
igraph::diameter(g) 
# para saber o responsavel pelo diametro
igraph::get.diameter(g)


#### transitividade ####
igraph::transitivity(g)

# interpretacao: existe uma probabilidade de que 2 pessoas da rede tenham um amigo em comum

#### assortatividade ####
sexo_num <- ifelse(V(g)$sexo == "M", 1, 2)
igraph::assortativity(g)


#### reciprocidade ####
# reciprocidade nao faz sentido para redes nao direcionadas
igraph::reciprocity(g)
# por isso, vamos usar outro grafo de exemplo
library(igraphdata) # install.packages("igraphdata")
data("UKfaculty")
?UKfaculty

plot(UKfaculty, vertex.label = NA, edge.arrow.size = 0.2, vertex.size = 10)
igraph::reciprocity(UKfaculty)


