library(igraph)
# carregar dataset
library(tidyverse)
df_arestas <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_arestas.csv")
df_vertices <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/turma_faculdade_vertices.csv")

g <- graph_from_data_frame(d = df_arestas,
                           directed = FALSE,  # a rede nao é direcionada )
                           vertices = df_vertices)

#### simulacao de redes ####

# o pacote igraph possui varios modelos de redes aleatorias.
# um deles é o Modelo G(N, p) de Erdős-Rényi, fornecido pela funcao sample_gnp

# vamos gerar 10000 redes aleatorias com os mesmos parametros N e p de nossa
# rede de estudo para analise suas propriedades

# parametros da Rede:
N <- vcount(g)
p <- edge_density(g) # equivalente a p <- graph.density(g)
N; p

# exemplo de rede aleatoria
set.seed(123) # garantir reprodutibildiade
g.aleat <- sample_gnp(n = N, p = p, directed = FALSE)
g.aleat
# plotar as redes lado a lado
par(mfrow = c(1, 2))
plot(g, vertex.label = NA, main = "Rede real")
plot(g.aleat, vertex.label = NA, main = "Rede aleatória")
dev.off()

# plotar
# comparar  grau medio
mean(degree(g))
mean(degree(g.aleat))

# comparar media dos caminhos mais curtos
mean_distance(g)
mean_distance(g.aleat)

# comparar transitividade
transitivity(g, type = "global")
transitivity(g.aleat, type = "global")

# gerar entao 10000 redes aleatorias e comparar as propriedades
grafos_aleatorios <- replicate(10000, sample_gnp(n = N, p = p, directed = FALSE),
                               simplify = FALSE)
grafos_aleatorios[[15]]

# calcular grau medio, diametro e transitividade das redes aleatorias
grau_medio_esperado <- sapply(grafos_aleatorios, function(x) mean(degree(x)))
avg_path_len_esperado <- sapply(grafos_aleatorios, mean_distance)
transit_esperado <- sapply(grafos_aleatorios, transitivity)

grau_medio_esperado
summary(grau_medio_esperado)

# plotar histograma da distribuicao dos valores em comparacao com o valor da rede real
hist(grau_medio_esperado)
abline(v = mean(degree(g)), col = "red")

# fazer esse processo para as propriedades
par(mfrow = c(3,1)) # layout de 3 linhas e 1 coluna
hist(grau_medio_esperado, main = "Grau médio")
abline(v = mean(degree(g)), col = "red")

hist(avg_path_len_esperado, main = "Caminho mais curto médio")
abline(v = mean_distance(g), col = "red")

hist(transit_esperado, main = "Transitividade")
abline(v = transitivity(g), col = "red")

# Como exercicio, conheça outros modelos de redes aleatorias fornecidos pelo
# pacote igraph

# http://igraph.org/r/doc/sample_gnm.html
# http://igraph.org/r/doc/sample_degseq.html
# http://igraph.org/r/doc/sample_islands.html


