# https://rpubs.com/shestakoff/sna_lab4

library(igraph)
library(igraphdata)

#### conhecendo o dataset ####
data("kite")
kite
?kite
plot(kite, vertex.size = 25, vertex.label.cex = .7)

# kite é uma rede social fictícia que possui 10 vértices.
# é o menor grafo em que o vértice mais central é diferente de acordo com 
# cada métrica de centralidade

#### degree ####
dg <- degree(kite)
dg
plot(kite, vertex.label = dg)
# colorir os pontos em escala de cinza (quanto mais escuro, maior a centralidade)
palette_cinza <- gray.colors(vcount(kite))
rank_inverso <- rank(-dg, ties.method = "first")
cor_vertice <- palette_cinza[rank_inverso]

plot(kite, vertex.color = cor_vertice)
#V(kite)$color <- dg
#kite$palette <- grayco

plot(kite, vertex.label = dg,
     vertex.color = ifelse(dg == max(dg), "yellow", "gray"))

#### closeness ####
cln <- closeness(kite)
cln
# plotar
rank_inverso <- rank(-cln, ties.method = "min")
cor_vertice <- palette_cinza[rank_inverso]
plot(kite, vertex.label = round(cln, 3), vertex.color = cor_vertice)

#### betweenness ####
btn <- betweenness(kite)
btn
# plotar
rank_inverso <- rank(-btn, ties.method = "min")
cor_vertice <- palette_cinza[rank_inverso]
plot(kite, vertex.label = round(btn, 1), vertex.color = cor_vertice)

#### eigenvector centrality ####
ev <- eigen_centrality(kite)$vector
ev
# plotar
rank_inverso <- rank(-ev, ties.method = "min")
cor_vertice <- palette_cinza[rank_inverso]
plot(kite, vertex.label = round(ev, 3), vertex.color = cor_vertice)


#### mostrar a diferenca das metricas de centralidade em um grafico so ####

# funcao para automatizar o processo
grafico_metrica <- function(metrica, titulo_grafico){
  rank_inverso <- rank(-metrica, ties.method = "min")
  cor_vertice <- palette_cinza[rank_inverso]
  plot(kite, vertex.label = V(kite)$name, vertex.color = cor_vertice,
       main = titulo_grafico)
}

par(mfrow = c(1, 3)) # matriz de graficos de 1 linha e 3 colunas
grafico_metrica(dg, "Degree")
grafico_metrica(cln, "Closeness")
grafico_metrica(btn, "Betweenness")
