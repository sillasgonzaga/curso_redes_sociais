library(igraph)
library(igraphdata)

#### graficos interativos ####
library(visNetwork) # install.packages("visNetwork")
data(karate)

# leiam os tutoriais do pacote:
# http://datastorm-open.github.io/visNetwork/

# criar objeto visNetwork a partir de um grafo do igraph
g2 <- toVisNetworkData(g)
str(g2)
# salvar sexo como atributo grupo da rede
g2$nodes$group <- g2$nodes$sexo

visNetwork(nodes = g2$nodes, edges = g2$edges)
?visNetwork

grafo <- visNetwork(nodes = g2$nodes, edges = g2$edges) %>% 
  visNodes(physics = FALSE, group = "sexo") %>% 
  visEdges(physics = FALSE) %>% 
  visLegend() %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") 
  
htmlwidgets::saveWidget(grafo, "grafo_interativo.html")


#### exportar para gephi ####
library(rgexf) # install.packages("rgexf")

gephi <- rgexf::igraph.to.gexf(g)
print(gephi, "gephi.txt")
