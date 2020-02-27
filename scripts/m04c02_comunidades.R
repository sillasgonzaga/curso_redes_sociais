library(igraph)
library(igraphdata)

data(karate)
?karate

#### algoritmo edge.betweenness ####
cm <- cluster_edge_betweenness(karate)
# investigando o objeto criado
print(cm)
class(cm)
help("communities")
length(cm) # quantidade de comunidades encontradas
sizes(cm) # quantidade de vertices por comunidade
membership(cm) # retorna a comunidade obtida para cada vertice
communities(cm) # semelhante a membership()
modularity(cm) # modularidade
# interpretacao: existe 34,5% de chance a mais que uma aresta pertença à uma comunidade
# do que uma aresta aleatória
crossing(cm, karate) # retorna as arestas que conectam vertices de diferentes comunidades
code_len(cm)
as.dendrogram(cm) # representacao hierarquica das comunidades
plot(as.dendrogram(cm))
plot(cm, karate) # plotar o resultado das comunidades

# exercicio: teste os comandos acima usando outros algoritmos de clusterizacao
# presentes no igraph#como:


#cluster_fast_greedy()
#cluster_label_prop()
#cluster_leading_eigen()
#cluster_louvain()
#cluster_optimal()
#cluster_spinglass()
#cluster_walktrap()


