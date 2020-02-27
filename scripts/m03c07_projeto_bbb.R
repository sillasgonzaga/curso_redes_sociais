### analise bbb
# O objetivo deste video é apresentar um exemplo pratico de aplicação dos conceitos 
# do modulo 03 em um dataset bem diferente: 
# a rede de relacionamentos entre brothers e sisters do bbb 2018

# Leia o meu blog post para entender como o dataset foi construido
# http://sillasgonzaga.com/post/bbb-no-r/

library(tidyverse)
library(igraph)

# importar os dados
df_bbb <- readr::read_csv2("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/bbb_arestas.csv")
# olhando o dataset
df_bbb
# construir dataset de sexo
participantes <- c("Ayrton", "Ana Clara", "Ana Paula", "Breno", "Caruso", "Diego",
                   "Gleici", "Jaqueline", "Jéssica", "Kaysar", "Lucas", "Mahmoud",
                   "Mara", "Nayara", "Patrícia", "Paula", "Viegas", "Wagner")

sexo = c("M", "F", "F", "M", "M", "M", "F", "F", "F", "M", "M", "M", "F", "F",
         "F", "F", "M", "M")

df_vertices <- data.frame(participante = participantes,
                          sexo = sexo)

g <- graph_from_data_frame(df_bbb, directed = FALSE, vertices = df_vertices)

# Dando uma olhada na rede. Parece ser bem densa!
plot(g)
#### propriedades da rede ----
# grau medio
sort(degree(g, normalized = TRUE))
par(mfrow = c(2,1))
hist(degree(g, normalized = FALSE), main = "Distribuição dos graus da rede do BBB 2018")
hist(degree(g, normalized = TRUE), main = "Distribuição dos graus da rede do BBB 2018")
# INTERPRETAÇÃO
# A distribuição da rede confirma nossa observação inicial sobre a densidade da rede:
# A maioria dos participantes interage com pelo menos 80% do resto da casa.
# Considerando o contexto, esses resultados eram esperados? Sim ou não? 

# densidade
graph.density(g); mean(degree(g))/(vcount(g)-1)
cohesion(g)
# distancia media
igraph::mean_distance(g)
# diametro
igraph::diameter(g) 
# para saber o responsavel pelo diametro
igraph::get.diameter(g)
# transitividade: a probabilidade de que 2 pares de vertices da rede sejam conectados
igraph::transitivity(g)
# assortatividade
sexo_num <- ifelse(V(g)$sexo == "M", 1, 2)
igraph::assortativity(g, types1 = sexo_num)
# Interpretação: de acordo com o resultado acima, o sexo dos participantes é um
# fator importante na rede?


#### centralidades de vertices ----
# construir dataframe com resultados
df_centralidade <- data.frame(
  participante = V(g)$name,
  degree = degree(g, norm = TRUE),
  close = closeness(g, norm = TRUE),
  btn = betweenness(g, norm = TRUE)
)
# Jessica e Kaysar são os membros mais centrais, mas isso pode querer dizer
# pouco em uma rede tão densa.

View(df_centralidade)
# matriz de correlacao
cor(df_centralidade[,-1])
pairs(df_centralidade[,-1])


#### cliques ----
# Revisao: são subgrafos em uma rede onde todos os seus vertices estao conectados entre si
# usando cliques para encontrar panelinhas
panelas <- igraph::largest_cliques(g)
# quantas panelas temos?
panelas
length(panelas) # Foram encontrados 4 cliques com 13 vertices

# fazer matriz de graficos para cada clique
par(mfrow = c(2,2))
# criar atributo de identificacao para cada aresta
E(g)$id <- 1:ecount(g) 
# inicializar layout para manter o mesmo posicionamento nos 4 graficos
l <- layout_nicely(g)
# iniciar loop
for (i in 1:length(panelas)){
  panela_loop <- panelas[[i]]
  sub <- induced_subgraph(g, vids = igraph::as_ids(panela_loop))
  plot(g,
       edge.color = ifelse(E(g)$id %in% E(sub)$id, "red", "gray"),
       vertex.color = ifelse(V(g)$name %in% V(sub)$name, "red", "gray"),
       layout = l)
}

# encontrar participantes que nao estao em nenhuma panela
paneleiros <- unique(unlist(lapply(panelas, as_ids)))
V(g)$name[!V(g)$name %in% paneleiros]

panelas
