#devtools::install_github("brunaw/vagalumeR")
library(vagalumeR)
library(tidyverse)
library(rvest)
library(igraph)
library(magrittr)
library(ggraph)

#### usar sua key aqui ####
#key <- "sua_key_aqui"
key <- "c43cb31102b8a3f48aa16d1392913bf4"
key <- as.list(key)


# extrair top 100 artistas nacionais por meio da API do vagalume
# https://api.vagalume.com.br/docs/rank/artistas/
type <- "art"
period <- "month"
periodVal <- "201801"
scope <- "nacional"
limit <- 100 # minimo 10 e maximo 100
url_req <- "https://api.vagalume.com.br/rank.php?type={type}&period={period}&periodVal={periodVal}&scope={scope}&limit={limit}&apikey={key}"
?str_glue
url_req <- str_glue(url_req)
url_req

# vamos agora extrair o json gerado pela requisicao
req <- httr::GET(url_req)
json <- httr::content(req)
# dando uma olhada no objeto gerado
# View(json)
# extraindo o objeto que queremos
lst_artistas <- json$art$month$nacional
# qual é a classe de cada objeto dentro da lista?
lapply(lst_artistas, class)
# exemplo:
lst_artistas[[1]]
# converter para dataframe é facil
as.data.frame(lst_artistas[[1]], stringsAsFactors = FALSE)
# vamos aplicar essa transformar a todos os elementos da lista e reduzir a lista
# a um dataframe so
df_top_100 <- lapply(lst_artistas, as.data.frame, stringsAsFactors = FALSE)
df_top_100 <- Reduce(rbind, df_top_100)
glimpse(df_top_100)

# remover colunas que nao preciso e transformar colunas de unique a rank para numerico
df_top_100 <- df_top_100 %>% 
  select(-pic_small, -pic_medium) %>% 
  mutate(uniques = as.numeric(uniques),
         views = as.numeric(views),
         rank = as.numeric(rank))

# coletar related artistas para cada artista
extrair_nome_da_url <- function(url_artista){
  x <- str_replace_all(url_artista, "https://www.vagalume.com.br", "")
  x <- str_replace_all(x, "/", "")
  x
}
# mostrando um exemplo de como a função funciona
df_top_100$url[1:3]
extrair_nome_da_url(df_top_100$url[1:3])
# aplicar a funcao para todos as urls do dataframe
df_top_100 <- df_top_100 %>% 
  mutate(artist_short_url = extrair_nome_da_url(url))

# agora estamos prontos para coletar os artistas relacionados a cada um do top 100
# para isso, vamos usar essa funcao
extrair_relacionados <- function(artista_busca){
  url_req <- paste0("https://www.vagalume.com.br/", artista_busca, "/index.js")
  req <- httr::GET(url_req)
  json <- httr::content(req, encoding = "UTF-8", as = "text")
  cont <- jsonlite::fromJSON(json)
  lst <- cont$artist
  artist_name <- lst$desc
  artist_id = lst$id
  
  rel <- lst$related
  cols <- c("id", "name", "url")
  
  if(!is.null(rel) && length(rel) > 0 && cols == names(rel)){
    df_related <- rel %>% 
      select(related_id = id,
             related_name = name,
             related_short_url = url) %>% 
      mutate(artist_short_url = artista_busca,
             artist_name = artist_name,
             artist_id = artist_id) %>% 
      mutate(related_short_url = str_replace_all(related_short_url, "/", ""))
    return(df_related)
  } else {
    # em caso de erro, exiba uma mensagem e um objeto vazio
    print(paste0(artista_busca, " nao possui artistas relacionados"))
    return(NULL)
  }
  
}

# um exemplo de como a funcao roda:
extrair_relacionados("anitta")
extrair_relacionados("thiago-matheus")
# aplicar em varios artistas:
#extrair_relacionados(df_top_100$artist_short_url)

# opa, deu erro!
# isso é pq a funcao relatedInfo só aceita um artista por vez
# precisamos entao iterar sobre todos os artistas, um de cada vez
# uma das formas de fazer isso é com a função lapply

# link para quem deseja conhecer mais sobre essa funcao
# http://www.dataperspective.info/2016/03/apply-lapply-rapply-sapply-functions-r.html
df_relacionados <- lapply(df_top_100$artist_short_url, extrair_relacionados)
df_relacionados
df_relacionados <- Reduce(rbind, df_relacionados)
glimpse(df_relacionados)

# montar dataframe de arestas
df_arestas <- df_relacionados %>%
  select(from = artist_name, to = related_name) %>% 
  # remover duplicatas
  count(from, to)

df_arestas
summary(df_arestas$n)


### extrair genero dos artistas ###
# a partir do genero dos artistas, podemos fazer algumas analises interessantes
# principalmente visualizacoes

# gerar um vetor com todos os artistas
vetor_todos_artistas <- unique(c(df_relacionados$related_short_url,
                                 df_relacionados$artist_short_url))

#### extrair genero dos artistas do dataframe de vertices

# criar funcao para extrair genero dos artistas
extrair_genero_musical <- function(artist, n_genres =1){
  # voces vao entender esse n_genres daqui a pouco
  req <- httr::GET(paste("https://www.vagalume.com.br/", artist,
                         "/index.js"))
  json <- httr::content(req, encoding = "UTF-8", as = "text")
  cont <- try({jsonlite::fromJSON(json)})
  #cont <- jsonlite::fromJSON(json)
  
  if (inherits(cont, "try-error")) return(NULL)
  
  lst <- cont$artist
  artist_name <- lst$desc
  artist_id = lst$id
  ### extrair genero(s) do artista
  genre <- lst$genre
  if (!is.null(genre)) {
    genre_name <- head(genre$name, n_genres)
  } else {
    genre_name <- NA
  }

  df_genre <- tibble(genre_name) %>%
    mutate(artist_short_url = artist, name = artist_name, id = artist_id)

  return(df_genre)
  
}


extrair_genero_musical("anitta", 1)
extrair_genero_musical("anitta", 3)

# novamente, precisamos iterar sobre todos os artistas
lst_genero <- lapply(vetor_todos_artistas, extrair_genero_musical)
lst_genero
# para saber em quem deu erro:
sapply(lst_genero, is.null)
which(sapply(lst_genero, is.null))
vetor_todos_artistas[which(sapply(lst_genero, is.null))]

# prosseguindo:
# um metodo alternativo de concatenar uma lista em um dataframe (ao inves de Reduce)
df_genero <- bind_rows(lst_genero)
head(df_genero)

# finalmente, montar dataframe com dados dos vertices

# juntar os dois dataframes df_top_100 e df_genero pelas colunas que possuem 
# em comum
glimpse(df_genero)
glimpse(df_top_100)
df_vertices <- left_join(df_genero, df_top_100, by = c("id", "name", "artist_short_url")) %>% 
  # colocar a coluna de nome do artista primeiro
  select(name, everything())

head(df_vertices)

# com isso, ja podemos partir para a analise de redes propriamente dita

#### analise de redes ####
g <- graph_from_data_frame(df_arestas, directed = FALSE, vertices = df_vertices)

# ih, deu erro!
# mas felizmente a mensagem de erro é clara. 
# tem artista que esta presente nas arestas mas nao no dataframe de vertices

# vamos investigar
df_arestas %>% 
  filter(!from %in% df_vertices$name | !to %in% df_vertices$name)

# vamos entao remover as arestas cujos vertices nao estao presentes no dataframe
df_arestas <- df_arestas %>% 
  filter(!(!from %in% df_vertices$name | !to %in% df_vertices$name))


# tentando novamente
g <- graph_from_data_frame(df_arestas, directed = FALSE, vertices = df_vertices)
g

plot(g, vertex.label = NA)


# não muito interessante, não é mesmo?
# vamos então colorir os vertices de acordo com o genero do artista
# mas temos generos demais, o que vai dificultar a visualizacao de cores
unique(V(g)$genre_name)

# vamos entao selecionar apenas os 10 principais generos e chamar o restante
# de "Outros"

# criar dicionario de cores baseado em genero

n <- 11
generos_populares <- rev(sort(table(V(g)$genre_name)))
generos_populares
generos_populares <- generos_populares[1:n]
generos_populares <- names(generos_populares)

# salvar esse novo atributo no grafo
V(g)$genero_geral <- ifelse(V(g)$genre_name %in% generos_populares, V(g)$genre_name, "Outros")

# outra coisa para deixar o grafico mais elegante será exibir o nome dos artistas,
# mas apenas dos top 100. para todos os outros, exibir string em branco.
V(g)$name

V(g)$nome_top_100 <- ifelse(V(g)$artist_short_url %in% df_top_100$artist_short_url,
                            V(g)$artist_short_url,
                            "")

#### agora sim podemos partir para a visualizacao
# contudo, ao inves de o fazer pelo igraph, vamos experimentar um  outro
# pacote, o ggraph, que faz gráficos de redes com a sintaxe do ggplot2

# carregar pacote
library(ggraph)


# garantir reprodutibilidade do grafo
set.seed(1)
g %>%
  # iniciar grafico
  ggraph("nicely") +
  # plotar arestas, adicionar alpha para as deixar mais transparentes
  geom_edge_fan(alpha = 0.1) +
  # adicionar vertices, coloridos pelo genero
  geom_node_point(aes(color = genero_geral), size = 2.3) + 
  # mudar tema (aparencia) do grafico
  ggraph::theme_graph() + 
  # acrescentar nomes dos artistas
  ggraph::geom_node_text(aes(label = nome_top_100), size = 2.7, repel = TRUE) +
  # mudar paleta das cores dos pontos
  scale_color_brewer(palette = "Paired") + 
  # mudar titulo da legenda
  labs(color = "Gênero") + 
  # colocar fundo cinza
  theme(plot.background = element_rect(fill = RColorBrewer::brewer.pal(9, "Greys")[3]))

# salvar grafico em alta qualidade
ggsave("sna-musica-br.png", width = 17, height = 14)  

# Rosa de Saron parece ser um artista muito relevante para a musica,
# funcionando como uma ponte entre artistas reliogiosos e não religiosos

# quais sao os vizinhos do Rosa de Saron
neighbors(g, "Rosa de Saron")

# se fizessemos uma mini rede em torno do Rosa de Saron, como seria ela?

# criar matriz de distancias entre artistas
mat_dist <- shortest.paths(g)
vizinhos <- mat_dist[, "Rosa de Saron"]
vizinhos <- vizinhos[vizinhos <= 2]
vizinhos
vizinhos <- names(vizinhos)

g.rosa_saron <- induced_subgraph(g, vizinhos)

                     
plot(g.rosa_saron, vertex.label = V(g.rosa_saron)$artist_short_url)
  
# nao ficou muito legal. vamos tentar novamente com o ggraph
g.rosa_saron %>% 
  ggraph("nicely") +
  geom_edge_fan(alpha = 0.1) +
  geom_node_point(aes(color = genero_geral), size = 2.3) + 
  ggraph::theme_graph() + 
  ggraph::geom_node_text(aes(label = name), size = 2.7, repel = TRUE) +
  scale_color_brewer(palette = "Paired") + 
  labs(color = "Gênero") + 
  theme(plot.background = element_rect(fill = RColorBrewer::brewer.pal(9, "Greys")[3]))

#### analise de centralidade ####

# neste especifico caso de rede, betweenness explica muito bem a importancia
# de cada artista
sort(betweenness(g))
# existe uma correlacao entre os indices de popularidade do vagalume e 
# as metricas de centralidade?
df_centralidade <- data.frame(
  artist_short_url = V(g)$artist_short_url,
  btn = betweenness(g),
  dg = degree(g),
  cln = closeness(g),
  stringsAsFactors = FALSE
)

head(df_centralidade)

df_correlacao <- left_join(df_top_100, df_centralidade, by = "artist_short_url")
head(df_correlacao)

# selecionar colunas numericas
df_correlacao <- df_correlacao %>% select_if(is.numeric)
# remover linhas sem dados
df_correlacao <- na.omit(df_correlacao)
head(df_correlacao)
# calcular matriz de correlacao
cor(df_correlacao)

#### analise de diametro da rede ####

# Notamos pelo grafico que essa rede é bem esparsa, com um diametro muito alto
# qual o maior trajeto que pode ser percorrido no grafo?
diameter(g)
maior_caminho <- get.diameter(g)
maior_caminho

# que tal entao destacar esse caminho no grafico? senta que la vem codigo...
# vamos fazer uma serie de tarefas relativamente complexas. 
# segue o passo a passo

# primeiro, extrair os nomes dos vertices que pertencem ao diametro
artistas_maior_caminho <- igraph::as_ids(maior_caminho)
# gerar uma lista de combinacoes 2 a 2
artistas_maior_caminho <- combn(artistas_maior_caminho, 2, simplify = FALSE)
# buscar arestas formadas por cada um desses pares de vertices
# por exemplo
E(g)["Anitta" %--% "Belo"]
E(g)["Anitta" %--% "Priscila Nocetti"]

lst_arestas <- lapply(artistas_maior_caminho, function(x){
  E(g)[x[1] %--% x[2]]
})
lst_arestas
# concatenar (reduzir) a lista em um vetor de arestas
lst_arestas <- Reduce(c, lst_arestas)
lst_arestas

# exibir apenas nomes dos que fazem parte do diametro
V(g)$nome_diametro <- ifelse(V(g)$name %in% unlist(artistas_maior_caminho),
                             V(g)$name,
                             "")

E(g)$cor_aresta <- ifelse(E(g) %in% lst_arestas, "red", "gray")

set.seed(1)
g %>% 
  ggraph("nicely") +
  geom_edge_fan(alpha = 0.7, aes(color = cor_aresta)) +
  scale_edge_color_identity() +
  geom_node_point(aes(color = genero_geral), size = 2.3) + 
  ggraph::theme_graph() + 
  # exibir apenas nomes dos que fazem parte do diametro
  ggraph::geom_node_text(aes(label = nome_diametro), size = 2.7, repel = TRUE) +
  scale_color_brewer(palette = "Paired") + 
  labs(color = "Gênero") + 
  theme(plot.background = element_rect(fill = RColorBrewer::brewer.pal(9, "Greys")[3]))

