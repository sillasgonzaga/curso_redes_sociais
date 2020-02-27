library(rtweet)
library(tidyverse)
library(graphTweets)
library(igraph)
library(ggraph)

## Leituras recomendadas
# Como conseguir seu token:
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# Intro ao pacote rtweet:
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

#### Configuracao do token do twitter


appname <- "teste_curso"
key <- "HwXEwPrvobgO8hkHRTza23QEV"
secret <- "IX5jLUhEi9zaWRXqJCMvfrk16cJV276FiMOLm5pyFDmqljrg3V"

# criar token
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

#### Coleta dos dados --------------------

# Existem diversas abordagens de analise de redes no twitter, como:
# Redes de seguidores (cada vertice um usuario e cada aresta uma relação de follow)
# Redes de replies (cada aresta um tweet de um usuario respondendo ao outro)
# Redes de retweets (cada aresta um retweet)

# Neste projeto, faremos uma analise da rede de retweets, usando duas hashtags
# usadas de maneira bem polarizada: #lulapreso e #lulavalealuta
tweets_preso <- rtweet::search_tweets("#lulapreso", n = 500)
tweets_livre <- rtweet::search_tweets("#LulaValeALuta", n = 500)

# Os dataframes acima contem, em cada linha, um tweet que contem a hashtag especificada
# e mais algumas colunas de metadados sobre o tweet
glimpse(tweets_livre)

# juntar os dois dataframes em um so, criando uma coluna para identificar
# a hashtag do tweet
df_tweets <- bind_rows(tweets_livre %>% mutate(hashtag = "#LulaValeALuta"),
                       tweets_preso %>% mutate(hashtag = "#lulapreso"))

# opcional: salve o dataframe em um arquivo local para nao precisar rodar 
# a funcao de obter os tweets novamente. isso é uma boa ideia por conta dos limites
# da API gratuita do twitter
write_rds(df_tweets, "df_tweets_lula.Rds")
# carregar dataset salvo
df_tweets <- read_rds("df_tweets_lula.Rds")

## -------------
# analise rts

df_rts <- df_tweets %>%
  # filtrar apenas rts
  filter(is_retweet & retweet_count > 1) %>%
  # remover tweets duplicaos
  distinct(status_id, .keep_all = TRUE) %>% 
  # selecionar apenas colunas importantes
  select(status_id, created_at, screen_name, text, hashtag)

glimpse(df_rts)

extrair_origem_tweet <- function(tweet){
  x <- stringr::str_split(tweet, ":", simplify = TRUE)[1,1]
  x <- stringr::str_remove_all(x, "[RT @]")
  x
}

df_rts$text[1:3]
extrair_origem_tweet(df_rts$text[1:3])
unlist(lapply(df_rts$text[1:3], extrair_origem_tweet))

# criar nova coluna com identificao do autor retweetado
df_rts <- df_rts %>% 
  mutate(usuario_tweet_original = unlist(lapply(text, extrair_origem_tweet)))

# preparar dataframe de arestas
df_rts_cnt <- df_rts %>% 
  # selecionar colunas importantes
  select(usuario_rt = screen_name, usuario_tweet_original, hashtag) %>% 
  # agregar arestas para calcular frequencia
  count(usuario_rt, usuario_tweet_original, hashtag)

df_rts_cnt %>% arrange(desc(n))

grafo_rts <- graph_from_data_frame(df_rts_cnt, directed = TRUE)

vcount(grafo_rts); ecount(grafo_rts)

#  maiores influencers
degree(grafo_rts) %>% sort() %>% tail()
# degree(grafo_rts, v = "", mode = "out")

# extrair os usuarios com os 10 maiores degrees
dg <- degree(grafo_rts, mode = "in")

(maiores_influencers <- names(tail(sort(dg), 10)))
# criar atributo no grafo para identificar se o vertice é um influencer.
# caso nao seja, retorne um string vazio
V(grafo_rts)$influencer <- ifelse(V(grafo_rts)$name %in% maiores_influencers,
                                  V(grafo_rts)$name,
                                  "")

plot(grafo_rts, vertex.label = V(grafo_rts)$influencer)

# tentar com pacote ggraph
set.seed(1)
grafo_rts %>% 
  ggraph("nicely") +
  # criar arestas coloridas de acordo com a hashtag
  geom_edge_link(aes(color = hashtag),
                 arrow = grid::arrow(type = "closed", length = unit(.05, "inches"))) + 
  # criar vertices cujos tamanhos sao proporcionais ao degree
  geom_node_point(aes(size = dg)) + 
  # acrescentar nome dos influencers
  geom_node_text(aes(label = influencer), hjust = 1, vjust = 1) + 
  # mudar cores das arestas manualmente
  scale_edge_color_manual(values = c("blue", "red")) +
  ggraph::theme_graph()
