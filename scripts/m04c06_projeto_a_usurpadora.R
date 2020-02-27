# Modelagem da novela mexicana A Usurpadora como uma rede social
# Vertices: Personagens da novela
# Arestas: Co-ocorrência de dois personagens em uma mesma cena.

# Objetivo:
# 01: Analisar se as propriedades da rede da usurpadora são atípicas
# 02: Detectar núcleos de personagens por meio de Clusterização

library(tidyverse)
library(rvest) # web scraping
library(lubridate) 
library(igraph)
library(xml2)

#### coleta dos dados e construcao da rede (Opcional) ----
# https://ndenovela.wordpress.com/a-usurpadora/
# https://pt.wikipedia.org/wiki/La_usurpadora

url_roteiros <- "https://ndenovela.wordpress.com/a-usurpadora/"
# ler codigo fonte da pagina
cod_fonte_scripts <- url_roteiros %>% 
  xml2::read_html()

# extrair texto nos paragrafos
roteiro <- cod_fonte_scripts %>% 
  html_nodes("p") %>% 
  html_text()

# coletar elementos do vetor cujo tamanho é maior que 1
roteiro <- roteiro[nchar(roteiro) > 1]

# detetcar posições no vetor em que o elemento possui a palavra Capítulo
ind_capitulo <- which(str_detect(roteiro, "Capítulo "))

# duplicado: cap 56
# remover indice dos capitulos 56

extrair_num_capitulo <- function(x){
  output <- str_remove_all(x, "Capítulo ")
  output <- str_remove_all(output, " .*")
  output <- as.numeric(output)
  output
}

# montar dataframe com cabeçalho de capitulo e texto
df_usurp <- data.frame(
  capitulo = roteiro[ind_capitulo],
  texto = roteiro[ind_capitulo + 1],
  stringsAsFactors = FALSE
)

# por erro do site, o cap 56 veio duplicado
df_usurp <- df_usurp[-56,]
# corrigir rownames (56 deixa de existir, cria-se um buraco)
rownames(df_usurp) <- seq(1, nrow(df_usurp), 1)

# criar nova coluna com numero do capitulo
df_usurp$num_cap <- extrair_num_capitulo(df_usurp$capitulo)
# substituir Dr. por Dr
df_usurp$texto <- str_replace_all(df_usurp$texto, "Dr\\. ", "Dr")
# remover espaços desnecessarios
df_usurp$texto <- str_squish(df_usurp$texto)

# extrair frases dos capitulos
x <- df_usurp$texto[1]

# montar dataframe separando os textos dos capítulos em frases
df_usurp_frase <- df_usurp %>% 
  as.tibble() %>% 
  mutate(frase = str_split(texto, "\\. ")) %>% 
  select(-texto) %>% 
  unnest()

# conjunto de personagens de a usurpadora
usurpadora_wiki <- read_html("https://pt.wikipedia.org/wiki/La_usurpadora")

# ler tabela
tb <- usurpadora_wiki %>% 
  html_table(fill = TRUE)

# selecionar quinta tabela da pagina
personagens <- tb[[5]]
personagens <- as.character(personagens[,2])
# corrigir doutor 
personagens <- str_replace_all(personagens, "Dr\\. ", "Dr")
# remover virgulas
personagens <- str_replace_all(personagens, ",", "")
personagens <- word(personagens, 1)
# corrigir vovo piedade
personagens[4] <- "Piedade"
# remover personagens sem importancia
personagens
personagens <- personagens[-c(42, 43, 64, 66, 69)]

extrair_mencionados <- function(x){
  unique(personagens[str_detect(x, personagens)])
}

gerar_pares <- function(x){
  if (length(x) < 2) return(NULL)
  
  x <- sort(unique(x))
  x <- x %>% combn(m = 2, simplify = TRUE) %>% t()
  x <- as.tibble(x)
  colnames(x) <- c("P1", "P2")
  x
}

df_pares_personagens <- df_usurp_frase %>% 
  mutate(par = frase %>% map(extrair_mencionados) %>% map(gerar_pares)) %>% 
  select(num_cap, frase, par)

df_pares_personagens <- df_pares_personagens %>% 
  filter(map_dbl(par, length) > 0) %>% 
  unnest()

# construir rede
df_pares_personagens <- df_pares_personagens %>% 
  count(P1, P2, sort = TRUE)

#### analise da rede ----

# 01 - Simulação ----
g <- graph_from_data_frame(df_pares_personagens, directed = FALSE)
set.seed(123)
l <- layout_nicely(g)
plot(g, layout = l)
# Interpretação: o gráfico mostra que não se trata de uma rede muito densa. 
# Alguns personagens são mais marginalizados.
# As protagonistas Paola e Paulina aparecem no centro do grafo.
# Quem é a ponte da rede?
sort(betweenness(g, norm = TRUE))
# Aparentemente a betweenness da Paulina é muito maior que a dos demais.
# Vamos confirmar isso:
hist(betweenness(g, norm = TRUE))
# De fato, a betweeness da Paulina é muito maior que a dos demais personagens.
# Podemos quantificar o quão comum é esse resultado?

# Gerando 5000 redes aleatorias
lst_g_aleatorio <- replicate(5000,
                             sample_gnp(n = vcount(g),
                                        p = graph.density(g),
                                        directed = FALSE),
                             simplify = FALSE)

# Para cada rede da lista, retorne a maior betweeness dos vertices
lst_btn <- lapply(lst_g_aleatorio, betweenness, norm = TRUE)
max_btn <- sapply(lst_btn, max)
summary(max_btn)

# 02 - Detecção de nucleos na novela ----
cl1 <- cluster_fast_greedy(g, weights = E(g)$n/sum(E(g)$n))
cl2 <- cluster_fast_greedy(g)

par(mfrow = c(1, 2))
plot(cl1, g, layout = l, mark.groups = NULL)
plot(cl2, g, layout = l, mark.groups = NULL)
