library(tidyverse)
library(lubridate)



#### dplyr ####
df <- read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/scielo_amostra.csv")

# Tres maneiras de visualizar um dataset
glimpse(df)
head(df)
View(df)

## 5 principais verbos do dplyr: select, filter, mutate, group_by e arrange

# select
?select
df2 <- df %>% select(doc_year = document_publishing_year, author_institution)
head(df2)
rm(df2)

### filter
?dplyr::filter
## comparacao exata
df_ufs <- df %>% filter(author_institution == "Universidade Federal de Sergipe")
## comparacao com numeros
df %>% filter(document_publishing_year >= 2015) %>% select(document_id, document_publishing_year)
## detectar palavra e retornar a negação do resultado
?str_detect
df %>% filter(!str_detect(author_institution, "Universidade")) %>% select(author_institution)


### mutate: serve para criar novas variaveis ou alterar uma ja existente
lubridate::today()
today()
year(today())
?mutate
df <- df %>% mutate(intervalo_tempo = year(today()) - document_publishing_year)
glimpse(df)
# modificar uma coluna ja existente
df %>% mutate(journal = str_to_lower(author_institution),
              area = word(area, 1))
# remover coluna criada
df <- df %>% select(-intervalo_tempo)

### group_by + summarise
# contar linhas por ano
df %>% 
  summarise(qtd = n())

df %>% 
  group_by(document_publishing_year) %>% 
  summarise(qtd_artigos = n())

# classificar em ordem decrescente pelo ano
df %>% 
  group_by(document_publishing_year) %>% 
  summarise(qtd_artigos = n_distinct(document_id)) %>% 
  arrange(desc(document_publishing_year))

# classificar em ordem decrescente pela contagem
df %>% 
  group_by(author) %>% 
  summarise(qtd = n_distinct(document_publishing_year)) %>% 
  arrange(desc(qtd))

# top 10 universidades que mais publicaram no ano anterior
df %>% 
  filter(document_publishing_year == year(today()) - 1) %>% 
  group_by(author_institution) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd))
  
#### ggplot2 ####

# O pacote `ggplot2` é sem dúvidas é melhor ferramenta de visualização de dados do mundo.
# A sintaxe coesa e o alto nível de customização, além de outros benefícios, o tornam uma
# ferramenta poderosa para sua caixa de ferramentas. Vamos aprender por exemplos:
  
# Primeiro, como poderíamos fazer um gráfico da quantidade de artigos publicados por ano?
  
# Antes de implementar o código, pense em Pseudo-algoritmo, algo parecido com uma receita de bolo.

# O processo consta basicamente em duas etapas: agrupar os dados por ano e contar
# apenas os documentos distintos (pois um mesmo artigo pode aparecer mais de uma linha
# caso tenha múltiplos autores).

# Usando o que já aprendemos com o `dplyr` e aplicando no `ggplot2`:
df_publi_por_ano <- df %>% 
  # remover 2018 pq o ano ainda nao acabou
  filter(between(document_publishing_year, 1999, 2017)) %>% 
  group_by(ano = document_publishing_year) %>% 
  summarise(artigos = n_distinct(document_id))

df_publi_por_ano

# hora de fazer o grafico
ggplot(df_publi_por_ano, aes(x = ano, y = artigos)) + 
  geom_line()

# É possível fazer várias customizações no gráfico acima, como acrescentar legendas,
# alterar a escala do eixo horizontal e mudar a aparência usando adicionar um título.

ggplot(df_publi_por_ano, aes(x = ano, y = artigos)) + 
  geom_line() +
  # acrescentar nomes nos eixos, titulo, subtitulo e legenda no rodapé
  labs(x = NULL, y = "Artigos", title = "Quantidade de artigos indexados no SCIELO por ano",
       subtitle = "O repentino aumento a partir de 2000 provavelmente se deve a um viés temporal",
       caption = "IBPAD") + 
  # alterar a escala do eixo horizontal (x)
  scale_x_continuous(breaks = seq(1999, 2017, 2)) +
  # mudar aparencia do grafico
  theme_minimal()


# Também seria possível separar a curva acima por Universidade.
# Vamos comparar então cinco universidades: USP, UFMG, UFPE, UNB e UFSC.

unis <- c("Universidade de São Paulo", "Universidade Federal de Minas Gerais",
          "Universidade Federal de Pernambuco", "Universidade de Brasília",
          "Universidade Federal de Santa Catarina")

df_publi_por_uni_ano <- df %>% 
  filter(author_institution %in% unis & between(document_publishing_year, 1999, 2017)) %>% 
  group_by(ano = document_publishing_year, universidade = author_institution) %>% 
  summarise(qtd = n_distinct(document_id))

head(df_publi_por_uni_ano)

ggplot(df_publi_por_uni_ano, aes(x = ano, y = qtd, color = universidade)) + 
  geom_line()


df %>% 
  filter(between(document_publishing_year, 1999, 2017)) %>% 
  group_by(document_publishing_year) %>% 
  summarise(qtd = n_distinct(document_id)) %>% 
  ggplot(aes(x = document_publishing_year, y = qtd)) +
    geom_line()


# grafico de barras das universidades que mais publicaram artigos em 2016
df_top_uni <- df %>% 
  filter(document_publishing_year == 2016) %>% 
  rename(uni =  author_institution) %>% 
  group_by(uni) %>% 
  summarise(qtd = n_distinct(document_id)) %>% 
  top_n(20, qtd)


ggplot(df_top_uni, aes(x = uni, y = qtd)) +
  geom_col()


ggplot(df_top_uni, aes(x = reorder(uni, qtd), y = qtd)) +
  geom_col() + 
  coord_flip()

