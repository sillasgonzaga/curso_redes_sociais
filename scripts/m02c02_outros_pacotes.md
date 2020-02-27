O pacote **igraph**, conforme mencionado nos vídeos, é o principal
pacote para todas as tarefas relacionadas a análise de redes. Contudo,
dentre os mais de 12 mil pacotes disponíveis no CRAN, o repositório
oficial de pacotes R, existem outros que podem ser úteis em seus
projetos de análise de redes sociais (ARS).

statnet
=======

Corresponde a uma coleção de pacotes para ARS (tal como o `tidyverse`).
Seus pacotes fornecem funções para realizar tarefas avançadas como
análise e modelagem de redes dinâmicas, simulação de redes, modelagem da
distribuição de graus (degrees) de redes e análise de redes temporais.

Referência: [statnet](http://statnet.org/)

network
=======

Funcões para criar e modificar objetos de redes. Possui funções básicas
de ARS (muito semelhantes às presentes no `igraph`) e alguns datasets
interessantes, como redes de [busca e
salvamento](https://www.rdocumentation.org/packages/network/versions/1.13.0/topics/emon)
e de [casamentos em famílias de
Florença](https://www.rdocumentation.org/packages/network/versions/1.13.0/topics/flo),
na Itália.

Referência:
[Vignette](https://cran.r-project.org/web/packages/network/vignettes/networkVignette.pdf)
do pacote.

sna
===

Link: Contem uma série de ferramentas para ARS, incluindo distância
estrutural, métodos de covariância, detecção de equivalência estrutura,
regressão de redes, modelos bayesianos de redes, eficiência de
Krackhardt de uma rede, geração de redes aleatórias e visualização.
Funciona em harmonia com o pacote `network`. Possui cerca de 226
funções.

Referência: Butts, Carter T. (2016). “sna: Tools for Social Network
Analysis.” R package version 2.4.

ggraph
======

É uma extensão do pacote `ggplot2` para visualização de redes, isto é,
foi idealizado para criar visualizações de redes com a mesma sintaxe do
`ggplot2`, que pode ser mais concisa do que a sintaxe do `igraph`.

Referência:
[`ggraph`](https://cran.r-project.org/web/packages/ggraph/index.html)

[keyplayer](https://cran.r-project.org/web/packages/keyplayer/index.html)
=========================================================================

Possui funções para calcular alguns métricas bem interessantes de
centralidade de uma rede que não estão presentes no pacote `igraph`,
como *diffusion*, que mede a habilidade de um dado indivíduo de
disseminar informação por todos os possíveis caminhos da rede, e score
de fragmentação, que mede tal atributo de uma rede após um vértice ser
removido. Também merece ser destacada a função `kpset`, que seleciona um
grupo de k vértices mais central de uma rede de acordo com uma dade
métrica de centralidade.

TODO
====

graficos interativos
====================

visNetwork
==========
