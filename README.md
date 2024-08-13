
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSAStat

<!-- badges: start -->
<!-- badges: end -->

The goal of GSAStat is to …

## Installation

You can install the development version of GSAStat from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("GabrielGabes/GSAStat")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(GSAStat)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

- Funções

  - Funções projetadas para formatar e ajustar a apresentação de valores
    numéricos

    - pval_string -\> Esta função formata valores de p, permitindo uma
      apresentação mais clara e consistente dos resultados estatísticos.
      Isso é particularmente útil para apresentar resultados em artigos
      científicos ou relatórios, onde a precisão e a clareza na
      comunicação dos valores de p são essenciais.

    - num_string -\> Esta função é usada para arredondar valores
      numéricos com diferentes critérios, dependendo da magnitude do
      valor. Ela parece ser útil em contextos onde valores pequenos e
      grandes precisam ser apresentados de forma que sejam facilmente
      compreensíveis.

  - Funções para a criação e personalização de tabelas cruzadas

    - Dados Categorico:
      - count_table
      - conti
    - Dados Númerico:
      - shapiro_test
      - normalidade_por_grupo_shapiro
      - normalidade_por_grupo_ks
      - normalidade_por_grupo_criterio
      - summary_numerico_parametrico
      - summary_numerico_n_parametrico
      - summary_numerico_por_grupo_parametrico
      - summary_numerico_por_grupo_n_parametrico
      - summary_numerico_por_grupo

  - Funções relacionadas à análise de modelos univariados e
    multivariados

    - analise_mod
    - metricas_de_avaliacao_glm
    - metricas_de_avaliacao_regressao

  - Funções para criação de Cross Tables

    - cross_table
    - cross_table_glm

``` r
pacman::p_load(
dplyr, # manipulação de dados
magrittr, # operador pipe line %>%
janitor, # tabela de contigencia => tabyl, adorn_pct_formatting, adorn_totals, adorn_percentages, adorn_ns
effsize # tamanho do efeito d'cohen
)
```
