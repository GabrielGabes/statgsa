
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statgsa

<!-- badges: start -->
<!-- badges: end -->

The goal of statgsa is to …

## Installation

You can install the development version of statgsa from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("GabrielGabes/statgsa")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> → Will install 1 package.
#> → Will download 1 package with unknown size.
#> + statgsa   0.0.0.9000 [bld][cmp][dl] (GitHub: dfafb62)
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got statgsa 0.0.0.9000 (source) (55.42 kB)
#> ℹ Packaging statgsa 0.0.0.9000
#> ✔ Packaged statgsa 0.0.0.9000 (895ms)
#> ℹ Building statgsa 0.0.0.9000
#> ✔ Built statgsa 0.0.0.9000 (6.4s)
#> ✔ Installed statgsa 0.0.0.9000 (github::GabrielGabes/statgsa@dfafb62) (82ms)
#> ✔ 1 pkg + 113 deps: kept 94, added 1, dld 1 (NA B) [15.7s]
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(statgsa)
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

# EU

``` r
library(MASS)
birthwt = MASS::birthwt
head(birthwt)
#>    low age lwt race smoke ptl ht ui ftv  bwt
#> 85   0  19 182    2     0   0  0  1   0 2523
#> 86   0  33 155    3     0   0  0  0   3 2551
#> 87   0  20 105    1     1   0  0  0   1 2557
#> 88   0  21 108    1     1   0  0  1   2 2594
#> 89   0  18 107    1     1   0  0  1   0 2600
#> 91   0  21 124    3     0   0  0  0   0 2622
```

``` r
# install.packages("remotes")
remotes::install_github("GabrielGabes/statgsa")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'statgsa' from a github remote, the SHA1 (dfafb62d) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(statgsa)
#> Warning: substituindo importação prévia 'DescTools::Recode' por 'car::Recode'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'DescTools::RMSE' por 'caret::RMSE'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'DescTools::MAE' por 'caret::MAE'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'car::recode' por 'dplyr::recode'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'magrittr::set_names' por
#> 'rlang::set_names' quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'pROC::cov' por 'stats::cov' quando
#> carregando 'statgsa'
#> Warning: substituindo importação prévia 'dplyr::lag' por 'stats::lag' quando
#> carregando 'statgsa'
#> Warning: substituindo importação prévia 'dplyr::filter' por 'stats::filter'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'pROC::smooth' por 'stats::smooth'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'janitor::chisq.test' por
#> 'stats::chisq.test' quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'lmerTest::step' por 'stats::step'
#> quando carregando 'statgsa'
#> Warning: substituindo importação prévia 'pROC::var' por 'stats::var' quando
#> carregando 'statgsa'
#> Warning: substituindo importação prévia 'janitor::fisher.test' por
#> 'stats::fisher.test' quando carregando 'statgsa'
```

Contagem geral

``` r
freq_table(birthwt, 'low')
#>   low   n percent
#> 1   0 130  68.78%
#> 2   1  59  31.22%
```

Raça da Mãe vs Habito de Fumar:

``` r
count_table(birthwt, 'race', 'smoke')
#>   Variable     Overall          1          2          3 P-value   Test_Used
#> 1    smoke        <NA>       <NA>       <NA>       <NA> < 0.001 Chi-squared
#> 2        0 60.85 (115) 45.83 (44) 61.54 (16) 82.09 (55)    <NA>        <NA>
#> 3        1  39.15 (74) 54.17 (52) 38.46 (10) 17.91 (12)    <NA>        <NA>
```

Raça da Mãe vs Idade da mãe

``` r
summary_num_nonparametric_groups(birthwt, 'age', 'race')
#>   Variable      Overall              1                 2            3 P-value
#> 1      age 23 [19 - 26] 23.5 [20 - 29] 20.5 [17.25 - 24] 22 [19 - 25]    0.02
#>        Test_Used
#> 1 Kruskal-Wallis
summary_num_parametric_groups(birthwt, 'age', 'race')
#>   Variable     Overall            1            2            3 P-value Test_Used
#> 1      age 23.24 (5.3) 24.29 (5.65) 21.54 (5.11) 22.39 (4.54)    0.01     Anova
```

Qual abordagem de analise é a adequada? teste t tem como pressuposto a
normalidade

``` r
group_normality_test(birthwt, 'age', 'race')
#>   race     p_value
#> 1    1 0.002429356
#> 2    2 0.012248531
#> 3    3 0.278115618
```

o terceiro grupo não contem distribuição normal, portanto uma abordagem
não parametrica é a mais adequada (median \[IQR\] - Teste de hipotese:
Mann Whitney) Porém não precisa se preocupar em prestar atenção nisso,
basta rodar o código abaixo

``` r
summary_num_groups(birthwt, 'age', 'race')
#>   Variable      Overall              1                 2            3 P-value
#> 1      age 23 [19 - 26] 23.5 [20 - 29] 20.5 [17.25 - 24] 22 [19 - 25]    0.02
#>        Test_Used
#> 1 Kruskal-Wallis
```

Analise Univariada

``` r
adaptive_cross_table(birthwt, 'low')
#>    Variable Overall 100% (n=189)   0 68.78% (n=130)      1 31.22% (n=59)
#> 1       age         23 [19 - 26]       23 [19 - 28]       22 [19.5 - 25]
#> 2       lwt      121 [110 - 140]  123.5 [113 - 147]      120 [104 - 130]
#> 3      race            1 [1 - 3]          1 [1 - 3]            2 [1 - 3]
#> 4     smoke                 <NA>               <NA>                 <NA>
#> 5         0          60.85 (115)         66.15 (86)           49.15 (29)
#> 6         1           39.15 (74)         33.85 (44)           50.85 (30)
#> 7       ptl            0 [0 - 0]          0 [0 - 0]            0 [0 - 1]
#> 8        ht                 <NA>               <NA>                 <NA>
#> 9         0          93.65 (177)        96.15 (125)           88.14 (52)
#> 10        1            6.35 (12)          3.85  (5)            11.86 (7)
#> 11       ui                 <NA>               <NA>                 <NA>
#> 12        0          85.19 (161)        89.23 (116)           76.27 (45)
#> 13        1           14.81 (28)         10.77 (14)           23.73 (14)
#> 14      ftv            0 [0 - 1]          1 [0 - 1]            0 [0 - 1]
#> 15      bwt   2977 [2414 - 3487] 3267 [2948 - 3651] 2211 [1928 - 2395.5]
#>    P-value    Test_Used
#> 1     0.24 Mann-Whitney
#> 2     0.01 Mann-Whitney
#> 3    0.051 Mann-Whitney
#> 4     0.03  Chi-squared
#> 5     <NA>         <NA>
#> 6     <NA>         <NA>
#> 7  < 0.001 Mann-Whitney
#> 8    0.051 Fisher Exact
#> 9     <NA>         <NA>
#> 10    <NA>         <NA>
#> 11    0.03  Chi-squared
#> 12    <NA>         <NA>
#> 13    <NA>         <NA>
#> 14    0.23 Mann-Whitney
#> 15 < 0.001 Mann-Whitney
```

Mudando o sentido da porcentagem

``` r
adaptive_cross_table(birthwt, 'low', 'row')
#>    Variable Overall 100% (n=189)   0 68.78% (n=130)      1 31.22% (n=59)
#> 1       age         23 [19 - 26]       23 [19 - 28]       22 [19.5 - 25]
#> 2       lwt      121 [110 - 140]  123.5 [113 - 147]      120 [104 - 130]
#> 3      race            1 [1 - 3]          1 [1 - 3]            2 [1 - 3]
#> 4     smoke                 <NA>               <NA>                 <NA>
#> 5         0         100.00 (115)         74.78 (86)           25.22 (29)
#> 6         1          100.00 (74)         59.46 (44)           40.54 (30)
#> 7       ptl            0 [0 - 0]          0 [0 - 0]            0 [0 - 1]
#> 8        ht                 <NA>               <NA>                 <NA>
#> 9         0         100.00 (177)        70.62 (125)           29.38 (52)
#> 10        1          100.00 (12)         41.67  (5)            58.33 (7)
#> 11       ui                 <NA>               <NA>                 <NA>
#> 12        0         100.00 (161)        72.05 (116)           27.95 (45)
#> 13        1          100.00 (28)         50.00 (14)           50.00 (14)
#> 14      ftv            0 [0 - 1]          1 [0 - 1]            0 [0 - 1]
#> 15      bwt   2977 [2414 - 3487] 3267 [2948 - 3651] 2211 [1928 - 2395.5]
#>    P-value    Test_Used
#> 1     0.24 Mann-Whitney
#> 2     0.01 Mann-Whitney
#> 3    0.051 Mann-Whitney
#> 4     0.03  Chi-squared
#> 5     <NA>         <NA>
#> 6     <NA>         <NA>
#> 7  < 0.001 Mann-Whitney
#> 8    0.051 Fisher Exact
#> 9     <NA>         <NA>
#> 10    <NA>         <NA>
#> 11    0.03  Chi-squared
#> 12    <NA>         <NA>
#> 13    <NA>         <NA>
#> 14    0.23 Mann-Whitney
#> 15 < 0.001 Mann-Whitney
```

``` r
for (coluna in names(birthwt)){
  qtd_levels = birthwt[[coluna]] %>% as.factor() %>% levels() %>% length()
  
  if (qtd_levels <= 3){
    birthwt[[coluna]] = birthwt[[coluna]] %>% as.factor()
  }
}
```

``` r
logit_model <- glm(low ~ age + lwt + race + smoke + ht + ui, data = birthwt, family = binomial)
# summary(logit_model)
binary_model_eval(logit_model) %>% round(2)
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#>             Accuracy       Pos Pred Value          Sensitivity 
#>                 0.75                 0.66                 0.39 
#>          Specificity          F1_Score.F1                  AUC 
#>                 0.91                 0.49                 0.73 
#>   Pseudo_R2.McFadden Pseudo_R2.Nagelkerke                  AIC 
#>                 0.13                 0.21               219.95 
#>                  BIC                  VIF               Status 
#>               245.88                 0.00                 1.00
univariate_model_analysis(logit_model)
#> Waiting for profiling to be done...
#>          OR 2.5 % 97.5 % Pr(>|z|)
#> age    0.98  0.91   1.05     0.60
#> lwt    0.98  0.97   1.00     0.01
#> race2  3.60  1.29  10.32     0.01
#> race3  2.46  1.07   5.91     0.03
#> smoke1 2.79  1.31   6.19    0.009
#> ht1    6.41  1.72  27.01    0.007
#> ui1    2.45  1.01   5.94     0.04
```

``` r
model1 <- lm(bwt ~ lwt, data = birthwt)
# summary(model1)
lm_model_eval(model1) %>% round(2)
#>       MAE       MSE      RMSE      MAPE       AIC       BIC        R2    R2_adj 
#>    571.54 510693.21    714.63     23.49   3026.48   3036.21      0.03      0.03 
#>       VIF    Status 
#>      0.00      1.00

model2 <- lm(bwt ~ lwt + age + race + smoke + ht + ui, data = birthwt)
# summary(model2)
lm_model_eval(model2) %>% round(2)
#>       MAE       MSE      RMSE      MAPE       AIC       BIC        R2    R2_adj 
#>    513.59 401230.61    633.43     20.52   2992.89   3022.07      0.24      0.21 
#>       VIF    Status 
#>      0.00      1.00
```
