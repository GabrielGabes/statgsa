
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statgsa

<!-- badges: start -->
<!-- badges: end -->

# statgsa: Automating Epidemiological Data Analysis

*“Anything that can be automated, should be automated.”*  
— Inspired by the philosophy of Hadley Wickham’s book “R Packages”

Building on this principle, I have developed an entire pipeline for
epidemiological data analysis, drawing from years of experience in data
science, specifically in biostatistics. The **statgsa** package is
designed to streamline and enhance the efficiency of statistical
analysis in research, ensuring that critical tasks are automated,
consistent, and error-free.

## Why statgsa?

This package offers a comprehensive suite of functions, each
meticulously crafted to address a wide array of tasks in statistical
analysis. Below are some of the key features:

- **Output Formatting**: Functions designed to ensure that the results
  of statistical analyses are presented clearly and professionally. This
  is crucial for organizing research findings in a way that meets the
  rigorous standards required by scientific journals.
- **Cross Tables with Hypothesis Testing**: Automatically analyzes the
  data and selects the most appropriate statistical tests, generating
  detailed cross-tabulations.
- **Numerical Summaries by Group with Hypothesis Testing**: Similar to
  the cross tables, this feature automatically determines and applies
  the correct hypothesis tests, providing insightful group summaries.
- **Comprehensive Crosstables**: Generate complete and detailed
  crosstables that are ready for reporting.
- **Evaluation Metrics for Supervised ML Models**: Includes measures for
  evaluating the performance of machine learning models in a statistical
  context.
- **Complex Statistical Analyses**: Streamline and automate intricate
  statistical procedures, allowing you to focus on interpreting the
  results.

### The Power of Adaptive Cross Tables

One of the standout features of statgsa is its ability to generate
**adaptive cross tables**. These tables automatically adjust based on
the types of variables involved, applying the correct hypothesis tests
and criteria. This flexibility significantly accelerates the process of
generating accurate and publication-ready tables for reports.

## Installation

You can install the development version of statgsa from
[GitHub](https://github.com/GabrielGabes/statgsa) with:

``` r
# install.packages("pak")
pak::pak("GabrielGabes/statgsa")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> ℹ No downloads are needed
#> ✔ 1 pkg + 113 deps: kept 95 [6.7s]
```

or

``` r
# install.packages("remotes")
# remotes::install_github("GabrielGabes/statgsa")
# library(statgsa)
```

## Loading Package

``` r
library(statgsa)
```

Also load dependent packages

``` r
# Package loading facilitator
if(!require(pacman)) install.packages("pacman")
#> Carregando pacotes exigidos: pacman
library(pacman)

# dependents Packages
pacman::p_load(
stats,
rlang,
dplyr,
janitor,
effsize,
caret,
DescTools,
car,
pROC,
MuMIn,
magrittr,
tidyr
)
```

## Example

This is a basic example that demonstrates how to solve a common problem
in the analysis of epidemiological study data:

The dataset from the `MASS` package contains data on 189 births at the
Baystate Medical Centre, Springfield, Massachusetts during 1986
(Venables & Ripley, 2002).

You can find the data dictionary here:  
[Data Dictionary for
birthwt](https://www.rdocumentation.org/packages/MASS/versions/7.3-61/topics/birthwt)

### Data Preparation

Let’s load the dataset and inspect the first few rows:

``` r
library(MASS)
#> 
#> Anexando pacote: 'MASS'
#> O seguinte objeto é mascarado por 'package:dplyr':
#> 
#>     select
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

Next, we’ll adjust the variable types:

``` r
for (coluna in names(birthwt)){
  qtd_levels = birthwt[[coluna]] %>% as.factor() %>% levels() %>% length()
  
  if (qtd_levels <= 3){
    birthwt[[coluna]] = birthwt[[coluna]] %>% as.factor()
  }
}
```

## Analysis

The main variable of interest is low birth weight, a binary response
variable (D. W. Hosmer & Lemeshow, 1989).

### General Count

``` r
freq_table(birthwt, 'low')
#>   low   n percent
#> 1   0 130  68.78%
#> 2   1  59  31.22%
```

### Mother’s Race vs. Smoking Habit

``` r
count_table(birthwt, 'race', 'smoke')
#>   Variable     Overall          1          2          3 P-value   Test_Used
#> 1    smoke        <NA>       <NA>       <NA>       <NA> < 0.001 Chi-squared
#> 2        0 60.85 (115) 45.83 (44) 61.54 (16) 82.09 (55)    <NA>        <NA>
#> 3        1  39.15 (74) 54.17 (52) 38.46 (10) 17.91 (12)    <NA>        <NA>
```

### Mother’s Race vs. Mother’s Age

We can summarize the age by race using both non-parametric and
parametric approaches:

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

### Determining the Appropriate Analytical Approach

Since the t-test assumes normality, we first check the normality of the
data:

``` r
group_normality_test(birthwt, 'age', 'race')
#>   race     p_value
#> 1    1 0.002429356
#> 2    2 0.012248531
#> 3    3 0.278115618
```

The third group does not follow a normal distribution, so a
non-parametric approach (median \[IQR\] - Hypothesis Test: Mann-Whitney)
is more appropriate. However, you don’t need to worry about deciding
this manually—just run the code below:

``` r
summary_num_groups(birthwt, 'age', 'race')
#>   Variable      Overall              1                 2            3 P-value
#> 1      age 23 [19 - 26] 23.5 [20 - 29] 20.5 [17.25 - 24] 22 [19 - 25]    0.02
#>        Test_Used
#> 1 Kruskal-Wallis
```

### Cross Tables - Univariate Analysis

``` r
adaptive_cross_table(birthwt, 'low')
#>    Variable Overall 100% (n=189)   0 68.78% (n=130)      1 31.22% (n=59)
#> 1       age         23 [19 - 26]       23 [19 - 28]       22 [19.5 - 25]
#> 2       lwt      121 [110 - 140]  123.5 [113 - 147]      120 [104 - 130]
#> 3      race                 <NA>               <NA>                 <NA>
#> 4         1           50.79 (96)         56.15 (73)           38.98 (23)
#> 5         2           13.76 (26)         11.54 (15)           18.64 (11)
#> 6         3           35.45 (67)         32.31 (42)           42.37 (25)
#> 7     smoke                 <NA>               <NA>                 <NA>
#> 8         0          60.85 (115)         66.15 (86)           49.15 (29)
#> 9         1           39.15 (74)         33.85 (44)           50.85 (30)
#> 10      ptl            0 [0 - 0]          0 [0 - 0]            0 [0 - 1]
#> 11       ht                 <NA>               <NA>                 <NA>
#> 12        0          93.65 (177)        96.15 (125)           88.14 (52)
#> 13        1            6.35 (12)          3.85  (5)            11.86 (7)
#> 14       ui                 <NA>               <NA>                 <NA>
#> 15        0          85.19 (161)        89.23 (116)           76.27 (45)
#> 16        1           14.81 (28)         10.77 (14)           23.73 (14)
#> 17      ftv            0 [0 - 1]          1 [0 - 1]            0 [0 - 1]
#> 18      bwt   2977 [2414 - 3487] 3267 [2948 - 3651] 2211 [1928 - 2395.5]
#>    P-value    Test_Used
#> 1     0.24 Mann-Whitney
#> 2     0.01 Mann-Whitney
#> 3     0.08  Chi-squared
#> 4     <NA>         <NA>
#> 5     <NA>         <NA>
#> 6     <NA>         <NA>
#> 7     0.03  Chi-squared
#> 8     <NA>         <NA>
#> 9     <NA>         <NA>
#> 10 < 0.001 Mann-Whitney
#> 11   0.051 Fisher Exact
#> 12    <NA>         <NA>
#> 13    <NA>         <NA>
#> 14    0.03  Chi-squared
#> 15    <NA>         <NA>
#> 16    <NA>         <NA>
#> 17    0.23 Mann-Whitney
#> 18 < 0.001 Mann-Whitney
```

### Cross Tables - Changing the Percentage Orientation

``` r
adaptive_cross_table(birthwt, 'low', 'row')
#>    Variable Overall 100% (n=189)   0 68.78% (n=130)      1 31.22% (n=59)
#> 1       age         23 [19 - 26]       23 [19 - 28]       22 [19.5 - 25]
#> 2       lwt      121 [110 - 140]  123.5 [113 - 147]      120 [104 - 130]
#> 3      race                 <NA>               <NA>                 <NA>
#> 4         1          100.00 (96)         76.04 (73)           23.96 (23)
#> 5         2          100.00 (26)         57.69 (15)           42.31 (11)
#> 6         3          100.00 (67)         62.69 (42)           37.31 (25)
#> 7     smoke                 <NA>               <NA>                 <NA>
#> 8         0         100.00 (115)         74.78 (86)           25.22 (29)
#> 9         1          100.00 (74)         59.46 (44)           40.54 (30)
#> 10      ptl            0 [0 - 0]          0 [0 - 0]            0 [0 - 1]
#> 11       ht                 <NA>               <NA>                 <NA>
#> 12        0         100.00 (177)        70.62 (125)           29.38 (52)
#> 13        1          100.00 (12)         41.67  (5)            58.33 (7)
#> 14       ui                 <NA>               <NA>                 <NA>
#> 15        0         100.00 (161)        72.05 (116)           27.95 (45)
#> 16        1          100.00 (28)         50.00 (14)           50.00 (14)
#> 17      ftv            0 [0 - 1]          1 [0 - 1]            0 [0 - 1]
#> 18      bwt   2977 [2414 - 3487] 3267 [2948 - 3651] 2211 [1928 - 2395.5]
#>    P-value    Test_Used
#> 1     0.24 Mann-Whitney
#> 2     0.01 Mann-Whitney
#> 3     0.08  Chi-squared
#> 4     <NA>         <NA>
#> 5     <NA>         <NA>
#> 6     <NA>         <NA>
#> 7     0.03  Chi-squared
#> 8     <NA>         <NA>
#> 9     <NA>         <NA>
#> 10 < 0.001 Mann-Whitney
#> 11   0.051 Fisher Exact
#> 12    <NA>         <NA>
#> 13    <NA>         <NA>
#> 14    0.03  Chi-squared
#> 15    <NA>         <NA>
#> 16    <NA>         <NA>
#> 17    0.23 Mann-Whitney
#> 18 < 0.001 Mann-Whitney
```

### Evaluation Metrics for Supervised ML Models

Here’s how to evaluate a logistic regression model:

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

And here’s how to evaluate linear regression models:

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
