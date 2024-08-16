#' summary_num_parametric_groups
#'
#' Generates a summary table of mean and standard deviation for a numeric variable across groups, with optional effect size calculation.
#'
#' This function calculates the mean and standard deviation of a numeric variable within groups defined by a categorical variable.
#' It can also perform statistical tests (T-Test for two groups or ANOVA for more than two groups) and optionally calculate Cohen's d
#' effect size for pairwise comparisons.
#'
#' @param df A data frame containing the data to be summarized.
#' @param col_num A string representing the name of the numeric variable to be summarized.
#' @param col_cat A string representing the name of the categorical variable that defines the groups.
#' @param teste_extra A character indicating whether to calculate Cohen's d effect size. Set to "T" to perform this calculation;
#'        otherwise, no effect size is calculated. Default is "F".
#'
#' @return A data frame with the following columns:
#' \item{Variable}{The name of the numeric variable.}
#' \item{mean (std)}{The mean and standard deviation of the numeric variable for each group and overall.}
#' \item{P-value}{The p-value from the statistical test (T-Test or ANOVA).}
#' \item{Test_Used}{The type of statistical test applied ("T Test" or "Anova").}
#' \item{teste_extra}{The Cohen's d effect size and confidence interval, if calculated.}
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#' @import effsize
#'
#' @examples
#' # Example of using the summary_num_parametric_groups function
#' data <- data.frame(value = c(1.2, 2.3, 3.1, 4.7, 5.0, 2.1, 3.2, 4.8, 5.6, 6.7),
#'                    group = c("A", "B", "A", "B", "A", "A", "B", "B", "A", "B"))
#' summary_num_parametric_groups(data, 'value', 'group', teste_extra = "T")
#' # Returns a summary table with mean, standard deviation, p-value, test used, and Cohen's d effect.

summary_num_parametric_groups <- function(df, col_num, col_cat, teste_extra = "F") {

  # Sumário por grupo
  sumario_grupo = df %>%
    # filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2),
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
    )
  sumario_grupo <- rename(sumario_grupo, coluna = all_of(col_cat))

  # Sumário geral (total)
  sumario_geral = df %>%
    # filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2),
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
    )

  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num

  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA

  if ( length(levels(as.factor(df[[col_cat]]))) <= 2 ){
    formula = formula(paste0(col_num, '~', col_cat))

    # Verificando homogenidade
    teste_homogeneidade = bartlett.test(formula, df)
    # h0 = as varianças são homogeneas

    if (teste_homogeneidade$p.value > 0.05){
      tabela[["Test_Used"]][1] = "Student's t-test"
      pvalor = pval_string(t.test(df[[col_num]]~df[[col_cat]], var.equal=T)$p.value)
    } else {
      tabela[["Test_Used"]][1] = "Welch's t-test"
      pvalor = pval_string(t.test(df[[col_num]]~df[[col_cat]], var.equal=F)$p.value)
    }

    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      d_cohen = effsize::cohen.d(grupo1, grupo2)
      estimador = as.character(num_string(d_cohen$estimate,2))
      IC_00 = as.character(num_string(d_cohen$conf.int[1],2))
      IC_01 = as.character(num_string(d_cohen$conf.int[2],2))
      d_cohen = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = d_cohen}
  }else {
    pvalor = summary(aov(df[[col_num]]~df[[col_cat]]))[[1]][["Pr(>F)"]][1]
    pvalor = pval_string(pvalor)
    tabela[["Test_Used"]][1] = "Anova"
  }

  tabela[["P-value"]] = pvalor

  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira

  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'

  return(tabela)
}
