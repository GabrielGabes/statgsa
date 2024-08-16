#' summary_num_nonparametric_groups
#'
#' Generates a summary table of median and interquartile range for a numeric variable across groups, with optional Hodges-Lehmann estimator.
#'
#' This function calculates the median and interquartile range (IQR) of a numeric variable within groups defined by a categorical variable.
#' It also performs non-parametric tests (Mann-Whitney for two groups or Kruskal-Wallis for more than two groups) and optionally calculates the Hodges-Lehmann estimator for pairwise comparisons.
#'
#' @param df A data frame containing the data to be summarized.
#' @param col_num A string representing the name of the numeric variable to be summarized.
#' @param col_cat A string representing the name of the categorical variable that defines the groups.
#' @param teste_extra A character indicating whether to calculate the Hodges-Lehmann estimator for effect size. Set to "T" to perform this calculation; otherwise, no effect size is calculated. Default is "F".
#'
#' @return A data frame with the following columns:
#' \item{Variable}{The name of the numeric variable.}
#' \item{median [IQR]}{The median and interquartile range (IQR) of the numeric variable for each group and overall.}
#' \item{P-value}{The p-value from the statistical test (Mann-Whitney or Kruskal-Wallis).}
#' \item{Test_Used}{The type of statistical test applied ("Mann-Whitney" or "Kruskal-Wallis").}
#' \item{teste_extra}{The Hodges-Lehmann estimator and confidence interval, if calculated.}
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' # Example of using the summary_num_nonparametric_groups function
#' data <- data.frame(value = c(1.2, 2.3, 3.1, 4.7, 5.0, 2.1, 3.2, 4.8, 5.6, 6.7),
#'                    group = c("A", "B", "A", "B", "A", "A", "B", "B", "A", "B"))
#' summary_num_nonparametric_groups(data, 'value', 'group', teste_extra = "T")
#' # Returns a summary table with median, IQR, p-value, test used, and Hodges-Lehmann estimator.

summary_num_nonparametric_groups = function(df, col_num, col_cat, teste_extra="F"){
  # Sumário por grupo
  sumario_grupo = df %>%
    #dplyr::filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2),
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )
  sumario_grupo <- rename(sumario_grupo, coluna = all_of(col_cat))

  # Sumário geral (total)
  sumario_geral = df %>%
    #filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2),
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )

  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num

  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if (length(levels(as.factor(df[[col_cat]]))) > 2){
    pvalor = pval_string(kruskal.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Kruskal-Wallis"}
  else{
    pvalor = pval_string(wilcox.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Mann-Whitney"

    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      teste_hip = wilcox.test(grupo1, grupo2, conf.int = TRUE)

      #Estimador Hodges Lehmann
      estimador = as.character(num_string(teste_hip$estimate,2))
      IC_00 = as.character(num_string(teste_hip$conf.int[1],2))
      IC_01 = as.character(num_string(teste_hip$conf.int[2],2))
      hodges_lehmann = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = hodges_lehmann
    }
  }
  tabela[["P-value"]] = pvalor

  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira

  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'

  return(tabela)
}
