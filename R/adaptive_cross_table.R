#' adaptive_cross_table
#'
#' Creates an adaptive summary table for a numeric or categorical variable across different columns, automatically selecting between parametric, non-parametric, and contingency table methods.
#'
#' This function generates a comprehensive summary table for a specified variable (`analyzed_column`) across multiple columns in a data frame.
#' Depending on the data type and distribution, it automatically applies the appropriate summary method: parametric, non-parametric, or contingency tables.
#'
#' @param df A data frame containing the data to be summarized.
#' @param analyzed_column A string representing the name of the primary variable to be analyzed.
#' @param sentido_percent A string indicating the direction for percentage calculations in contingency tables. Default is "col".
#' @param apenas_fisher A logical value indicating whether only the Fisher's Exact Test should be used for contingency tables. Default is FALSE.
#' @param list_columns A character vector of column names to be included in the analysis. By default, all columns except `analyzed_column` are included.
#'
#' @return A data frame that contains a comprehensive summary of the analyzed variable across the specified columns. The output includes:
#' \itemize{
#'   \item Summary statistics (mean, median, etc.) depending on the data distribution.
#'   \item P-values and tests used (T-Test, ANOVA, Mann-Whitney, Kruskal-Wallis).
#'   \item Contingency tables for categorical variables.
#' }
#'
#' @export
#' @import stats
#' @import rlang
#' @import dplyr
#' @import magrittr
#' @import janitor
#'
#' @examples
#' # Example of using the adaptive_cross_table function
#' data <- data.frame(
#' idade = rnorm(100, mean = 30, sd = 40),
#' sexo = sample(c("F", "M"), size = 100, replace = TRUE, prob = c(0.7, 0.3)),
#' grupo = sample(c("A", "B"), size = 100, replace = TRUE, prob = c(0.6, 0.4)),
#' resultado = sample(c("Positive", "Negative"), size = 100, replace = TRUE, prob = c(0.5, 0.5))
#' )
#' adaptive_cross_table(data, "grupo")
#' # Returns a summary table adapting the analysis based on the type of data in each column.

adaptive_cross_table = function(df, analyzed_column, sentido_percent='col', apenas_fisher=F, list_columns=names(df)){

  lista_coluna = list_columns[which(!(list_columns %in% c(analyzed_column)))]
  tabelona = summary_num_parametric_groups(df, lista_coluna[1], analyzed_column)[FALSE, ]

  for (coluna in lista_coluna){
    classe = class(df[[coluna]])[1]

    tryCatch({
      if (classe == "numeric"){
        local_erro = 'normality test'
        if (group_normality_test(df, coluna, analyzed_column, type_response = 1) == TRUE){
          local_erro = 'parametric test'
          tabelinha = summary_num_parametric_groups(df, coluna, analyzed_column)
        }
        else {
          local_erro = 'non parametric test'
          tabelinha = summary_num_nonparametric_groups(df, coluna, analyzed_column)
        }
      }
      else if (classe == 'character' | classe == 'factor'){
        local_erro = 'contingency test'
        tabelinha = count_table(df, analyzed_column, coluna, sentido_percent)
      }
      tabelona = rbind(tabelona, tabelinha)
    }, error = function(e) {
      print(paste('problems with:', coluna, '\ntype error:', local_erro))
    })
  }

  # Ajustes Finais
  colnames(tabelona)[colnames(tabelona) == "Overall"] = paste0("Overall 100% (n=", nrow(df[complete.cases(df[[analyzed_column]]), ]), ")")
  niveis = levels(as.factor(df[[analyzed_column]]))
  for (i in 1:length(niveis)){
    nivel = niveis[i]

    table_d = table( df[[analyzed_column]] )
    prob_table = prop.table( table_d ) %>% round(4) * 100

    colnames(tabelona)[colnames(tabelona) == nivel] = paste0(nivel, ' ', prob_table[i], "% (n=", table_d[i], ")")
  }

  return(tabelona)
}
