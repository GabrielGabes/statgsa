#' count_table
#'
#' Creates a contingency table with counts, percentages, and hypothesis testing.
#'
#' This function generates a contingency table that includes counts, row and column totals, and percentages.
#' Additionally, it applies appropriate hypothesis tests (Fisher's exact test or Chi-squared test) to evaluate
#' the association between two categorical variables. The table is formatted for clarity, making it ideal for
#' presentation in reports or publications.
#'
#' @param df A data frame containing the data.
#' @param var_y A string representing the name of the dependent (response) categorical variable.
#' @param var_x A string representing the name of the independent (explanatory) categorical variable.
#' @param sentido_percent A string indicating the direction in which percentages should be calculated.
#'        Options are 'col' for column percentages or 'row' for row percentages. Default is 'col'.
#' @param apenas_fisher A logical value indicating whether to use only Fisher's exact test when the contingency table
#'        has 2x2 dimensions. If `TRUE`, Fisher's test will always be used for 2x2 tables. Default is `FALSE`.
#'
#' @return A data frame containing the formatted contingency table with counts, percentages, and the results of the hypothesis test.
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#' @import janitor
#'
#' @examples
#' # Example of using the count_table function
#' data <- data.frame(treatment = c("A", "B", "A", "C", "B", "A"),
#'                    outcome = c("Yes", "No", "Yes", "Yes", "No", "No"))
#' count_table(data, "outcome", "treatment")
#' # Returns a contingency table with counts, percentages, and a p-value from the appropriate test

count_table = function(df, var_y, var_x, sentido_percent='col', apenas_fisher=F){
  #sentido_porcent => #col, row
  tabela = df %>%
    janitor::tabyl(!!sym(var_x), !!sym(var_y), show_na = FALSE) %>%
    adorn_totals(c("row", "col")) %>%
    adorn_percentages(sentido_percent) %>%
    adorn_pct_formatting(2) %>%
    adorn_ns

  #tabela = as.data.frame(tabela) %>%
  #mutate(across(where(is.character), ~ str_replace_all(.x, "(\\d+\\.\\d+)\\% *\\(? *(\\d+)\\)?", "\\2 (\\1%)")))
  tabela <- rename(tabela, "Variable" = all_of(var_x))
  tabela = rbind(NA, tabela)
  tabela[["Variable"]] = as.character(tabela[["Variable"]])
  tabela[["Variable"]][1] = var_x
  tabela = tabela[-nrow(tabela), ] #excluindo ultima linha

  #Aplicando teste de hipotese adequado
  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if(is_fisher_applicable(df, var_y, var_x) == F){
    if (nrow(tabela) <= 3 && apenas_fisher == T){
      tabela[["P-value"]][1] = pval_string(stats::fisher.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Fisher Exact"}
    else {
      tabela[["P-value"]][1] = pval_string(stats::chisq.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Chi-squared"}}
  else{
    tabela[["P-value"]][1] = pval_string(stats::fisher.test(df[[var_x]],df[[var_y]])$p.value)
    tabela[["Test_Used"]][1] = "Fisher Exact"}

  # Reordenando as colunas, colocando coluna total para primeira posicao
  # Determina a sequência de índices das colunas
  indices = seq_len(ncol(tabela))
  # Altera a sequência para colocar a coluna -3 na posição 2
  indices = c(indices[1], indices[length(indices)-2], indices[2:(length(indices)-3)], indices[(length(indices)-1):length(indices)])
  # Reordena as colunas do dataframe de acordo com a sequência
  tabela = tabela[, indices]
  tabela[["Variable"]][1] = var_x

  # Ultimos Ajustes
  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'
  tabela[] = lapply(tabela, function(x) gsub("%", "", x))
  tabela[] = lapply(tabela, function(x) gsub("  ", " ", x))

  return(tabela %>% as.data.frame())
}
