#' is_fisher_applicable
#'
#' Determines if the Fisher's exact test is appropriate for testing the hypothesis between two categorical variables.
#'
#' This function checks whether the Fisher's exact test is suitable for testing the association between two categorical variables.
#' The Fisher's test is generally more appropriate when the sample sizes are small and when the expected frequencies in any cell
#' of the contingency table are less than 5. This function returns `TRUE` if Fisher's test is recommended based on these criteria,
#' and `FALSE` otherwise.
#'
#' @param df A data frame containing the categorical variables to be tested.
#' @param var1 A string representing the name of the first categorical variable.
#' @param var2 A string representing the name of the second categorical variable.
#'
#' @return A logical value. Returns `TRUE` if Fisher's exact test is applicable, and `FALSE` otherwise.
#'
#' @export
#' @import stats
#'
#' @examples
#' # Example of using the is_fisher_applicable function
#' data <- data.frame(group = c("A", "B", "A", "C", "B", "A"),
#'                    outcome = c("Yes", "No", "Yes", "Yes", "No", "No"))
#' is_fisher_applicable(data, "group", "outcome")
#' # Returns TRUE or FALSE based on whether Fisher's exact test is appropriate

is_fisher_applicable = function(df, var1, var2){
  length1 = length(levels(as.factor(df[[var1]])))
  length2 = length(levels(as.factor(df[[var2]])))
  if (length1 >= 3 || length2 >= 3){
    return(FALSE)
  }
  else {
    # Criar tabela de contignecia
    tabela = table(df[[var1]], df[[var2]])
    # Calcular as expectativas de frequência
    total_geral = sum(tabela)
    expectativas = outer(rowSums(tabela), colSums(tabela), FUN = "*") / total_geral
    # Verificar se alguma célula tem expectativa de frequência < 5
    return(any(expectativas < 5))
  }
}
