#' summary_num_parametric
#'
#' Generates a summary table with the mean and standard deviation for a numeric variable.
#'
#' This function calculates the mean and standard deviation of a numeric variable from a data frame,
#' providing a summary that is useful for parametric analyses. The results are returned in a formatted table
#' with the variable name included for clarity.
#'
#' @param df A data frame containing the data to be summarized.
#' @param col_num A string representing the name of the numeric variable to be summarized.
#' @param digits An integer indicating the number of decimal places to be used in the formatted output.
#'
#' @return A data frame with the following columns:
#' \item{Variable}{The name of the numeric variable.}
#' \item{mean}{The mean of the numeric variable.}
#' \item{std}{The standard deviation of the numeric variable.}
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' # Example of using the summary_num_parametric function
#' data <- data.frame(value = c(1.2, 2.3, 3.1, 4.7, 5.0))
#' summary_num_parametric(data, 'value')
#' # Returns a data frame with the mean and standard deviation of 'value'

summary_num_parametric = function(df, col_num, digits=2){
  tabela = df %>%
    #dplyr::filter(!is.na(!!sym(col_num))) %>%
    dplyr::summarise(
      mean = round(mean(!!sym(col_num), na.rm = TRUE), digits),
      std = round(sd(!!sym(col_num), na.rm = TRUE), digits))

  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]

  return(tabela)
}
