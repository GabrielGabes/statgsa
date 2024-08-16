#' summary_num_nonparametric
#'
#' Generates a summary table with the median and interquartile range for a numeric variable.
#'
#' This function calculates the median and interquartile range (IQR) of a numeric variable from a data frame,
#' providing a summary that is useful for non-parametric analyses. The results are returned in a formatted table
#' with the variable name included for clarity.
#'
#' @param df A data frame containing the data to be summarized.
#' @param col_num A string representing the name of the numeric variable to be summarized.
#'
#' @return A data frame with the following columns:
#' \item{Variable}{The name of the numeric variable.}
#' \item{median}{The median of the numeric variable.}
#' \item{q1_q3}{The interquartile range (IQR) of the numeric variable, displayed as Q1 - Q3.}
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' # Example of using the summary_num_nonparametric function
#' data <- data.frame(value = c(1.2, 2.3, 3.1, 4.7, 5.0, 2.1, 3.2, 4.8, 5.6, 6.7))
#' summary_num_nonparametric(data, 'value')
#' # Returns a data frame with the median and IQR of 'value'

summary_num_nonparametric = function(df, col_num){
  tabela = df %>%
    #dplyr::filter(!is.na(!!sym(col_num))) %>%
    dplyr::summarise(
      median = round(median(!!sym(col_num), na.rm = TRUE), 2),
      q1_q3 = paste0(
        '[', round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2), ']'))

  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]

  return(tabela)
}
