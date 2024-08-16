#' freq_table
#'
#' Creates a simple frequency table with percentage formatting.
#'
#' This function generates a frequency table for a specified variable in a data frame. The output includes
#' both counts and percentages, formatted for easy interpretation. It is particularly useful for summarizing categorical data
#' and presenting the results in reports or tables.
#'
#' @param df A data frame containing the data.
#' @param variavel A string representing the name of the variable for which the frequency table is to be created.
#'
#' @return A data frame containing the frequency counts and percentages for the specified variable.
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#' @import janitor
#'
#' @examples
#' # Examples of using the freq_table function
#' data <- data.frame(category = c("A", "B", "A", "C", "B", "A"))
#' freq_table(data, "category")
#' # Returns a data frame with the counts and percentages of each category

freq_table = function(df, variavel){
  df %>% janitor::tabyl(!!sym(variavel), show_na = FALSE) %>%
    adorn_pct_formatting(2) %>% as.data.frame() %>% return()
}
