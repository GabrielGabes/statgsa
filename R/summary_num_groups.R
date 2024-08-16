#' summary_num_groups
#'
#' Generates a summary table for a numeric variable across groups, selecting between parametric and non-parametric methods based on normality.
#'
#' This function automatically determines whether to use parametric or non-parametric methods to summarize a numeric variable across groups
#' defined by a categorical variable. It first tests for normality within each group and then applies the appropriate summary method:
#' means and standard deviations (parametric) or medians and interquartile ranges (non-parametric).
#'
#' @param df A data frame containing the data to be summarized.
#' @param col_num A string representing the name of the numeric variable to be summarized.
#' @param col_cat A string representing the name of the categorical variable that defines the groups.
#'
#' @return A data frame summarizing the numeric variable across groups. The summary will include either:
#' \itemize{
#'   \item Mean and standard deviation for each group if the data follows a normal distribution (parametric).
#'   \item Median and interquartile range for each group if the data does not follow a normal distribution (non-parametric).
#' }
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#' @import effsize
#'
#' @examples
#' # Example of using the summary_num_groups function
#' data <- data.frame(response = rnorm(200, mean = 30, sd = 40),
#'                    desfecho = sample(c("A", "B"), size = 200, replace = TRUE, prob = c(0.6, 0.4)))
#' summary_num_groups(data, 'response', 'desfecho')
#' # Returns a summary table using either parametric or non-parametric methods based
#' # on the normality of the data.

summary_num_groups = function(df, col_num, col_cat){
  if (group_normality_test(df, col_num, col_cat, 1)){
    summary_num_parametric_groups(df, col_num, col_cat) %>% return()
  } else{
    summary_num_nonparametric_groups(df, col_num, col_cat) %>% return()
  }
}
