#' group_normality_test
#'
#' Determines and applies the appropriate normality test for each group in a dataset.
#'
#' This function assesses the normality of a numeric variable within groups defined by a categorical variable.
#' It automatically selects the appropriate normality test based on the number of observations in each group.
#' If any group has more than 5000 observations, the Kolmogorov-Smirnov test is applied; otherwise, the Shapiro-Wilk test is used.
#'
#' @param df A data frame containing the data to be tested.
#' @param col_num A string representing the name of the numeric variable to be tested for normality.
#' @param col_cat A string representing the name of the categorical variable that defines the groups.
#' @param type_response An integer that specifies the type of output.
#'        If `0`, the function returns a data frame with the p-values for each group.
#'        If `1`, the function returns `TRUE` if all groups follow a normal distribution, and `FALSE` otherwise. Default is `0`.
#'
#' @return Depending on the value of `type_response`:
#' \itemize{
#'   \item If `type_response = 0`: A data frame with two columns, `group` and `p_value`, showing the p-values for each group.
#'   \item If `type_response = 1`: A logical value. Returns `TRUE` if all groups follow a normal distribution, and `FALSE` otherwise.
#' }
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' # Example of using the group_normality_test function
#' data <- data.frame(response = rnorm(200, mean = 30, sd = 40),
#'                    group = sample(c("A", "B"), size = 200, replace = TRUE, prob = c(0.6, 0.4)))
#' group_normality_test(data, 'response', 'group', type_response = 0)
#' # Returns a data frame with p-values for each group
#'
#' # Example of using the function with logical output
#' group_normality_test(data, 'response', 'group', type_response = 1)
#' # Returns TRUE if all groups follow a normal distribution, FALSE otherwise

group_normality_test <- function(df, col_num, col_cat, type_response = 0) {

  # Filter data and count the number of observations in each group
  tabela <- df %>%
    #filter(!is.na(df[[col_num]]) & !is.na(df[[col_cat]])) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(n = n())

  # Check if any group has more than 5000 observations
  # Apply Kolmogorov-Smirnov test if any group has more than 5000 observations, otherwise apply Shapiro-Wilk test
  if (any(tabela$n > 5000) == T){
    results = group_ks_test(df, col_num, col_cat, type_response)
    return(results)
  } else {
    results = group_shapiro_test(df, col_num, col_cat, type_response)
    return(results)
  }
}
