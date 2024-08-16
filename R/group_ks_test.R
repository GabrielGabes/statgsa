#' group_ks_test
#'
#' Performs the Kolmogorov-Smirnov test for normality by group.
#'
#' This function applies the Kolmogorov-Smirnov test to assess the normality of a numeric variable within groups
#' defined by a categorical variable. The function provides an option to return either the p-values for each group
#' or a logical value indicating whether all groups follow a normal distribution.
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
#'
#' @examples
#' # Example of using the group_ks_test function with p-value output
#' data <- data.frame(response = rnorm(200, mean = 30, sd = 40),
#'                    desfecho = sample(c("A", "B"), size = 200, replace = TRUE, prob = c(0.6, 0.4)))
#' group_ks_test(data, 'response', 'desfecho', type_response = 0)
#' # Returns a data frame with p-values for each group
#'
#' # Example of using the function with logical output
#' group_ks_test(data, 'response', 'desfecho', type_response = 1)
#' # Returns TRUE if all groups follow a normal distribution, FALSE otherwise

group_ks_test <- function(df, col_num, col_cat, type_response = 0) {

  # Perform the Kolmogorov-Smirnov test for each group
  resultados <- by(df[[col_num]], df[[col_cat]], function(subset) {
    ks.test(subset, "pnorm", mean(subset), sd(subset))
  })

  # Extract p-values
  p_values <- sapply(resultados, function(test) test$p.value)

  # Check if any p-values indicate non-normality
  verificacao <- any(p_values < 0.05)

  tabela = data.frame(group = names(p_values), p_value = p_values)
  rownames(tabela) = NULL

  colnames(tabela) <- c(col_cat, "p_value")  # Rename columns for clarity

  # Return the results based on the type_response parameter
  if (type_response == 0) {
    return(tabela)
  } else if (type_response == 1) {
    # At least one distribution does not follow normality
    # All distributions follow normality
    return(!any(resultados$p_value < 0.05))
  }
}

