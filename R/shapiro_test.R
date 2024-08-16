#' shapiro_test
#'
#' Performs the Shapiro-Wilk test for normality on a numeric vector.
#'
#' This function applies the Shapiro-Wilk test to a numeric vector to assess whether the data follows a normal distribution.
#' It returns the p-value from the test, which can be used to determine if the null hypothesis of normality should be rejected.
#'
#' @param x A numeric vector containing the data to be tested for normality.
#'
#' @return A numeric value representing the p-value from the Shapiro-Wilk test.
#' A small p-value (typically < 0.05) indicates that the data significantly deviates from a normal distribution.
#'
#' @export
#' @import stats
#'
#' @examples
#' # Example of using the shapiro_test function
#' data <- c(1.2, 2.3, 3.1, 4.7, 5.0)
#' shapiro_test(data)
#' # Returns the p-value from the Shapiro-Wilk test

shapiro_test <- function(x) {
  return(shapiro.test(x)$p.value)
}
