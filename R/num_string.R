#' num_string
#'
#' Formats numeric values for clear and consistent presentation.
#'
#' This function formats numeric values with varying criteria depending on the magnitude of the value.
#' It ensures that small and large values are presented in a way that is easy to interpret, making it particularly useful
#' for presenting results in scientific reports, tables, or data summaries where precision and clarity are important.
#'
#' @param valor A numeric value to be formatted.
#' @param digits An integer indicating the number of decimal places to be used in the formatted output.
#'
#' @return A string with the formatted numeric value.
#' Values smaller than 0.01 are formatted with three decimal places, values equal to 0 are displayed as "0.00",
#' very small values (<= 0.0009) and very large values (> 10,000) are formatted in scientific notation,
#' and other values are formatted according to the specified number of decimal places.
#'
#' @export
#' @import stats
#'
#' @examples
#' # Examples of using the num_string function
#' num_string(30, 2)  # Returns "30.00"
#' num_string(0, 2)  # Returns "0.00"
#' num_string(0.001, 2)  # Returns "0.001"
#' num_string(0.0001, 2)  # Returns "1.00e-04"
#' num_string(0.041212, 2)  # Returns "0.04"
#' num_string(5115156, 2)  # Returns "5.12e+06"

num_string = function(valor, digits=2) {
  if (is.na(valor)){return(NA)}

  if (abs(valor) < 0.01 && abs(valor) > 0.0009) {
    return(formatC(valor, format = "f", digits = 3))
  } else if (valor == 0) {
    return("0.00")
  } else if (abs(valor) <= 0.0009 && valor != 0 || abs(valor) > 10000) {
    return(formatC(valor, format = "e", digits = digits))
  } else {
    return(formatC(valor, format = "f", digits = digits))
  }
}
