#' pval_string
#'
#' Formats p-values for clear and consistent presentation.
#'
#' This function formats p-values to ensure clear and consistent presentation in statistical results.
#' It is particularly useful for presenting results in scientific papers or reports where accuracy
#' and clarity in communicating p-values are essential. Extremely small p-values are displayed as "< 0.001"
#' to highlight their significance, while values close to 1 are formatted to retain two decimal places.
#'
#' @param valor A numeric value representing the p-value to be formatted.
#'
#' @return A string with the formatted p-value.
#' If the p-value is less than 0.001, it returns "< 0.001". For other values, it returns a string with the appropriate formatting.
#'
#' @export
#' @import stats
#'
#' @examples
#' # Examples of using the pval_string function
#' pval_string(0.054)  # Returns "0.054"
#' pval_string(0.050)  # Returns "0.050"
#' pval_string(0.00002)  # Returns "< 0.001"
#' pval_string(0.3454149)  # Returns "0.34"
#' pval_string(1)  # Returns "1.00"

pval_string = function(valor){
  if (is.na(valor)){return(NA)}

  valor_str = formatC(valor, format = "f", digits = 6)
  if (valor < 0.05){
    if (valor < 0.001 || valor == 0){return("< 0.001")}
    else {
      if (valor < 0.01){return(substring(as.character(valor_str), 1, 5))}
      else {return(substring(as.character(valor_str), 1, 4))}
    }}
  else {
    if (valor < 0.06){return(substring(as.character(valor_str), 1, 5))}
    else {return(substring(as.character(valor_str), 1, 4))}}
}

# pval_string2 = function(valor){
#   if (!is.character(valor)){valor = pval_string(valor)}
#
#   if (valor == "< 0.001"){return("P-Value < 0.001")}
#   else{ return(paste("P-Value =", valor))}
# }
