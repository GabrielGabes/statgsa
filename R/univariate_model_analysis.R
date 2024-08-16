#' Univariate Model Analysis
#'
#' This function performs a univariate analysis on a given model, calculating either the Odds Ratio (OR) or Relative Risk (RR) depending on the type of model provided.
#' The function handles models of class `glm` with either a `binomial` or `quasibinomial` family.
#'
#' @param modelo A model object of class `glm`. The model should be either logistic regression (with family `binomial`) for Odds Ratio calculation or quasibinomial regression for Relative Risk calculation.
#'
#' @return A data frame containing the calculated Odds Ratio or Relative Risk, along with their confidence intervals and p-values. If the model is not supported, the function returns `NULL`.
#'
#' @examples
#' # Logistic regression example
#' model <- glm(vs ~ am + carb, data = mtcars, family = binomial())
#' univariate_model_analysis(model)
#'
#' # Quasibinomial regression example
#' model <- glm(vs ~ am + carb, data = mtcars, family = poisson())
#' univariate_model_analysis(model)
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr

univariate_model_analysis <- function(modelo) {
  # Verifica se o modelo e do tipo "glm" com link binomial (regressao logistica)
  if (inherits(modelo, "glm") && family(modelo)$family == "binomial") {
    # Calcula a Razao de Chances (Odds Ratio)
    estimadores <- as.data.frame(summary(modelo)$coefficients)
    odds <- as.data.frame(exp(cbind(OR = coef(modelo), confint(modelo))))

    estimadores[["Pr(>|z|)"]] = sapply(estimadores[["Pr(>|z|)"]], function(x){pval_string(x)})
    odds[["OR"]] = sapply(odds[["OR"]], function(x){num_string(x, digits = 2)})
    odds[["2.5 %"]] = sapply(odds[["2.5 %"]], function(x){num_string(x, digits = 2)})
    odds[["97.5 %"]] = sapply(odds[["97.5 %"]], function(x){num_string(x, digits = 2)})

    tabela <- cbind(odds, estimadores)
    tabela <- tabela[-1, ] # Remove a primeira linha, que e o intercepto

    # Remove colunas desnecessarias
    tabela$Estimate <- NULL
    tabela$`Std. Error` <- NULL
    tabela$`z value` <- NULL

    return(tabela)

    # Verifica se o modelo e adequado para calcular o Risco Relativo
  } else if (inherits(modelo, "glm") && family(modelo)$family == "poisson") {
    warning("Risco Relativo pode ser mais apropriado para este modelo.")
    # Aqui você pode adicionar o código para calcular o Risco Relativo, se aplicavel
    # (Abaixo esta um exemplo simples)

    estimadores <- as.data.frame(summary(modelo)$coefficients)
    rr <- as.data.frame(exp(cbind(RR = coef(modelo), confint(modelo))))

    estimadores[["Pr(>|z|)"]] = sapply(estimadores[["Pr(>|z|)"]], function(x){pval_string(x)})
    rr[["RR"]] <- sapply(rr[["RR"]], function(x){num_string(x, digits = 2)})
    rr[["2.5 %"]] <-sapply(rr[["2.5 %"]], function(x){num_string(x, digits = 2)})
    rr[["97.5 %"]] <- sapply(rr[["97.5 %"]], function(x){num_string(x, digits = 2)})

    tabela <- cbind(rr, estimadores)
    tabela <- tabela[-1, ] # Remove a primeira linha, que e o intercepto

    # Remove colunas desnecessarias
    tabela$Estimate <- NULL
    tabela$`Std. Error` <- NULL
    tabela$`z value` <- NULL

    return(tabela)

  } else {
    warning("O modelo fornecido nao suporta a analise de Odds Ratio ou Risco Relativo. Use um modelo 'glm' com uma familia binomial ou quasibinomial.")
    return(NULL)
  }
}
