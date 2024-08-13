univariate_model_analysis <- function(modelo) {
  # Verifica se o modelo é do tipo "glm" com link binomial (regressão logística)
  if (inherits(modelo, "glm") && family(modelo)$family == "binomial") {
    # Calcula a Razão de Chances (Odds Ratio)
    estimadores <- as.data.frame(summary(modelo)$coefficients)
    odds <- as.data.frame(exp(cbind(OR = coef(modelo), confint(modelo))))

    estimadores <- apply_retorne_p(estimadores, "Pr(>|z|)")
    odds <- apply_rround(odds, "OR")
    odds <- apply_rround(odds, "2.5 %")
    odds <- apply_rround(odds, "97.5 %")

    tabela <- cbind(odds, estimadores)
    tabela <- tabela[-1, ] # Remove a primeira linha, que é o intercepto

    # Remove colunas desnecessárias
    tabela$Estimate <- NULL
    tabela$`Std. Error` <- NULL
    tabela$`z value` <- NULL

    return(tabela)

    # Verifica se o modelo é adequado para calcular o Risco Relativo
  } else if (inherits(modelo, "glm") && family(modelo)$family == "quasibinomial") {
    warning("Risco Relativo pode ser mais apropriado para este modelo.")
    # Aqui você pode adicionar o código para calcular o Risco Relativo, se aplicável
    # (Abaixo está um exemplo simples)

    rr <- as.data.frame(exp(cbind(RR = coef(modelo), confint(modelo))))

    rr <- apply_rround(rr, "RR")
    rr <- apply_rround(rr, "2.5 %")
    rr <- apply_rround(rr, "97.5 %")

    # Retorne a tabela de Risco Relativo
    return(rr)

  } else {
    warning("O modelo fornecido não suporta a análise de Odds Ratio ou Risco Relativo. Use um modelo 'glm' com uma família binomial ou quasibinomial.")
    return(NULL)
  }
}
