#' binary_model_eval
#'
#' Evaluates binary classification models using various metrics.
#'
#' This function assesses the performance of binary classification models, including Generalized Linear Models (GLM)
#' and Generalized Linear Mixed Models (GLMM). It computes various evaluation metrics such as accuracy, precision, sensitivity,
#' specificity, F1 score, and area under the ROC curve (AUC). Additionally, it calculates model-specific statistics such as
#' pseudo R-squared values (McFadden and Nagelkerke) and information criteria (AIC, BIC). If the model is a GLMM, the function
#' also provides conditional and marginal R-squared values.
#'
#' @param modelo A fitted binary classification model object. Supported models include `glm` for logistic regression and `glmerMod` for mixed-effects logistic regression.
#'
#' @return A named numeric vector containing the evaluation metrics:
#' \item{acuracia}{Accuracy of the model.}
#' \item{precisao}{Precision (positive predictive value) of the model.}
#' \item{sensibilidade}{Sensitivity (recall) of the model.}
#' \item{especificidade}{Specificity of the model.}
#' \item{F1_Score}{F1 score of the model.}
#' \item{AUC}{Area under the ROC curve.}
#' \item{Pseudo_R2.McFadden}{McFadden's pseudo R-squared.}
#' \item{Pseudo_R2}{Nagelkerke's pseudo R-squared.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{VIF}{Variance Inflation Factor indicating multicollinearity (if applicable).}
#' \item{R2M}{Marginal R-squared (for GLMM).}
#' \item{R2c}{Conditional R-squared (for GLMM).}
#' \item{Status}{Indicates successful completion (1) or an error (0).}
#'
#' @export
#' @import stats
#' @import dplyr
#' @import magrittr
#' @import lmerTest
#' @import MuMIn
#' @import caret
#' @import DescTools
#' @import car
#' @import pROC
#' @import MuMIn
#'
#' @examples
#' # Example of using the binary_model_eval function with a logistic regression model
#' model <- glm(vs ~ am + carb, data = mtcars, family = binomial())
#' binary_model_eval(model)
#'
#' # Example of using the function with a mixed-effects logistic regression model
#' #library(lmerTest)
#' #model_mixed <- lmerTest::glmer(as.factor(vs) ~ am + (1|carb), data = mtcars, family = binomial())
#' #binary_model_eval(model_mixed)
#'
#' # The function returns a named vector with the calculated metrics

binary_model_eval = function(modelo){
  data_df = model.frame(modelo)

  formula_modelo <- formula(modelo)
  variavel_dependente <- as.character(formula_modelo[[2]])
  vars_independentes <- as.character(formula_modelo[[3]])

  formula_texto = paste0(variavel_dependente, "~", vars_independentes)
  vars_independentes_lista <- unlist(strsplit(vars_independentes, split = "\\+"))

  tryCatch({
    # Previsoes e dados reais
    df_clean = data_df #[complete.cases(data_df[, c(variavel_dependente, vars_independentes_lista)]), ]
    previsoes = predict(modelo, newdata = df_clean, type = "response")

    # Classificacao binaria das previsoes
    previsoes_bin = ifelse(previsoes > 0.5, 1, 0)
    dados_reais = df_clean[[variavel_dependente]]

    # Metricas de Avaliacao
    matrix = caret::confusionMatrix(as.factor(previsoes_bin), as.factor(dados_reais), positive = "1")
    acuracia = matrix$overall['Accuracy']
    precisao = matrix$byClass['Pos Pred Value']
    sensibilidade = matrix$byClass['Sensitivity']
    especificidade = matrix$byClass['Specificity']
    f1 = matrix$byClass['F1']
    auc = pROC::roc(dados_reais, previsoes)$auc # Curva roc

    # Extracao dos valores de TP, TN, FP, FN
    tp = matrix$table[2, 2]
    tn = matrix$table[1, 1]
    fp = matrix$table[1, 2]
    fn = matrix$table[2, 1]

    # Criterio de informacao de (AKAIKE/BAYESIANO)
    aic = AIC(modelo)
    bic = BIC(modelo)

    # MODELANDO ESPECIFICACOES DO MODELO
    if (inherits(modelo, "glm")) { # Modelo GLM
      # Pseudo R2
      pseudo_r2_McFadden = 1 - (modelo$deviance / modelo$null.deviance)
      pseudo_r2_Nagelkerke = DescTools::PseudoR2(modelo, which = "Nagelkerke")

      # null_model <- glm(target ~ 1, data = dfTrain, family = binomial)
      # pseudo_r2 <- 1 - logLik(model)/logLik(null_model)
      # print(paste("Pseudo R2: ", pseudo_r2))

      # VIF (Variance inflation factor) - Multicolinearidade
      if (length(vars_independentes_lista) > 1) {
        VIF = any(car::vif(modelo) > 10) %>% as.numeric()
      } else {
        VIF = 0
      }

      resultados = c(# tp=tp, tn=tn, fp=fp, fn=fn,
        acuracia, precisao, sensibilidade, especificidade,
        F1_Score = f1, AUC = auc,
        Pseudo_R2.McFadden = pseudo_r2_McFadden, Pseudo_R2 = pseudo_r2_Nagelkerke,
        AIC = aic, BIC = bic, VIF = VIF,
        Status = 1
      )
      return(resultados)

    } else if (inherits(modelo, "glmerMod")) { # Modelo Generalizado **Misto**
      warning("Voce inseriu um modelo misto")

      # coeficiente de determinacao condicional
      R2M = MuMIn::r.squaredGLMM(modelo)[1]
      # coeficiente marginal
      R2c = MuMIn::r.squaredGLMM(modelo)[2]

      resultados = c(# tp=tp, tn=tn, fp=fp, fn=fn,
        acuracia, precisao, sensibilidade, especificidade,
        F1_Score = f1, AUC = auc,
        R2M = R2M, R2c = R2c,
        AIC = aic, BIC = bic, #VIF = VIF,
        Status = 1
      )
      return(resultados)

    } else {
      warning("O modelo fornecido nao e suportado. Use um modelo 'glm' ou 'lmerMod'.")
      return(NULL)
    }
  }, error = function(e) {
    print(paste0('Erro com o modelo: ', formula_texto))
    return(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
  })
}
