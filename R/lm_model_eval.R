#' lm_model_eval
#'
#' Evaluates linear and mixed-effects regression models using various metrics.
#'
#' This function assesses the performance of linear models (`lm`) and mixed-effects models (`lmerMod`)
#' by calculating a range of evaluation metrics. These metrics include Mean Absolute Error (MAE), Mean Squared Error (MSE),
#' Root Mean Squared Error (RMSE), Mean Absolute Percentage Error (MAPE), and model information criteria such as AIC and BIC.
#' For mixed-effects models, the function also calculates conditional and marginal R-squared values.
#'
#' @param modelo A fitted regression model object. Supported models include `lm` for linear regression and `lmerMod` for mixed-effects linear regression.
#'
#' @return A named numeric vector containing the evaluation metrics:
#' \item{MAE}{Mean Absolute Error of the model.}
#' \item{MSE}{Mean Squared Error of the model.}
#' \item{RMSE}{Root Mean Squared Error of the model.}
#' \item{MAPE}{Mean Absolute Percentage Error of the model.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{R2}{R-squared for linear models.}
#' \item{R2_adj}{Adjusted R-squared for linear models.}
#' \item{R2M}{Marginal R-squared for mixed-effects models (proportion explained by fixed effects).}
#' \item{R2c}{Conditional R-squared for mixed-effects models (proportion explained by both fixed and random effects).}
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
#' @import MuMIn
#'
#' @examples
#' # Example of using the lm_model_eval function with a linear regression model
#' modelo_lm0 <- lm(mpg ~ disp, data = mtcars)
#' lm_model_eval(modelo_lm0)
#'
#' # Example of using the function with a more complex linear regression model
#' modelo_lm <- lm(mpg ~ disp + hp, data = mtcars)
#' lm_model_eval(modelo_lm)
#'
#' # Example of using the function with a mixed-effects model
#' # library(lmerTest)
#' # modelo_mixed <- lmerTest::lmer(mpg ~ disp + (1|carb), data = mtcars)
#' # lm_model_eval(modelo_mixed)
#'
#' # The function returns a named vector with the calculated metrics

lm_model_eval = function(modelo){
  data_df = model.frame(modelo)

  formula_modelo <- formula(modelo)
  variavel_dependente <- as.character(formula_modelo[[2]])
  vars_independentes <- as.character(formula_modelo[[3]])

  formula_texto = paste0(variavel_dependente, "~", vars_independentes)
  vars_independentes_lista <- unlist(strsplit(vars_independentes, split = "\\+"))

  tryCatch({
    # Previsões do modelo
    predictions = predict(modelo)

    # MAE - Mean Absolute Error
    MAE = mean(abs(data_df[[variavel_dependente]] - predictions))

    # MSE - Mean Squared Error
    MSE = mean((data_df[[variavel_dependente]] - predictions)^2)

    # RMSE - Root Mean Squared Error
    RMSE = sqrt(MSE)

    # MAPE - Mean Absolute Percentage Error
    MAPE = mean(abs((data_df[[variavel_dependente]] - predictions) / data_df[[variavel_dependente]])) * 100

    # Critério de informação de (AKAIKE/BAYESIANO)
    aic = AIC(modelo)
    bic = BIC(modelo)

    # MODELANDO ESPEFICAÇÕES DO MODELO
    # Verificando qual é o modelo
    if (inherits(modelo, "lm")) { # Modelo Linear
      #warning("vc inseriu um modelo linear")

      # VIF (Variance inflation factor) - Multicolinearidade
      # if (length(vars_independentes_lista) >= 2){
      #   tryCatch({
      #     VIF = any(car::vif(modelo) > 10) %>% as.numeric()
      #     }, error = function(e) {
      #       VIF = 1
      #       })
      # } else{
      #   VIF = 0
      # }
      VIF = 0

      # Summary do modelo
      summary_LM = modelo %>% summary()
      # R-quadrado (R²)
      r_squared <- summary_LM$r.squared
      # R-quadrado ajustado
      r_squared_adj <- summary_LM$adj.r.squared

      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE),
                      AIC = as.numeric(aic), BIC = as.numeric(bic),
                      R2 = as.numeric(r_squared), R2_adj = as.numeric(r_squared_adj),
                      VIF = as.numeric(VIF),
                      Status = 1)
      return(resultados)
    } else if (inherits(modelo, "lmerMod")) { # Modelo Linear **Misto**
      #warning("vc inseriu um modelo linear misto")

      # coeficiente de determinação condicional -> R2m = proporção explica apenas pelas variaveis fixas
      R2M = MuMIn::r.squaredGLMM(modelo)[1]
      # coeficiente marginal -> R2c = proporção explicada pelas variaveis fixas e aleatoria # R quadrado geral
      R2c = MuMIn::r.squaredGLMM(modelo)[2]

      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE),
                      AIC = as.numeric(aic), BIC = as.numeric(bic),
                      R2M = as.numeric(R2M), R2c = as.numeric(R2c),
                      #VIF = as.numeric(VIF),
                      Status = 1)
      return(resultados)
    } else {
      #warning("O modelo fornecido não é suportado. Use um modelo 'lm' ou 'lmerMod'.")

      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE),
                      AIC = as.numeric(aic), BIC = as.numeric(bic),
                      #VIF = as.numeric(VIF),
                      Status = 1)
      return(resultados)
    }
  }, error = function(e) {
    print(paste0('erro com o modelo: ', formula_texto))

    return(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
  })
}
