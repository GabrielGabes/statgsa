# Função criação e avaliação de modelos de classificação
library(caret)
library(DescTools)
library(car)
library(pROC)
library(MuMIn)

binary_model_eval = function(modelo){
  data_df = model.frame(modelo)

  formula_modelo <- formula(modelo)
  variavel_dependente <- as.character(formula_modelo[[2]])
  vars_independentes <- as.character(formula_modelo[[3]])

  formula_texto = paste0(variavel_dependente, "~", vars_independentes)
  vars_independentes_lista <- unlist(strsplit(vars_independentes, split = "\\+"))

  tryCatch({
    # Previsões e dados reais
    df_clean = data_df#[complete.cases(data_df[, c(variavel_dependente, vars_independentes_lista)]), ]
    previsoes = predict(modelo, newdata = df_clean, type = "response")

    # Classificação binária das previsões
    previsoes_bin = ifelse(previsoes > 0.5, 1, 0)
    dados_reais = df_clean[[variavel_dependente]]

    # Metricas de Avaliação
    matrix = caret::confusionMatrix(as.factor(previsoes_bin), as.factor(dados_reais), positive = "1")
    acuracia = matrix$overall['Accuracy']
    precisao = matrix$byClass['Pos Pred Value']
    sensibilidade = matrix$byClass['Sensitivity']
    especificidade = matrix$byClass['Specificity']
    f1 = matrix$byClass['F1']
    auc = pROC::roc(dados_reais, previsoes)$auc # Curva roc

    # Extração dos valores de TP, TN, FP, FN
    tp = matrix$table[2, 2]
    tn = matrix$table[1, 1]
    fp = matrix$table[1, 2]
    fn = matrix$table[2, 1]

    # Critério de informação de (AKAIKE/BAYESIANO)
    aic = AIC(modelo)
    bic = BIC(modelo)

    # MODELANDO ESPECIFICAÇÕES DO MODELO
    if (inherits(modelo, "glm")) { # Modelo GLM
      # Pseudo R²
      pseudo_r2_McFadden = 1 - (modelo$deviance / modelo$null.deviance)
      pseudo_r2_Nagelkerke = DescTools::PseudoR2(modelo, which = "Nagelkerke")

      # null_model <- glm(target ~ 1, data = dfTrain, family = binomial)
      # pseudo_r2 <- 1 - logLik(model)/logLik(null_model)
      # print(paste("Pseudo R²: ", pseudo_r2))

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
      warning("Você inseriu um modelo misto")

      # coeficiente de determinação condicional
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
      warning("O modelo fornecido não é suportado. Use um modelo 'glm' ou 'lmerMod'.")
      return(NULL)
    }
  }, error = function(e) {
    print(paste0('Erro com o modelo: ', formula_texto))
    return(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
  })
}

# modelo = glm(desfecho ~ fixed_effects + group, data = dff, family = binomial())
# binary_model_eval(modelo) %>% round(4)
#
# modelo_reduzido = glm(desfecho ~ fixed_effects, data = dff, family = binomial())
# binary_model_eval(modelo_reduzido) %>% round(4)
#
# modelo_misto = glmer(desfecho ~ fixed_effects + (1|group), data=dff, family = binomial())
# binary_model_eval(modelo_misto) %>% round(4)


# Função criação e avaliação de modelos regressão

pacman::p_load(MuMIn, # for r-squared for mixeds models
               lmerTest) # for mixxed models

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

# modelo_lm0 <- lm(response ~ fixed_effects, data = dff)
# metricas_de_avaliacao_regressao(modelo_lm0) %>% round(4)
#
# modelo_lm <- lm(response ~ fixed_effects + group, data = dff)
# metricas_de_avaliacao_regressao(modelo_lm) %>% round(4)
#
# modelo <- lmer(response ~ fixed_effects + (1|group), data = dff)
# metricas_de_avaliacao_regressao(modelo) %>% round(4)
