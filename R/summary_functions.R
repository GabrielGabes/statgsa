summary_parametric = function(df, col_num){
  tabela = df %>%
    filter(!is.na(!!sym(col_num))) %>%
    summarise(
      mean = round(mean(!!sym(col_num), na.rm = TRUE), 2),
      std = round(sd(!!sym(col_num), na.rm = TRUE), 2))

  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]

  return(tabela)
}
summary_parametric(dff, 'var_num')

library(effsize) #tamanho do efeito d'cohen
summary_parametric_groups = function(df, col_num, col_cat, teste_extra="F"){
  # Sumário por grupo
  sumario_grupo = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2),
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
    )
  sumario_grupo = rename(sumario_grupo, "coluna" = col_cat)

  # Sumário geral (total)
  sumario_geral = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2),
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
    )

  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num

  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA

  if ( length(levels(as.factor(df[[col_cat]]))) <= 2 ){
    pvalor = retorne_p(t.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "T Test"

    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      d_cohen = cohen.d(grupo1, grupo2)
      estimador = as.character(rround(d_cohen$estimate,2))
      IC_00 = as.character(rround(d_cohen$conf.int[1],2))
      IC_01 = as.character(rround(d_cohen$conf.int[2],2))
      d_cohen = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = d_cohen}
  }else {
    pvalor = summary(aov(df[[col_num]]~df[[col_cat]]))[[1]][["Pr(>F)"]][1]
    pvalor = retorne_p(pvalor)
    tabela[["Test_Used"]][1] = "Anova"
  }

  tabela[["P-value"]] = pvalor

  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira

  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'

  return(tabela)
}
summary_parametric_groups(dff, "var_num", "desfecho", 'T')


summary_nonparametric = function(df, col_num){
  tabela = df %>%
    filter(!is.na(!!sym(col_num))) %>%
    summarise(
      median = round(median(!!sym(col_num), na.rm = TRUE), 2),
      q1_q3 = paste0(
        '[', round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2), ']'))

  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]

  return(tabela)
}

summary_nonparametric_groups = function(df, col_num, col_cat, teste_extra="F"){
  # Sumário por grupo
  sumario_grupo = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2),
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )
  sumario_grupo = rename(sumario_grupo, "coluna" = col_cat)

  # Sumário geral (total)
  sumario_geral = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2),
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2),
        " - ",
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )

  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num

  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if (length(levels(as.factor(df[[col_cat]]))) > 2){
    pvalor = retorne_p(kruskal.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Kruskal-Wallis"}
  else{
    pvalor = retorne_p(wilcox.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Mann-Whitney"

    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      teste_hip = wilcox.test(grupo1, grupo2, conf.int = TRUE)

      #Estimador Hodges Lehmann
      estimador = as.character(rround(teste_hip$estimate,2))
      IC_00 = as.character(rround(teste_hip$conf.int[1],2))
      IC_01 = as.character(rround(teste_hip$conf.int[2],2))
      hodges_lehmann = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = hodges_lehmann
    }
  }
  tabela[["P-value"]] = pvalor

  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira

  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'

  return(tabela)
}
summary_nonparametric_groups(dff, "var_num", "desfecho", 'T')


summary_groups = function(df, col_num, col_cat){
  if (group_normality_test(df, col_num, col_cat)){
    summary_parametric_groups(df, col_num, col_cat) %>% return()
  } else{
    summary_nonparametric_groups(df, col_num, col_cat) %>% return()
  }
}
summary_groups(dff, "var_num", "desfecho")
