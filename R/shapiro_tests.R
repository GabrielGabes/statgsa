# Função de normalização
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

shapiro_test <- function(x) { return(shapiro.test(x)$p.value) }

group_shapiro_test = function(df, col_num, col_cat){
  tabela = aggregate(df[[col_num]]~df[[col_cat]], data=df, shapiro_test)
  resultados = tabela$`p-value`
  verificacao = any(resultados < 0.05)

  if (verificacao){
    return(FALSE) # Alguma distribuição não segue a normal
  } else{
    return(TRUE)} # Todas as distribuições seguem a normal
}

group_ks_test = function(df, col_num, col_cat){
  resultados <- by(df[[col_num]], df[[col_cat]], function(subset) {
    ks.test(subset, "pnorm", mean(subset), sd(subset))
  })

  p.values <- sapply(resultados, function(test) test$p.value)
  verificacao = any(p.values < 0.05)

  if (verificacao){
    return(FALSE)  # Alguma distribuição não segue a normal
  } else {
    return(TRUE)  # Todas as distribuições seguem a normal
  }
}

group_normality_test = function(df, col_num, col_cat){
  tabela = df %>% filter(!is.na(!!sym(col_num)) & !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>% summarise(n = n())
  resultados = tabela$n

  # verificar se algum dos grupos é maior que 5000
  verificacao = any(resultados > 5000)

  if (verificacao){
    group_ks_test(df, col_num, col_cat) %>% return()
  } else {
    group_shapiro_test(df, col_num, col_cat) %>% return()
  }
}
