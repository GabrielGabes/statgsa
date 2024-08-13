# Tabela contagem simples
freq_table = function(df, variavel){
  df %>% tabyl(.data[[variavel]], show_na = FALSE) %>%
    adorn_pct_formatting(2) %>% as.data.frame()
}


# Função que verifica se é melhor aplicavel o teste de fisher para testar a hipotese entre duas variaveis categoricas
is_fisher_applicable = function(df, var1, var2){
  length1 = length(levels(as.factor(df[[var1]])))
  length2 = length(levels(as.factor(df[[var2]])))
  if (length1 >= 3 || length2 >= 3){
    return(FALSE)
  }
  else {
    # Criar tabela de contignecia
    tabela = table(df[[var1]], df[[var2]])
    # Calcular as expectativas de frequência
    total_geral = sum(tabela)
    expectativas = outer(rowSums(tabela), colSums(tabela), FUN = "*") / total_geral
    # Verificar se alguma célula tem expectativa de frequência < 5
    return(any(expectativas < 5))
  }
}

# Tabela de contignecia
count_table = function(df, var_y, var_x, sentido_percent='col', apenas_fisher=F){
  #sentido_porcent => #col, row
  tabela = df %>%
    tabyl(.data[[var_x]], .data[[var_y]], show_na = FALSE) %>%
    adorn_totals(c("row", "col")) %>%
    adorn_percentages(sentido_percent) %>%
    adorn_pct_formatting(2) %>% adorn_ns

  #tabela = as.data.frame(tabela) %>%
  #mutate(across(where(is.character), ~ str_replace_all(.x, "(\\d+\\.\\d+)\\% *\\(? *(\\d+)\\)?", "\\2 (\\1%)")))
  tabela = rename(tabela, "Variable" = var_x)
  tabela = rbind(NA, tabela)
  tabela[["Variable"]] = as.character(tabela[["Variable"]])
  tabela[["Variable"]][1] = var_x
  tabela = tabela[-nrow(tabela), ] #excluindo ultima linha

  #Aplicando teste de hipotese adequado
  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if(is_fisher_applicable(df, var_y, var_x) == F){
    if (nrow(tabela) <= 3 && apenas_fisher == T){
      tabela[["P-value"]][1] = retorne_p(fisher.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Fisher Exact"}
    else {
      tabela[["P-value"]][1] = retorne_p(chisq.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Chi-squared"}}
  else{
    tabela[["P-value"]][1] = retorne_p(fisher.test(df[[var_x]],df[[var_y]])$p.value)
    tabela[["Test_Used"]][1] = "Fisher Exact"}

  # Reordenando as colunas, colocando coluna total para primeira posicao
  # Determina a sequência de índices das colunas
  indices = seq_len(ncol(tabela))
  # Altera a sequência para colocar a coluna -3 na posição 2
  indices = c(indices[1], indices[length(indices)-2], indices[2:(length(indices)-3)], indices[(length(indices)-1):length(indices)])
  # Reordena as colunas do dataframe de acordo com a sequência
  tabela = tabela[, indices]
  tabela[["Variable"]][1] = var_x

  # Ultimos Ajustes
  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'
  tabela[] = lapply(tabela, function(x) gsub("%", "", x))
  tabela[] = lapply(tabela, function(x) gsub("  ", " ", x))

  return(tabela %>% as.data.frame())
}

count_table(dff, "desfecho", "tratamentos")
