adaptive_cross_table = function(df, coluna_analisada, sentido_percent='col', apenas_fisher=F, lista_colunas=names(df)){

  lista_coluna = lista_colunas[which(!(lista_colunas %in% c(coluna_analisada)))]

  colunas = levels(as.factor(df[[coluna_analisada]]))
  #c('Variable','Overall',)
  tabelona = summary_nonparametric_groups(df, "idade", coluna_analisada)[FALSE, ]

  for (coluna in lista_coluna){
    classe = class(df[[coluna]])[1]

    tryCatch({
      if (classe == "numeric"){
        local_erro = 'normalidade'
        if (group_normality_test(df, coluna, coluna_analisada) == TRUE){
          local_erro = 'teste parametrico'
          tabelinha = summary_parametric_groups(df, coluna, coluna_analisada)
        }
        else {
          local_erro = 'teste não parametrico'
          tabelinha = summary_nonparametric_groups(df, coluna, coluna_analisada)
        }
      }
      else if (classe == 'character' | classe == 'factor'){
        local_erro = 'tabela de tabela de contigencia'
        tabelinha = count_table(df, coluna_analisada, coluna, sentido_percent, apenas_fisher=F)
      }
      tabelona = rbind(tabelona, tabelinha)
    }, error = function(e) {
      print(paste('problema com a coluna:', coluna, '\ntipo erro:', local_erro))
    })
  }

  # Ajustes Finais
  colnames(tabelona)[colnames(tabelona) == "Overall"] = paste0("Overall 100% (n=", nrow(df[complete.cases(df[[coluna_analisada]]), ]), ")")
  niveis = levels(as.factor(df[[coluna_analisada]]))
  for (i in 1:length(niveis)){
    nivel = niveis[i]

    table_d = table( df[[coluna_analisada]] )
    prob_table = prop.table( table_d ) %>% round(4) * 100

    colnames(tabelona)[colnames(tabelona) == nivel] = paste0(nivel, ' ', prob_table[i], "% (n=", table_d[i], ")")
  }

  return(tabelona)
}
