# Valor de P
# Propósito: Esta função formata valores de p, permitindo uma apresentação mais clara e consistente dos resultados estatísticos. Isso é particularmente útil para apresentar resultados em artigos científicos ou relatórios, onde a precisão e a clareza na comunicação dos valores de p são essenciais.

pval_string = function(valor){
  valor_str = formatC(valor, format = "f", digits = 6)
  if (valor < 0.05){
    if (valor < 0.001 || valor == 0){"< 0.001"}
    else {
      if (valor < 0.01){substring(as.character(valor_str), 1, 5)}
      else {substring(as.character(valor_str), 1, 4)}
    }}
  else {
    if (valor < 0.06){substring(as.character(valor_str), 1, 5)}
    else {substring(as.character(valor_str), 1, 4)}}
}
pval_string(0.054)
pval_string(0.050)
pval_string(0.059)
pval_string(0.045)
pval_string(0.4)
pval_string(0.3454149)
pval_string(0.399949)
pval_string(0.04)
pval_string(0.002)
pval_string(0.00002)


pval_string2 = function(valor){
  if (!is.character(valor)){valor = pval_string(valor)}

  if (valor == "< 0.001"){"P-Value < 0.001"
  } else{ paste("P-Value =", valor)}
}
pval_string2(0.00002)
pval_string2(0.399949)
pval_string2(0.04)
