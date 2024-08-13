num_string = function(valor, digitos) {
  if (is.na(valor)) {
    return(NA)
  }

  if (abs(valor) < 0.01 && abs(valor) > 0.0009) {
    return(formatC(valor, format = "f", digits = 3))
  } else if (valor == 0) {
    return("0.00")
  } else if (abs(valor) <= 0.0009 && valor != 0 || abs(valor) > 10000) {
    return(formatC(valor, format = "e", digits = digitos))
  } else {
    return(formatC(valor, format = "f", digits = digitos))
  }
}

num_string(30, 2)
num_string(0, 2)
num_string(0.001, 2)
num_string(0.0001, 2)
num_string(0.041212, 2)
num_string(0.1, 2)
num_string(45.5151, 2)
num_string(5115156, 2)
num_string(0.0000000041212, 2)
