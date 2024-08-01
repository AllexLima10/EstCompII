bootstrap.media <- function(x, B, nivel.confianca=0.95) {
  # Inicializar um vetor para armazenar os resultados
  resultados <- numeric(B)
  # Realizar o Bootstrap
  for (i in 1:B) {
    # Selecionar uma amostra com reposição da amostra original
    x.bootstrap <- x[sample(1:length(x), length(x), replace = TRUE)]
    # Calcular a média da amostra selecionada
    resultados[i] <- mean(x.bootstrap)
  }
  # Ordenar os resultados
  resultados <- sort(resultados)
  hist(resultados)
  # Calcular o limite inferior e superior do intervalo de confiança
  limite.inferior <- resultados[floor(((1-nivel.confianca)/2)*B)]
  limite.superior <- resultados[ceiling(((1+nivel.confianca)/2)*B)]
  vies <- mean(resultados) - mean(x)
  var.estimador <- var(resultados)
  # Retornar os limites do intervalo de confiança
  return(list(intervalo=c(limite.inferior, limite.superior), nivel.confianca= paste0(nivel.confianca*100,'%'),vies=vies, var.estimador=var.estimador, EP=sqrt(var.estimador)))
}