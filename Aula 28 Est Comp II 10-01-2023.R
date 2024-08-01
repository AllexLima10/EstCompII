##Exemplo
#Uma aa de tamanho n=6 de uma dada população resultou nos seguintes dados: 7,2; 5,7; 4,9; 6,2; 8,5; 2,8
#Encontre a estimativa pontual da média µ.
#Construa o intervalo de confiança de 95% pelo método jacknife para a média populacional µ.
amostra <- c(7.2,5.7,4.9,6.2,8.5,2.8)
alfa=0.05
n <- length(amostra)
subamostras <- NULL
media_subamostras <- NULL
for (i in 1:length(amostra)) {
 subamostras[[i]]<- list(amostra[-i])
 media_subamostras <- c(media_subamostras,mean(subamostras[[i]][[1]])) 
 }
media_subamostral <- mean(as.vector(media_subamostras))
(Est_Jacknife <- n*mean(amostra)-(n-1)*media_subamostral)
(Vies_Jack <- mean(amostra)-Est_Jacknife)
#S2_Jack <- ((n-1)^2/n)*sum((media_subamostras-media_subamostral)^2/(n-1))
S2_Jack <- ((n-1)^2/n)*var(media_subamostras)
(intervalo=Est_Jacknife+c(qt(alfa/2,(n-1)),qt(1-alfa/2,(n-1)))*sqrt(S2_Jack))

#Forma function
amostra <- c(7.2,5.7,4.9,6.2,8.5,2.8)
alfa=0.05
Jacknife <- function(amostra,alfa,f_estimado=function(x){mean(x)}){
  n <- length(amostra)
  subamostras <- NULL
  est_subamostras <- NULL
  for (i in 1:length(amostra)) {
    subamostras[[i]]<- list(amostra[-i])
    est_subamostras <- c(est_subamostras,f_estimado(subamostras[[i]][[1]])) 
  }
  media_subamostral <- mean(as.vector(est_subamostras))
  Est_Jacknife <- n*f_estimado(amostra)-(n-1)*media_subamostral
  Vies_Jack <- f_estimado(amostra)-Est_Jacknife
  S2_Jack <- ((n-1)^2/n)*sum((est_subamostras-media_subamostral)^2/(n-1))
  intervalo=Est_Jacknife+c(qt(alfa/2,(n-1)),qt(1-alfa/2,(n-1)))*sqrt(S2_Jack)
  return(list(Estimador_Pontual=Est_Jacknife,Vies=Vies_Jack,Variancia_Amostral=S2_Jack,Intervalo_Confianca=intervalo))
  }
Jacknife(amostra,0.05,f_estimado = function(x){mean(x)})

amostra2 <- c(2.1,1.7,4,1.8,5)
Jacknife(amostra2,0.05,f_estimado = function(x){var(x)})
Jacknife(amostra2,0.05,f_estimado = function(x){min(x)})