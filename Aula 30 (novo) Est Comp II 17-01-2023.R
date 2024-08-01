Bootstrap_2_dados <- function(dado1,dado2,tamanho,f,alfa=NA){
  theta_chapeu <- f(dado1,dado2)
  dados=cbind(dado1,dado2)
  thetab=NULL
  for(i in 1:tamanho){
    xj=dados[sample(1:nrow(dados),size=nrow(dados),replace = T),]
    thetab[i] <- f(xj[,1],xj[,2])
  }
  IC=NULL
  if(is.na(alfa)==F){
    thetab_ordenado <- sort(thetab)
    IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
  }
  Vies_theta_boot=mean(thetab)-theta_chapeu
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot,IC))
}

Bootstrap_1_dado <- function(dado1,tamanho,f,alfa=NA){
  theta_chapeu <- f(dado1)
  thetab=NULL
  for(i in 1:tamanho){
    xj=dado1[sample(1:nrow(dados),size=nrow(dados),replace = T)]
    thetab[i] <- f(xj)
  }
  IC=NULL
  if(is.na(alfa)==F){
    thetab_ordenado <- sort(thetab)
    IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
  }
  Vies_theta_boot=mean(thetab)-theta_chapeu
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot,IC))
}
Bootstrap_1_dado(cbind(sample(1:1000,15),sample(1:1000,15)),tamanho = 10000,f=mean,alfa=0.05)
Bootstrap_2_dados(sample(1:1000,15),sample(1:1000,15),1200,function(x,y){x-y})




