#
set.seed(06122022)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  x=(b-a)*runif(n)+a
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(c(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
#Integra??o de Monte Carlo por m?dia
integral_Monte_Carlo_por_media_geral <- function(g=function(x){x},inversa,n=1000,lambda=1,a=0,b=Inf,funcao_real=NULL){
  amostra <- inversa(runif(n))
  media_amostral <- mean(g(amostra))
  erro=abs(media_amostral-funcao_real)
  VI=(1/n)*var(g(amostra))
  return(c(Media_Estimada=media_amostral,Erro=erro,Variancia_Estimada=VI))
}
#Integra??o de Monte Carlo por m?dia
integral_Monte_Carlo_por_media_geral_densidade_incompleta <- function(g=function(x){x},k,f,inversa,n=1000,lambda=1,a=0,b=Inf,funcao_real=NULL){
  amostra <- inversa(runif(n))
  Y <- (g(amostra)*k(amostra))/f(amostra)
  Z <- k(amostra)/f(amostra)
  media_amostral <- mean(Y)/mean(Z)
  erro=abs(media_amostral-funcao_real)
  VI=(1/n)*var(g(amostra))
  return(c(Media_Estimada=media_amostral,Erro=erro,Variancia_Estimada=VI))
}

Integral_Funcao_Importancia <- function(n,f,f_Imp,f_Imp_Inv){
  x=f_Imp_Inv(runif(n))
  funcaorazao=function(x){f(x)/f_Imp(x)}
  valor=f(x)
  resultado=mean(funcaorazao(x))
  return(resultado) 
}

Integral_Funcao_Importancia_geral_densidade_incompleta <- function(g=function(x){x},k,f_Imp,f_Imp_Inv,n=1000,lambda=1,a=0,b=Inf,funcao_real=NULL){
  X <- f_Imp_Inv(runif(n))
  Y <- (g(X)*k(X))/f(X)
  Z <- k(X)/f(X)
  media_amostral <- mean(Y)/mean(Z)
  erro=abs(media_amostral-funcao_real)
  VI=(1/n)*var(g(X))
  return(c(Media_Estimada=media_amostral,Erro=erro,Variancia_Estimada=VI))
}


#Estime P{0,1<X<0,5} usando integra??o de Monte Carlo, quando k(x) = x(1-x)^3 , com 0<x<1
set.seed(06122022)
integral_Monte_Carlo_por_media_geral_densidade_incompleta(g=function(x){ifelse(x>0.1 & x<0.5,1,0)},k=function(x){x*(1-x)^3},inversa=function(x){x},f=function(x){ifelse(x>0 & x<1,1,0)},a=0,b=0.5,funcao_real = pbeta(0.5,2,4)-pbeta(0.1,2,4),n=1000000)

