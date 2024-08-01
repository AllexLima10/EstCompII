#Calcular o valor esperado da qui-quadrado(k), onde k= 2 g.l.: 
set.seed(29112022)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  x=(b-a)*runif(n)+a
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(c(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
f <- function(x){(1/2)*x*exp(-0.5*x)}
integral_Monte_Carlo(f,0,20,1000,2)

#Seja X variável aleatória da Exp(1). Calcule E(X) usando o método de Monte Carlo e calcule também o erro de aproximação e a estimativa do desviopadrão,com: n=100 e n=1000
set.seed(29112022)
integral_Monte_Carlo_por_media_exp <- function(g=function(x){x},n=1000,lambda=1,a=0,b=Inf,funcao_real=NULL){
  amostra_exp <- (-1/lambda)*log(1-runif(n))
  media_amostral <- g(mean(amostra_exp))
  erro=abs(media_amostral-funcao_real)
  VI=(1/n)*var(g(amostra_exp))
  return(c(Media_Estimada=media_amostral,Erro=erro,Variancia_Estimada=VI))
}
integral_Monte_Carlo_por_media_exp(n=1000,lambda=1/2,funcao_real = 2)
integral_Monte_Carlo_por_media_exp(n=10000,lambda=1/2,funcao_real = 2)


#Forma function para qualquer distribuição (é necessário obter a inversa)
integral_Monte_Carlo_por_media_geral <- function(g=function(x){x},inversa,n=1000,lambda=1,a=0,b=Inf,funcao_real=NULL){
  amostra <- inversa(runif(n))
  media_amostral <- g(mean(amostra))
  erro=abs(media_amostral-funcao_real)
  VI=(1/n)*var(g(amostra))
  return(c(Media_Estimada=media_amostral,Erro=erro,Variancia_Estimada=VI))
}
set.seed(29112022)
integral_Monte_Carlo_por_media_geral(inversa=function(x){(-1/(1/2))*log(1-x)},n=1000,lambda=1/2,funcao_real = 2)
integral_Monte_Carlo_por_media_geral(inversa=function(x){(-1/(1/2))*log(1-x)},n=10000,lambda=1/2,funcao_real = 2)
