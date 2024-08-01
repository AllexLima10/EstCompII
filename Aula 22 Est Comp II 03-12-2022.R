#Função Necessária
set.seed(031222)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  x=(b-a)*runif(n)+a
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(c(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
#ConsidereX~Beta(2,2).EstimeE(X2)usandointegraçãodeMonteCarloviafunçãode importânciacomaU(0,1)comofunçãodeimportância
set.seed(031222)
Integral_Funcao_Importancia <- function(n,f,f_Imp,f_Imp_Inv){
  x=f_Imp_Inv(runif(n))
  funcaorazao=function(x){f(x)/f_Imp(x)}
  valor=f(x)
  resultado=mean(funcaorazao(x))
  return(resultado) 
}
Integral_Funcao_Importancia(n=1000,f=function(x){6*x^3*(1-x)*ifelse(x>1,0,1)},f_Imp=function(x){1},f_Imp_Inv=function(x){x})

#Considere X ~Beta(2,2). Estime E(X2) usando integração de Monte Carlo via funçãodeimportânciacomaExp(1)comofunçãodeimportância
Integral_Funcao_Importancia(n=1000,f=function(x){6*x^3*(1-x)*ifelse(x>1,0,1)},f_Imp=function(x){exp(-x)},f_Imp_Inv=function(x){-log(1-x)})

