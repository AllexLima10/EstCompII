#Calcular 𝑃𝑃 8 ≤ 𝑋𝑋 ≤ 13 , onde X ~ Gama(3,0,5), usando a regra do Trapézio e a regra de Simpson, e calcule também o erro de aproximação, com n=3 e n=8
integral_trapezio <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
  #x=seq(a,b,l=I(pedacos+1))
  inttrap=0
  print(determinador)
  if(determinador==2){
    x=a+((0:(pedacos))*((b-a)/(pedacos)))
    for(i in 1:(pedacos)){
    inttrap=inttrap+((f(x[i])+f(x[i+1]))/2)*(x[i+1]-x[i])
  }
  }
  else{
    erro_atual=erro+1
    print(erro)
    pedacos=0
    while(erro_atual>erro){
     pedacos=pedacos+1
     x=a+((0:(pedacos))*((b-a)/(pedacos)))
     i=0
     inttrap=0
     for(i in 1:(pedacos)){
       inttrap=inttrap+((f(x[i])+f(x[i+1]))/2)*(x[i+1]-x[i])
     }
     print(c(funcao_real,inttrap))
     erro_atual=abs(funcao_real-inttrap)
     print(erro_atual)
    }
    }
    
  return(c(inttrap,pedacos))
}
integral_trapezio(function(x){(x^2*exp(-x/2))/16},8,13,30)
integral_trapezio(function(x){(x^2*exp(-x/2))/16},8,13,1000)
integral_trapezio(function(x){(x^2*exp(-x/2))/16},a=8,b=13,erro=(10^-8),funcao_real = pgamma(13,3,1/2)-pgamma(8,3,1/2))


integral_Simpson <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
  #x=seq(a,b,l=I(pedacos+1))
  print(determinador)
  if(determinador==2){
    x=a+((0:(pedacos))*((b-a)/(pedacos)))
    intsimp=0
    for(i in 1:(pedacos)){
      intsimp=intsimp+((x[i+1]-x[i])/6)*(f(x[i])+4*f((x[i]+x[i+1])/2)+f(x[i+1]))
    }
  }
  else{
    erro_atual=erro+1
    print(erro)
    pedacos=0
    while(erro_atual>erro){
      pedacos=pedacos+1
      x=a+((0:(pedacos))*((b-a)/(pedacos)))
      i=0
      intsimp=0
      for(i in 1:(pedacos)){
        intsimp=intsimp+((x[i+1]-x[i])/6)*(f(x[i])+4*f((x[i]+x[i+1])/2)+f(x[i+1]))
      }
      print(c(funcao_real,intsimp))
      erro_atual=abs(funcao_real-intsimp)
      print(erro_atual)
    }
  }
  
  return(c(intsimp,pedacos))
}
integral_Simpson(function(x){(x^2*exp(-x/2))/16},8,13,30)
integral_Simpson(function(x){(x^2*exp(-x/2))/16},8,13,1000)
integral_Simpson(function(x){(x^2*exp(-x/2))/16},a=8,b=13,erro=(10^-8),funcao_real = pgamma(13,3,1/2)-pgamma(8,3,1/2))


#2º método
#Regra do Trapézio:
f1 <- function(x) {
  (1/16)*(x^2)*exp(-0.5*x)
}



vt=pgamma(13,3,0.5)-pgamma(8,3,0.5)
vt



x0=8
xn=13
n=0
erro=1000
emax=10^-8



while (erro>emax & n<10000) {
  n=n+1
  x=seq(x0,xn,l=I(n+1)) #l=length (n° de elementos no vetor x)
  int_trap=0
  i=0
  for (i in 1:n) {
    int_trap=int_trap+((f1(x[i])+f1(x[i+1]))/2)*(x[i+1]-x[i])
  }
  int_trap
  erro=abs(vt-int_trap)
}
n



#--------------------------------------------------------------------------------------------------------



#Regra de Simpson:
f1 <- function(x) {
  (1/16)*(x^2)*exp(-0.5*x)
}



vt=pgamma(13,3,0.5)-pgamma(8,3,0.5)
vt



x0=8
xn=13
n=0
erro=1000
emax=10^-8



while (erro>emax & n<10000) {
  n=n+1
  x=seq(x0,xn,l=I(n+1)) #l=length (n° de elementos no vetor x)
  int_simp=0
  i=0
  for (i in 1:n) {
    int_simp=int_simp+((x[i+1]-x[i])/6)*(f1(x[i])+4*f1((x[i]+x[i+1])/2)+f1(x[i+1]))
  }
  int_simp
  erro=abs(vt-int_simp)
}
n

#-------------------------------------------------------------------------------------------
set.seed(26112022)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
x=(b-a)*runif(n)+a
I=((b-a)/n)*sum(f(x))
VI=((b-a)^2/n)*var(f(x))
erro=abs(I-funcao_real)
return(c(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
integral_Monte_Carlo(function(x){exp(-x)},n=100000,a=0,b=30,funcao_real = pexp(30,1))

