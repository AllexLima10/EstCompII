#Questão 1
f1=function(x){1/(pi*(1+x^2))}
q1 <- list()
f1_real=atan(2)/pi #arctg(2)/pi é o resultado da acumulada dessa função de 0 a 2 
#a)
integral_trapezio <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
  #x=seq(a,b,l=I(pedacos+1))
  inttrap=0
  if(determinador==2){
    x=a+((0:(pedacos))*((b-a)/(pedacos)))
    for(i in 1:(pedacos)){
      inttrap=inttrap+((f(x[i])+f(x[i+1]))/2)*(x[i+1]-x[i])
    }
    erro_atual=ifelse(is.null(funcao_real),NULL,abs(inttrap-funcao_real))
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
  
  return(list(integral=inttrap,pedacos,erro=erro_atual))
}


q1$a <- list(n_1000 <- list(Probabilidade=1/2-integral_trapezio(f1,0,2,pedacos = 1000,funcao_real=f1_real)$integral,erro=integral_trapezio(f1,0,2,pedacos = 1000,funcao_real=f1_real)$erro),n_10000=list(Probabilidade=1/2-integral_trapezio(f1,0,2,pedacos = 10000,funcao_real=f1_real)$integral,erro=integral_trapezio(f1,0,2,pedacos = 10000,funcao_real=f1_real)$erro))
q1$a

#b)
integral_Simpson <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
  #x=seq(a,b,l=I(pedacos+1))
  if(determinador==2){
    x=a+((0:(pedacos))*((b-a)/(pedacos)))
    intsimp=0
    for(i in 1:(pedacos)){
      intsimp=intsimp+((x[i+1]-x[i])/6)*(f(x[i])+4*f((x[i]+x[i+1])/2)+f(x[i+1]))
    }
    erro_atual=ifelse(is.null(funcao_real),NULL,abs(intsimp-funcao_real))  }
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
  return(list(integral=intsimp,pedacos,erro=erro_atual))
}
integral_Simpson(f1,0,2,1000,funcao_real=f1_real)
q1$b <- list(n_1000 <- list(Probabilidade=1/2-integral_Simpson(f1,0,2,1000,funcao_real=f1_real)$integral,erro=integral_Simpson(f1,0,2,1000,funcao_real=f1_real)$erro),n_10000=list(Probabilidade=1/2-integral_Simpson(f1,0,2,10000,funcao_real=f1_real)$integral,erro=integral_Simpson(f1,0,2,10000,funcao_real=f1_real)$erro))
q1$b

#c)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  x=(b-a)*runif(n)
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(list(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
integral_Monte_Carlo(f1,0,2,1000,f1_real)
q1$c <- list(n_1000 <- list(Probabilidade=1/2-integral_Monte_Carlo(f1,0,2,1000,f1_real)$Integral_Estimada,erro=integral_Monte_Carlo(f1,0,2,1000,f1_real)$Erro,Desvio_padrao_estimado=sqrt(integral_Monte_Carlo(f1,0,2,1000,f1_real)$Variancia_Estimada)
),n_10000=list(Probabilidade=1/2-integral_Monte_Carlo(f1,0,2,10000,f1_real)$Integral_Estimada,erro=integral_Monte_Carlo(f1,0,2,10000,f1_real)$Erro,Desvio_padrao_estimado=sqrt(integral_Monte_Carlo(f1,0,2,10000,f1_real)$Variancia_Estimada)
))
q1$c

#d)
set.seed(10)
 funcao_imp_1d <- function(x){ifelse(x>2,2/x^2,1)}
 funcao_inv_1d <- function(x){ifelse(x<1/2,2/(1-x),1)} #função calculada no caderno (escreva a equação de y e tente encontrar uma equação para x)
 Integral_Funcao_Importancia <- function(n,f,f_Imp,f_Imp_Inv){
     x=f_Imp_Inv(runif(n))
     funcaorazao=function(x){f(x)/f_Imp(x)}
    valor=f(x)
     resultado=mean(funcaorazao(x))
     return(resultado) 
   }
 Integral_Funcao_Importancia(n=1000,f=f1,f_Imp=funcao_imp_1d,f_Imp_Inv=funcao_inv_1d)







