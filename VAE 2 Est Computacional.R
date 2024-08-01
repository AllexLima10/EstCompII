#1
#a
x=c(10.216 , 8.909 , 9.301 , 8.858 , 2.341 , 8.759 , 9.740 , 0.296 , 8.744 , 8.458 , 17.202 , 0.658 , 1.537 ,
    1.581 , 1.703 , 0.897 , 4.822 , 0.180 , 0.535 , 23.701 , 3.648 , 2.491 , 0.204 , 10.523 , 8.568 , 7.472 ,
    1.536 , 1.539 , 1.950 , 1.670 , 5.761 , 1.930 , 2.124 , 8.401 , 4.433 , 6.983 , 0.677 , 29.320 , 1.299 , 0.077) 
Jacknife <- function(amostra,f_estimado=function(x){mean(x)}){
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
  return(list(Estimador_Pontual=Est_Jacknife,Vies=Vies_Jack,Variancia_Amostral=S2_Jack))
}
q1 <- list()
q1$a <-Jacknife(x,f_estimado=function(x){1/mean(x)}) 
q1$a

#b
length(x)
Bootstrap_1_dado <- function(dado1,tamanho,f,alfa=NA){
  theta_chapeu <- f(dado1)
  thetab=NULL
  set.seed(240123)
  for(i in 1:tamanho){
    xj=dado1[sample(1:40,size=40,replace = T)]
    thetab[i] <- f(xj)
  }
  IC=NULL
  if(is.na(alfa)==F){
    thetab_ordenado <- sort(thetab)
    IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
  }
  Vies_theta_boot=abs(mean(thetab)-theta_chapeu)
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot))
}
q1$b <- Bootstrap_1_dado(x,200,function(x){1/mean(x)})
q1$b
#c
Bootstrap_1_dado <- function(dado1,tamanho,f,alfa=NA){
  theta_chapeu <- f(dado1)
  thetab=NULL
  set.seed(240123)
  for(i in 1:tamanho){
    xj=dado1[sample(1:length(dado1),size=length(dado1),replace = T)]
    thetab[i] <- f(xj)
  }
  IC=NULL
  if(is.na(alfa)==F){
    thetab_ordenado <- sort(thetab)
    IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
  }
  Vies_theta_boot=mean(thetab)-theta_chapeu
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot))
}
q1$c=Bootstrap_1_dado(x,200,function(x){1/mean(x)})



#d
q1$a$Vies>q1$b$Vies
q1$b$Vies>q1$c$Vies
(q1$d <- paste0("Como verificado a amostragem Bootstrap foi a que apresentou um menor viés"))

#2
f2 <- function(x){exp(x)}
f2_real <- integrate(function(x){exp(x)},0,1)$value
integral_trapezio <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
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
  
  return(list(integral=inttrap,erro=erro_atual))
}

integral_Simpson <- function(f,a,b,pedacos=0,erro=NULL,funcao_real=NULL){
  determinador=ifelse(pedacos==0,1,2)
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
  return(list(integral=intsimp,erro=erro_atual))
}
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  set.seed(240123)
  x=(b-a)*runif(n)
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(list(Integral_Estimada=I,Erro=erro))
}
q2 <- list(Trapezio=integral_trapezio(f2,0,1,1000,funcao_real=f2_real),Simpson=integral_Simpson(f2,0,1,1000,funcao_real = f2_real),Monte_Carlo=integral_Monte_Carlo(f2,0,1,10000,funcao_real = f2_real))
q2

#3
f3 <- function(x){(exp(-x)/(1+x^2))}
f3_imp <- function(x){(exp(-x))*ifelse(x>0,1,0)}
F3_imp_inv=function(u){ifelse(u<1-exp(-1),-log(1-u),1)}
Integral_Funcao_Importancia <- function(n,f,f_Imp,f_Imp_Inv){
  set.seed(240123)
  x=f_Imp_Inv(runif(n))
  funcaorazao=function(x){f(x)/f_Imp(x)}
  valor=f(x)
  resultado=mean(funcaorazao(x))
  return(resultado) 
}
(q3 <- paste0("O resultado da integral Monte Carlo utilizando a função de importancia definida é: ",Integral_Funcao_Importancia(10000,f3,f3_imp,F3_imp_inv)))

#4
aa=c(0.4686379,0.3293115,0.6710360,0.4683662,0.5331134,0.7766946,0.6313886,0.5652129,0.5734934,0.5154058,0.5913630,0.4264529,0.7109585,0.7814624,0.3937559,0.9695128,0.6059747,0.7193509,0.5534170,0.8323150,0.5088905,0.5274100,0.7061571,0.6474273,0.6867186,0.5316064,0.8238470,0.5512383,0.4694828,0.7005840) 
Metodo_Newton <- function(vetor_inicial,fx,jx,eps=10^-8,max_iter=1000){
  err <- eps+1
  i <- 0
  xnovo <- vetor_inicial
  while(err>eps & i<max_iter){
    i <- i+1
    xant <- xnovo
    nj <- jx(xant)
    xnovo <- xant-solve(nj)%*%fx(xant)
    err <- norm((xant-xnovo)/xant)
  }
  return(list(Vetor_Raiz=xnovo,Numero_iteracoes=i,Erro=err))  
}
n4=length(aa)
f4_alfa <- function(x,alfa,beta){n4*(digamma(alfa+beta)/gamma(alfa+beta))- n*(digamma(alfa)/gamma(alfa))+prod(x)}
f4_beta <- function(x,alfa,beta){n4*(digamma(alfa+beta)/gamma(alfa+beta))- n*(digamma(beta)/gamma(beta))+prod(1-x)}
Metodo_Newton(c(0,0),f4_alfa,j4)
Metodo_Newton(c(0,0),f4_beta,j4)
