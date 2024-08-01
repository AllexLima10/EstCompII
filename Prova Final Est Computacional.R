
#Questao 1
#a
set.seed(310123)
gera_2_normais <- function(n){
  u1=runif(n)
  u2=runif(n)
  x1=NULL
  x2=NULL
  x1=sqrt(-2*log(u1))*cos(2*pi*u2)
  x2=sqrt(-2*log(u1))*sin(2*pi*u2)
  x=cbind(x1,x2)
  return(x)
}
q1 <- list()
(q1$a <- gera_2_normais(30)[,1]+5)

#b
set.seed(310123)
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
(q1$b <- Jacknife(q1$a,0.05,function(x){exp(mean(x))}))

#c

Bootstrap_1_dado <- function(dado1,tamanho,f,alfa=NA){
  theta_chapeu <- f(dado1)
  thetab=NULL
  set.seed(310123)
  for(i in 1:tamanho){
    xj=dado1[sample(1:length(dado1),size=length(dado1),replace = T)]
    thetab[i] <- f(xj)
  }
  IC=NULL
  if(is.na(alfa)==F){
    thetab_ordenado <- sort(thetab)
    IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
  }
  Vies_theta_boot=abs(mean(thetab)-theta_chapeu)
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot,Intervalo_Confianca=IC))
}
(q1$c <- Bootstrap_1_dado(dado1=q1$a,tamanho=200,f=function(x){exp(mean(x))},alfa = 0.05))

#d
Bootstrap_1_dado_p <- function(dado1,tamanho,f,alfa=NA){
   theta_chapeu <- f(dado1)
   thetab=NULL
   set.seed(310123)
   for(i in 1:tamanho){
     xj=runif(n,0,exp(mean(dado1)))
     thetab[i] <- f(xj)
   }
   IC=NULL
   if(is.na(alfa)==F){
     thetab_ordenado <- sort(thetab)
     IC <- c(quantile(thetab_ordenado,alfa/2),quantile(thetab_ordenado,1-alfa/2))
   }
   Vies_theta_boot=abs(mean(thetab)-theta_chapeu)
   desvio_theta_boot <- sqrt(var(thetab))
   return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot,Intervalo_Confianca=IC))
 }
(q1$d <- Bootstrap_1_dado_p(dado1=q1$a,tamanho=200,f=function(x){exp(mean(x))},alfa = 0.05))

paste0("tem algo de errado com o método paramétrico, necessário verificar")

#e
paste0("devido as amostras Jacknife fornecerem estimadores com um viés menor e apresentar um intervalo menor as de Bootstrap eu preferiria utilizar esse método")


#Questao 2 
f2 <- function(x){((15*x^3+21*x^2+41*x+3)^(1/4))*exp(-0.5*x)}
integrate(f2,0,4)
(valor_real <- integrate(f2,0,4)$value)
integral_Monte_Carlo <- function(f,a=-Inf,b=Inf,n=1000,funcao_real=NULL){
  x=(b-a)*runif(n)
  I=((b-a)/n)*sum(f(x))
  VI=((b-a)^2/n)*var(f(x))
  erro=abs(I-funcao_real)
  return(list(Integral_Estimada=I,Erro=erro,Variancia_Estimada=VI))
}
q2=list()
(q2$simples <- integral_Monte_Carlo(f2,0,4,n=10000,funcao_real = valor_real))

Integral_Funcao_Importancia <- function(n,f,f_Imp,f_Imp_Inv){
  x=f_Imp_Inv(runif(n))
  funcaorazao=function(x){f(x)/f_Imp(x)}
  resultado=mean(funcaorazao(x))
  vies <- abs(resultado-valor_real)
  return(list(Integral=resultado,Vies=vies)) 
}
f2_imp <- function(x){dexp(x,0.5)}
f2_imp_inv <- function(u){-2*log(1-u)}
(q2$Importancia <- Integral_Funcao_Importancia(100000,f2,f2_imp,f2_imp_inv))
paste0("Como é possível observar, o viés da integração por monte carlo simples, que é de ",q2$simples$Erro," é muito menor do que a da funcao de importancia que é ",q2$Importancia$Vies," o que indica que usar a função de exponencial fornecida como funcao de importancia não é uma boa abordagem")


#Questão 3 
aa=c(1409.4471,844.0588,1342.7956,1194.9779,823.7168,1080.8480,1549.6381,1188.8855,935.8013,1511.8703,1289.3882,825.1971,1183.6301,1106.3233,1045.7224,1101.1766,849.4615,1309.9896,1438.1696,952.7269,980.6783,1035.4598,973.6713,1291.0679,1236.6407,1289.5011,1038.0297,911.9891,976.8469,1298.3,145)
Metodo_Newton <- function(vetor_inicial,fx,jx,eps=10^-8,max_iter=1000){
  err <- eps+1
  i <- 0
  xnovo <- vetor_inicial
  while(err>eps & i<max_iter){
    i <- i+1
    xant <- xnovo
    nj <- jx(xant)
    print(solve(nj))
    xnovo <- xant-solve(nj)%*%fx(xant)
    err <- norm((xant-xnovo)/xant)
  }
  return(list(Vetor_Raiz=xnovo,Numero_iteracoes=i,Erro=err))  
}
n=length(aa)
l_mi <- function(mi,sigma2){(-1/2*sigma2)*(-sum(log(aa))-2*mi)}
l_mi_2 <- function(mi,sigma2){-1/sigma2}
l_sigma2 <- function(mi,sigma2){(-n/(2*sigma2))*(sum((log(aa)-mi)^2)/sigma2^2)}
l_sigma2_2 <-function(mi,sigma2) {(n/sigma2^2)-(sum((log(aa)-mi)^2)/sigma2^3)}
l_mi_sigma2 <- function(mi,sigma2){(-sum(log(aa))+2*mi)/sigma2^3}
jx <- function(p){cbind(c(l_mi_2(p[1],p[2]),l_mi_sigma2(p[1],p[2])),c(l_mi_sigma2(p[1],p[2]),l_sigma2_2(p[1],p[2])))}
fx <- function(p){c(l_mi(p[1],p[2]),l_sigma2(p[1],p[2]))}

q3 <- Metodo_Newton(c(7.0,0.01),fx,jx)


#Questão 4
(A4 <- cbind(c(10,2,0,0,0,0),c(1,20,3,1,0,0),c(1,0,30,0,0,1),c(0,1,0,10,2,1),c(0,0,3,0,20,1),c(0,0,3,-1,-2,10)))
(B4 <- c(10,10,0,5,5,0))
Jacobi <- function(M,erro,b,vetor_inicial=rep(1,length(M[,1]))){
  sep_LDU <- function(M){
    n <- length(M[,1])
    L=matrix(rep(0,n^2),nrow=n,ncol=n)
    D=matrix(rep(0,n^2),nrow=n,ncol=n)
    U=matrix(rep(0,n^2),nrow=n,ncol=n)
    for(i in 1:n){
      D[i,i]=M[i,i]
      for (j in 1:n) {
        if(i>j){
          L[i,j] <- M[i,j]
        }
        else{
          if(i<j){U[i,j] <- M[i,j]}
        }
      }
    }
    return(list(L=L,D=D,U=U))    
  } 
  D=sep_LDU(M)$D
  D_inv=ifelse(D>0,1/D,0)
  x=list(NULL)
  x[[1]]=rep(1,length(M[,1]))
  iter=1
  NX=1
  while(NX>erro & iter<10001){
    iter=iter+1
    x[[iter]]=-D_inv%*%(sep_LDU(M)$L+sep_LDU(M)$U)%*%x[[iter-1]]+D_inv%*%b
    NX <- norm(x[[iter]]-x[[iter-1]])
  }
  return(list(x[[iter]],paste("foram necessárias",iter-1,"iterações pelo método Jacobi")))
}
q4 <- list()
(q4$Jacobi <- (Jacobi(A4,10^-10,B4)))

Gauss_Seidel <- function(M,erro,b){
  sep_LDU <- function(M){
    n <- length(M[,1])
    L=matrix(rep(0,n^2),nrow=n,ncol=n)
    D=matrix(rep(0,n^2),nrow=n,ncol=n)
    U=matrix(rep(0,n^2),nrow=n,ncol=n)
    for(i in 1:n){
      D[i,i]=M[i,i]
      for (j in 1:n) {
        if(i>j){
          L[i,j] <- M[i,j]
        }
        else{
          if(i<j){U[i,j] <- M[i,j]}
        }
      }
    }
    return(list(L=L,D=D,U=U))    
  } 
  inverte_matriz_triang <- function(U){
    n <-length(U[1,])
    X=matrix(rep(0,n*n),ncol=n)
    for( i in n:1){
      X[i,i] <- 1/U[i,i]
      if(i<n) {
        for (j in (i+1):n) {
          X[i,j] <- -sum(X[(i+1):j,j]*U[i,(i+1):j])/U[i,i]
        }
      }
    }
    return(X)
  }
  DL_inv <- t(inverte_matriz_triang(t(sep_LDU(M)$L+sep_LDU(M)$D)))
  x=list(NULL)
  x[[1]]=rep(1,length(M[,1]))
  iter=1
  NX=1
  while(NX>erro & iter<10001){
    iter=iter+1
    x[[iter]]=(-DL_inv)%*%sep_LDU(M)$U%*%x[[iter-1]]+DL_inv%*%b
    NX <- norm(x[[iter]]-x[[iter-1]])
  }
  print(iter-1)
  return(list(x[[iter]],paste("foram necessárias",iter-1,"iterações pelo método Gauss-Seidel")))
}
(q4$Gauss_Seidel <- Gauss_Seidel(A4,10^-10,B4))


#Questão 5
c5=(optimize(function(x){dbeta(x,6,9)},lower = 0,upper = 1))$minimum
Aceitacao_rejeicao <- function(n,fx=NA,gx=NA,ry=NA,c){
  ry <- if(is.function(ry)==F){ry <- function(y){fx(x=y)/gx(x=y)}}
  x=NULL
  j=0
  while(length(x)<n){
    j <- j+1
    u <- runif(1)
    y <- runif(1)
    if(u<ry(y)/c){x=c(x,y)}
  }
  return(list(Amostra=x,Taxas=paste0('Taxa de aceitação teorica: ', 1/c), paste0('Taxa de aceitação prática: ', n/j)))
}
(q5 <- Aceitacao_rejeicao(1000,fx=function(x){dbeta(x,6,9)},gx=function(x){dunif(x,0,1)},c=c5))


