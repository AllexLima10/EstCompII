###VAE 1 Est. Computacional II - Állex Lima de Jesus
##Questão 1
set.seed(191122)
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
(A1 <- matrix(c(4,-1,-1,0,0,-1,4,-1,-1,0,-1,-1,4,-1,-1,0,-1,-1,4,-1,0,0,1,-1,4),5,5,byrow = T))
(b1=c(-1,2,6,2,-1))
Jacobi(A1,10^-10,b1)
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
Gauss_Seidel(A1,10^-10,b1)

##Questão 2
set.seed(191122)
gera_poisson_por_exp <- function(n,lam){
  k=0
  t=0
  x=NULL
  for (i in 1:n) {
    t=0
    k=0
    while (t<=1) {
      k=k+1
      u=runif(1)
      y=(-log(1-u))/lam
      t=t+y
    }
    x[i]=k-1
  }
  return(x)
}
(x2_a=gera_poisson_por_exp(1000,6))

gera_poisson <- function(n,lam){
x=NULL
p=0
u=runif(200)
x=rep(0,n)
i=0
m=max(u)
while(p<m){
  pv=p
  p=p+(exp(-lam)*lam^i)/factorial(i)
  x[u>pv & u<=p]=i
  i=i+1
}
return(x)
}
(x2_b=gera_poisson(1000,6))
mean(x2_a)
var(x2_a)
mean(x2_b)
var(x2_b)

##Questão 3
gera_exp <- function(n=1,lambda=1){
  u=runif(n)
  x <- -log(1-u)/lambda
  return(x)
}
curve(dgamma(x,6.2,9.3)/(6*exp(-6*x)))
#conclui-se que o maior valor é atingido quando x=1
c=dgamma(1,6.2,9.3)/(6*exp(-6*1))
i=0
x3=NULL
while(length(x3)<1000){
  i=i+1
u=runif(1)
y=gera_exp(1,6)
ry=dgamma(y,6.2,9.3)/(6*exp(-6*y))
if(u<ry/c){x3=c(x3,y)}
}
x3
paste("Taxa de Aceitação Teórica:",1/c)
paste("Taxa de Aceitação Efetiva",length(x3)/i)


##Questão 4
u=runif(100)
x=ifelse(u<(2/3),6*sqrt(u),sqrt(1-u)+sqrt(3))
paste("Média da Amostra:",mean(x))
paste("Média Teórica Encontrada:",15/9)

##Questão 5
gera_normais_por_Cholesky_Box_Miller <- function(n,mi,SIGMA){
  Cholesky <- function(A){
    n=length(A[1,])
    U=matrix(c(rep(0,(n^2))),n,n)
    for(i in 1:length(A[1,])){
      U[i,i]=sqrt(A[i,i]-sum((U[1:(i-1),i])^2))
      for(j in (i+1):length(A[,1])){
        if(i<length(A[1,])) {U[i,j]=((A[i,j]-sum(U[1:(i-1),i]*U[1:(i-1),j])))/U[i,i]}
      }
    }
    return(U)
  }
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
  variaveis=list()
  variaveis=NULL
  for(i in 1:n){
    variaveis[[i]]=t(Cholesky(SIGMA))%*%gera_2_normais(length(mi))[,1]+mi 
  }
  return(variaveis)
}
gera_normais_por_Cholesky_Box_Miller(2,c(8,15,18),cbind(c(5,-1,2),c(-1,3,-2),c(2,-2,7)))







