#Voltando à geração de vetores normais, gere uma amostra de tamanho 3 da N([3,5],cbind(c(5,1),c(1,5)))
set.seed(2048)
M=cbind(c(5,1),c(1,5))
n=3
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
x=list(NULL)
z <- gera_2_normais(3)
for (i in 1:n) {
  x[[i]]=t(Cholesky(M))%*%z[i,]+c(3,5)  
}
x
