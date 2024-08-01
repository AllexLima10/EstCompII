#Código necessário
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
  return(x[[iter]])
}
#Resolver o sistema do Mat. 2 - Slide 37  usandoométododeGauss-Seidel
coef=cbind(c(2,-1,0),c(-1,2,-1),c(0,-1,2))
(x_1 <- Gauss_Seidel(coef,10^-11,c(0,2,1)))
round(coef%*%x_1,2)

#Resolver o sistema do Mat. 2 - Slide 38  usandoométododeGauss-Seidel
coef2=cbind(c(4,-2,-1),c(-1,6,1),c(-1,1,7))
(x_2 <- Gauss_Seidel(coef2,10^-13,c(3,9,-6)))
coef2%*%x_2
b=c(3,9,-6)
coef2%*%c(1,2,-1)
solve(coef2)%*%b
coef2

