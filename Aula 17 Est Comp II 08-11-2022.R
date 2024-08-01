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
###########################################################

Jacobi <- function(M,erro,b,cte_inicial=1){
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
print(iter-1)
return(x[[iter]])
      }
coef=cbind(c(2,-1,0),c(-1,2,-1),c(0,-1,2))
Jacobi(coef,10^-11,c(0,2,1),1)
coef2=cbind(c(4,-2,-1),c(-1,6,1),c(-1,1,7))
Jacobi(coef2,10^-11,c(3,9,-6),1)
