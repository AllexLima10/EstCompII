#Faça a decomposição de Cholesky para a matriz cbind(c(5,1),c(1,3))
install.packages("matrixcalc")
library(matrixcalc)
A=cbind(c(5,1),c(1,3))
is.positive.definite(A)
U=cbind(rep(0,2),rep(0,2))
for(i in 1:length(A[1,])){
  U[i,i]=sqrt(A[i,i]-sum((U[1:(i-1),i])^2))
  for(j in (i+1):length(A[,1])){
 if(i<length(A[1,])) {U[i,j]=((A[i,j]-sum(U[1:(i-1),i]*U[1:(i-1),j])))/U[i,i]}
    }
  }
U
t(U)%*%U
#forma function
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
t(U)%*%U
cholesky(A)
#Exemplo 2
A=cbind(c(2,-1,0),c(-1,2,-1),c(0,-1,2))
n=length(A[1,])
U=matrix(c(rep(0,(n^2))),n,n)
for(i in 1:n){
  U[i,i]=sqrt(A[i,i]-sum((U[1:(i-1),i])^2))
  for(j in (i+1):n){
    if(i<n) {U[i,j]=((A[i,j]-sum(U[1:(i-1),i]*U[1:(i-1),j])))/U[i,i]}
  }
}
U
t(U)%*%U
Cholesky(A)