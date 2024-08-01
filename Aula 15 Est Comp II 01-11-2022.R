#Faça a decomposição LU para 2 −1 1 −1 2 −1 1 −1 2 e usar essa decomposição pararesolver osistema
LU <- function(A){
  i=0
  j=0
  n=length(A[1,])
  L=matrix(rep(0,n^2),n,n)
  U=diag(n)
  for(i in 1:n){
    for (j in i:n) {
    L[j,i] = A[j,i] -sum(L[j,1:I(i-1)]*U[1:I(i-1),i])
    }
    if(i<n){
    for (j in (i+1):n) {
     U[i,j]=(A[i,j]-sum(L[i,1:I(i-1)]*U[1:I(i-1),j]))/L[i,i] 
    }
    }  
  }
  return(list(L,U))
}
b=cbind(c(2,-1,1),c(-1,2,-1),c(1,-1,2))
LU(b)
LU(b)[[1]]%*%LU(b)[[2]]
solve(LU(b)[[1]]%*%LU(b)[[2]],c(0,2,1))
#forma function
resolve_sistema_LU <- function(m,b){
  LU <- function(A){
    i=0
    j=0
    n=length(A[1,])
    L=matrix(rep(0,n^2),n,n)
    U=diag(n)
    for(i in 1:n){
      for (j in 1:n) {
        L[j,i] = A[j,i] -sum(L[j,1:I(i-1)]*U[1:I(i-1),i])
      }
      if(i<n){
        for (j in (i+1):n) {
          U[i,j]=(A[i,j]-sum(L[i,1:I(i-1)]*U[1:I(i-1),j]))/L[i,i] 
        }
      }  
    }
    return(list(L,U))
  }  
U=LU(m)[[2]]
L=LU(m)[[1]]
z=NULL
for (i in 1:n) {
  z[i]= ifelse(i==1,b[1]/L[1,1],(b[i]-sum(L[i,1:i-1]*z[1:i-1]))/L[i,i]) 
}
x=NULL
for (j in (n:1)) {
  x[j]= ifelse(j==n,z[j]/U[j,j],(z[j]-sum(U[j,(j+1):n]*x[(j+1):n]))/U[j,j]) 
}
return(list(z,x))
}
m=cbind(c(2,-1,1),c(-1,2,-1),c(1,-1,2))
resolve_sistema_LU(m,c(0,2,1))
m%*%resolve_sistema_LU(m,c(0,2,1))[[2]]
