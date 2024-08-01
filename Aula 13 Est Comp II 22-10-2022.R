#Resolva o sistema m*x = b onde m é uma matriz 2x2 e b é um vetor 1x2
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
m=cbind(c(5,1),c(1,3))
U=Cholesky(cbind(c(5,1),c(1,3)))
a=t(U)
b=t(cbind(2,1))
z=NULL
n=length(U[,1])
for (i in 1:n) {
  z[i]= ifelse(i==1,b[1]/a[1,1],(b[i]-sum(a[i,1:i-1]*z[1:i-1]))/a[i,i]) 
}
z
t(U)%*%z
x=NULL
for (j in (n:1)) {
  x[j]= ifelse(j==n,z[j]/U[j,j],(z[j]-sum(U[j,(j+1):n]*x[(j+1):n]))/U[j,j]) 
}
x
m%*%x

# forma function
resolve_sistema_Cholesky <- function(m,b){
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
U=Cholesky(m)
a=t(U)
z=NULL
n=length(U[,1])
for (i in 1:n) {
  z[i]= ifelse(i==1,b[1]/a[1,1],(b[i]-sum(a[i,1:i-1]*z[1:i-1]))/a[i,i]) 
}
x=NULL
for (j in (n:1)) {
  x[j]= ifelse(j==n,z[j]/U[j,j],(z[j]-sum(U[j,(j+1):n]*x[(j+1):n]))/U[j,j]) 
}
return(x)
}

#Faça agora com uma outra matriz sendo m uma 3x3 e b um vetor de tamanho 3
resolve_sistema_Cholesky(cbind(c(2,-1,0),c(-1,2,-1),c(0,-1,2)),c(0,2,1))
