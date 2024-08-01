##Questão 1
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
n=3
p=3
x1=list()
SIGMA <- cbind(c(15,-4,0),c(-4,17,-7),c(0,-7,18))
mi <- c(1,4,6)
variaveis=NULL
for(i in 1:n){
  x1[[i]]=t(Cholesky(SIGMA))%*%gera_2_normais(3)[,1]+mi 
}
x1

#forma function
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
gera_normais_por_Cholesky_Box_Miller(3,c(1,4,6),cbind(c(15,-4,0),c(-4,17,-7),c(0,-7,18)))

##Questão 2
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
A <- matrix(c(4,12,-16,12,37,-43,-16,-43,98),3,3)
b <- c(0,2,1)
(x2 <- resolve_sistema_Cholesky(A,b))

##Questão 3
(x3 <- gera_normais_por_Cholesky_Box_Miller(3,c(1,-1,3),matrix(c(2,-1,1,-1,2,-1,1,-1,2),3,3)))

##Questão 4
resolve_sistema_LU <- function(m,b){
  n <- length(b)
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
resolve_sistema_LU(matrix(c(1,3,1,5,6,19,4,33,2,4,8,9,4,15,12,3),4,4),c(8,25,18,72))[[2]]

##Questão 5
inverte_por_LU <- function(matriz){
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
U_inv <- inverte_matriz_triang(LU(matriz)[[2]])
L_inv <- t(inverte_matriz_triang(t(LU(matriz)[[1]])))
LU_inv <- U_inv %*% L_inv
return(LU_inv)
}
A5=matrix(c(2,4,-6,4,3,7,-10,6,0,2,0,4,0,0,1,5),4,4)
b5=c(1,2,1,0)
A5_inv=inverte_por_LU(A5)
(x5 <- resolve_sistema_LU(A5,b5)[[2]])

##Questão 6
A6=matrix(c(4,-1,-1,-2,6,1,-1,1,7),3,3)
b6 <- c(3,9,-6)
(x6 <- resolve_sistema_LU(A6,b6)[[2]])

#Questão 7
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
a7=matrix(c(6,2,1,-1,2,4,1,0,1,1,4,-1,-1,0,-1,2),4,4)
b7=c(11,16,20,5)
(x7=Jacobi(a7,10^-14,b7))

##Questão 8
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
a8=a7
b8=b7
Gauss_Seidel(a8,10^-14,b8)

