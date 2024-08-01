#Para a resolução dos Exercícios foi necessária a função LU para decompor as matrizes em triangulares
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

#Algoritmo:Inversão de matriz triangular superior:XU=I 
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

#Encontre a inversa de cbind(c(2 −1 1), (-1,2,-1), c(1,-1,2)) usando a decomposição LU
matriz_1 <- cbind(c(2,-1, 1), c(-1,2,-1), c(1,-1,2))
LU_m1 <- LU(matriz_1)
(matriz1_inv <-inverte_matriz_triang(LU_m1[[2]])%*%t(inverte_matriz_triang(t(LU_m1[[1]])))) 
matriz1_inv%*%matriz_1 #verificando resultado


#Encontre a inversa de A usando a decomposição LU
matriz2 <- cbind(c(4,12,-16),c(12,37,-43),c(-16,-43,98))
LU_matriz2 <- LU(matriz2)
(matriz2_inv <-inverte_matriz_triang(LU_matriz2[[2]])%*%t(inverte_matriz_triang(t(LU_matriz2[[1]])))) 
solve(matriz2) #outra maneira de verificar o resultado
round(matriz2_inv%*%A,2)
