x=NULL
X=c(x[1]+x[2]^2-3,2+(x[1]/x[2]))
fx <- function(x){c(x[1]+x[2]^2-3,2+(x[1]/x[2]))}
jx <- function(x){matrix(c(1,2*x[2],(1/x[2]),(-x[1]/x[2]^2)),byrow = T,ncol=2)}

Metodo_Newton <- function(vetor_inicial,fx,jx,eps=10^-8,max_iter=1000){
err <- eps+1
i <- 0
xnovo <- vetor_inicial
  while(err>eps & i<max_iter){
   i <- i+1
   xant <- xnovo
   nj <- jx(xant)
   xnovo <- xant-solve(nj)%*%fx(xant)
   err <- norm((xant-xnovo)/xant)
  }
return(list(Vetor_Raiz=xnovo,Numero_iteracoes=i,Erro=err))  
}

#(Mat. 4 - Slide 10) Encontre a raiz da função vetorial: matrix(c(exp(x[1])-2,5*x[3]-4,4*x[1]*x[2]-2*x[3]-6),nrow=3)
fx1=function(x){matrix(c(exp(x[1])-2,5*x[3]-4,4*x[1]*x[2]-2*x[3]-6),nrow=3)}
jx1=function(x){matrix(c(exp(x[1]),0,0,0,0,5,4*x[2],4*x[1],-2), ncol=3, byrow = T)}
Metodo_Newton(fx=fx1,jx=jx1,vetor_inicial = c(1,1,1))
