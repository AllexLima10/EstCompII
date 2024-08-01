##Questão 1
x=NULL
x0=1111
a=1373
c=16807
M=128
x[1]=(a*x0+c)%%M
n=10
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
u1 <- x/M
gera_poisson <- function(lam,u){
  x=NULL
  p=0
  n=length(u)
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
gera_poisson(2,u1)



#forma function
LCG <- function(x0,a,c,M,n=10){
  x=NULL
  x[1]=(a*x0+c)%%M
  for (i in 2:n) {
    x[i]=(a*x[i-1]+c)%%M
  }
  return(x/M)
}

LCG_poisson <- function(x0,a,c,M,n,lam){
LCG <- function(x0,a,c,M,n){
x=NULL
x[1]=(a*x0+c)%%M
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
return(x/M)
}
gera_poisson <- function(n,lam,u){
  x=NULL
  p=0
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
variaveis <- gera_poisson(10,2,LCG(1111,1373,16807,128,10))
return(variaveis)
}

(valores <- LCG_poisson(1111,1373,16807,128,10,2))

##Questão 2
xbin=NULL
for(i in 1:10000){
xbin[i] <- sum(ifelse(runif(6)<(1-0.28),0,1))
}
mean(xbin)
var(xbin)

##Questão 3
p3=NULL
p3[1:4] <- 1:4/10
n=10000
u=runif(n)
x3=NULL
(x3[1:n] <- ifelse(u<sum(p3[1:1]),1,ifelse(u<sum(p3[1:2]),2,ifelse(u<sum(p3[1:3]),3,ifelse(u<sum(p3[1:4]),4,0)))))

##Questão 4
u4=runif(100)
x4 <- (-log(1-u4)/8)^(1/3)

##Questão 5
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
x5 <- NULL
for(i in 1:200){
  x5[i] <- gera_2_normais(1)[,1]/sqrt(sum(c(gera_2_normais(4))^2)/8)
}
hist(x5, freq=F)
curve(dt(x,8), col=2, add=T)

#forma function (definindo 1 como o padrão para o tamanho e os graus de liberdade caso não seja(m) declarado(s))
gera_t <- function(n=1,gl=1){
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
  variaveis <- NULL
  for(i in 1:n){
    variaveis[i] <- gera_2_normais(1)[,1]/sqrt(sum(gera_2_normais(gl)[,1]^2)/gl)
  }
  return(variaveis)
}
x5_1 <- gera_t(200,8)
hist(x5_1,freq=F)
curve(dt(x,8),col=2,add=T)

##Questão 6
u6 <- runif(30)
x6 <- (u6-mean(u6))/sqrt(var(u6))
hist(x6)

##Questão 7
gera_exp <- function(n=1,lambda=1){
  u=runif(n)
  x <- -log(1-u)/lambda
  return(x)
}

#a)
gera_chisq_por_exp <- function(n=1,gl=1){
  gera_exp <- function(n=1,lambda=1){
    u=runif(n)
    x <- -log(1-u)/lambda
    return(x)
  }
  variaveis <- NULL
  for (i in 1:n) {
    variaveis[i] <- sum(gera_exp(gl/2,1/2))
    }
  return(variaveis)
}
x7_a <- gera_chisq_por_exp(100,12)
hist(x7_a,freq=F)
curve(dchisq(x,12),col=2,add=T)

#b) (assumindo que 0,45 seja o parametro β da Gama)
gera_gama <- function(n=1,alfa=1,beta=1/alfa){
  gera_exp <- function(n=1,lambda=1){
    u=runif(n)
    x <- -log(1-u)/lambda
    return(x)
  }
  variaveis=NULL
  for (i in 1:n) {
    variaveis[i] <- sum(gera_exp(alfa,beta))
  }
  return(variaveis)
}
(x7_b <- gera_gama(100,8,0.45))
hist(x7_b,freq=F)
curve(dgamma(x,8,0.45),col=2,add=T)

#c)
gera_beta <- function(n,a,b,lambda=1){
  gera_gama <- function(n=1,alfa=1,beta=1/alfa){
    gera_exp <- function(n=1,lambda=1){
      u=runif(n)
      x <- -log(1-u)/lambda
      return(x)
    }
    variaveis=NULL
    for (i in 1:n) {
      variaveis[i] <- sum(gera_exp(alfa,beta))
    }
    return(variaveis)
  }
y=gera_gama(n,a,lambda)
x=gera_gama(n,b,lambda)
for (i in 1:n) {
  variaveis <- y[i]/(y[i]+x[i])
  }
variaveis <- y/(y+x)
return(variaveis)
}
x7_c <- gera_beta(100,8,6)
hist(x7_c,freq=F)
curve(dbeta(x,8,6),col=2,add=T)

#d)
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
x7_d <- gera_poisson_por_exp(100,3)
hist(x7_d,freq=F)
points(c(0:max(x)),dpois(0:max(x),3),verticals=F,col=2,lwd=3,lty=3,add=T,pch=19)
mean(x7_d)
var(x7_d)
plot(ecdf(x7_d),xlim=c(0,max(x7_d)),ylim=c(0,1))
plot(stepfun(c(0:max(x)),c(0,ppois(0:max(x),3))),verticals=F,col=2,lwd=3,lty=3,add=T,pch=19)

##Questão 8
gera_cauchy_por_norm <- function(n=1,alfa=0,beta=1){
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
  normais <- gera_2_normais(n)
  variaveis=beta*(normais[,1]/normais[,2])+alfa
  return(variaveis)
}
x8=NULL
i=0
c=pi/sqrt(2*pi)
while(length(x8)<10000){
i=i+1
y <- gera_cauchy_por_norm(n=1)
ry=(1/sqrt(2*pi))*exp(-(y^2)/2)*(pi*(1+y^2))
if(runif(1)<ry/c){x8=c(x8,y)}
}
x8
length(x8)/i
1/c

##Questão 9
gera_half_normal <- function(n=1,sigma=1){
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
  variaveis <- abs(gera_2_normais(n)[,1])*sqrt(sigma)
  return(variaveis)
}
gera_exp <- function(n=1,lambda=1){
  u=runif(n)
  x <- -log(1-u)/lambda
  return(x)
}
i=0
x9=NULL
while(length(x9)<10000){
i=i+1
y=gera_exp(1)
c=(sqrt(2)/sqrt(pi))*exp(-1^2/2)/(exp(-1))
ry=(sqrt(2)/sqrt(pi))*exp(-y^2/2)/(exp(-y))
if(runif(1)<ry/c){x9 <- c(x9,y)}
}
hist(x9)
length(x9)/i
1/c

##Questão 10 (Tava errado mas acho que consegui ajustar a questão, CUIDADO)
gera_gama<- function(n=1,alfa=1,beta=1/alfa){
  gera_exp <- function(n=1,lambda=1){
    u=runif(n)
    x <- -log(1-u)/lambda
    return(x)
  }
  variaveis=NULL
  for (i in 1:n) {
    variaveis[i] <- sum(gera_exp(alfa,beta))
  }
  return(variaveis)
}
x10=NULL
i=0
c=((1/0.00617284)*(1/2)^3*exp(-9*(1/2)^2))/(16*(1/2)*exp(-4*(1/2))) #0.00617284 é o valor que se obtêm integrando a função f(x) em seu intervalo. Para transformar a mesma em uma fdp adotei o alfa da questão como o inverso desse valor, além disso 1/2 é o valor em que se maximiza a razão da mesma com a f.d.p. da Gama
while(length(x10)<100){
  i=i+1
  y <- gera_gama(1,2,4)
  ry= ((1/0.00617284)*y^3*exp(-9*y^2))/(16*y*exp(-4*y))
  if(runif(1)<ry/c){x10=c(x10,y)}
}
x10
hist(x10,freq=F)
length(x10)/i
1/c
