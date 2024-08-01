#Gereumaamostraaleatóriadetamanho200daN(0,1)
set.seed(1898)
k=1000
n=200
x=NULL
for (i in 1:n) {
  x[i]=(mean(runif(k))-0.5)/12*k
}
hist(x,freq=F)
mean(x)
var(x)
#2° método
set.seed(1313)
n=20000
u1=runif(n/2)
u2=runif(n/2)
x1=NULL
x2=NULL
x1=sqrt(-2*log(u1))*cos(2*pi*u2)
x2=sqrt(-2*log(u1))*sin(2*pi*u2)
x=c(x1,x2)
hist(x,freq=F)
curve(dnorm(x,0,1),col=2,add=T)
mean(x)
var(x)
#Gere uma amostra aleatória de tamanho 200 da N(10,4).
set.seed(1313)
n=20000
u1=runif(n/2)
u2=runif(n/2)
x1=NULL
x2=NULL
x1=sqrt(-2*log(u1))*cos(2*pi*u2)
x2=sqrt(-2*log(u1))*sin(2*pi*u2)
x=c(x1,x2)
y=2*x+10
hist(y,freq=F)
curve(dnorm(x,10,2),col=2,add=T)
mean(y)
var(y)

#Gere uma amostra aleatória de 200 observações da distribuição t-student com 3 graus de liberdade
set.seed(1313)
n=200
ngl=3
u1=runif(n/2)
u2=runif(n/2)
x1=sqrt(-2*log(u1))*cos(2*pi*u2)
x2=sqrt(-2*log(u1))*sin(2*pi*u2)
x=c(x1,x2)
x22=NULL
for (j in 1:n) {
  x22[j]=sum((sqrt(-2*log(runif(ngl)))*cos(2*pi*runif(ngl)))^2)
  }
t=x/sqrt(x22/(ngl))
hist(t,freq=F ,ylim = c(0, 0.4))
hist(t, freq = F, xlim = c(-4, 4), ylim = c(0, 0.5))
curve(dt(x, 3), col = 2, add = T, cex = 1.5)
curve(dnorm(x, 0, 1), col = 4, add = T, cex = 1.5)
#Exemplo: Gerar uma amostra aleatória daNormal mistatal queθ = 0,7; μ =0; a =5;𝜎𝜎 0
set.seed(1313)
p=0.7
x=rep(0,n)
u=runif(n)
u1=runif(n)
u2=runif(n)
x1=5+sqrt(2)*sqrt(-2*log(u1))*cos(2*pi*u2)
x0=sqrt(-2*log(u1))*sin(2*pi*u2)
q=1-p
y=ifelse(runif(n)>q,x1,x0)
hist(y,freq=F)
fmist=function(x,tet,mi,a,sig0,sig1) {
  (1-tet)*dnorm(x,mi,sqrt(sig0))+(tet)*dnorm(x,mi+a,sqrt(sig1))
}
curve(fmist(x,0.7,0,5,1,2), add = T, col = 2) #tet=0.7; mi=0; a=5; sig0=1 e sig1=2