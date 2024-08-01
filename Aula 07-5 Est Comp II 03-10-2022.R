#Então, para gerar n valores da Poisson(λ) usando a geração de valores da distribuição Exponencial pelo método da transformação inversa
set.seed(1210)
k=0
t=0
n=200
lam=2
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
hist(x,freq=F)
points(c(0:max(x)),dpois(0:max(x),lam),verticals=F,col=2,lwd=3,lty=3,add=T,pch=19)
mean(x)
var(x)
plot(ecdf(x),xlim=c(0,max(x)),ylim=c(0,1))
plot(stepfun(c(0:max(x)),c(0,ppois(0:max(x),lam))),verticals=F,col=2,lwd=3,lty=3,add=T,pch=19)

