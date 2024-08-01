#Gere uma amostra aleatória de tamanho 200 da distribuição f(x)=x+1, -1<x<0, -x+1, 0<x<1:
n=200
set.seed(1142)
u=runif(n)
x=ifelse(u<=0.5,sqrt(2*u)-1,1-sqrt(2*(1-u)))
hist(x)
f<-  function(x){
  y=ifelse(x<=0 & x>=-1,x+1,ifelse(x>=0 & x<=1,-x+1,0))
}
return(y)
hist(x,ylim=c(0,1),freq=F)
curve(f(x),col=4,add=T)

#Gere uma amostra aleatória de tamanho 200 da distribuição f(x)=4x, 0<x<1/2, 4(1-x), 1/2<x<1:
n=200
set.seed(1142)
u=runif(n)
x=ifelse(u<=0.5,sqrt(2*u/4),1-(sqrt(2-2*u)/2))
hist(x,freq=F)
f<-  function(x){
  y=ifelse(x>=0 & x<=1/2,4*x,ifelse(x>=1/2,4*(1-x),0))
  return(y)}

hist(x,freq=F)
curve(f(x),col=4,add=T)

