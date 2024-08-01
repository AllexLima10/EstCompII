#Escreva um algoritmo para gerar n=20 valores de uma Bernoulli(0,34) a partir de uma amostra aleatória da U(0,1
set.seed(2709)
x=NULL
n=20
u=runif(n)
p=0.34
q=1-p
ub=ifelse(u>q,1,0)
x=c(x,ub)
hist(x)
mean(x)

#Escreva um algoritmo para gerar n = 200 valores de uma variável aleatória X com função de probabilidade dada (0,1 se x=1, 0,3 se x=2, 0,35 se x=3, 0,25 se x=4) a partir de uma amostraaleatória daU(0,1) 
set.seed(2709)
x=NULL
n=200
u=runif(n)
x=c(x,ifelse(u<0.1,1,ifelse(u<0.4,2,ifelse(u<0.75,3,4))))
mean(x)
hist(x)

#Escreva um algoritmo para gerar n = 200 valores de uma Poisson(3) a partir de uma amostraaleatóriadaU(0,1). 
set.seed(2709)
x=NULL
lam=3
n=200
p=0
u=runif(200)
x=rep(0,n)
i=0
m=max(u)
while(p<m){
  pv=p
  p=p+ (exp(-lam)*lam^i)/factorial(i)
  x[u>pv & u<=p]=i
  i=i+1
}
mean(x)
hist(x)






