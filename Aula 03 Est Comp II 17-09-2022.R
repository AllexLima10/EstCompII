#Distribuição de Erlang – A distribuição Erlang é uma distribuição Gama(n,λ), onde n é um número inteiro positivo. Sabe-se que, se Y1,...,Yn  uma amostra aleatória da Exp(lambda) então sua soma tem dist. Gama(n,lambda)
#Gere uma amostra de tamanho 100 da Gama(7,0,25),onde E(X) = 7/0.25 = 28
set.seed(1234)
lam=0.25
n=7
m=100
s1=NULL
for (i in 1:m){
  u1=runif(n)
  (f1=-(1/lam)*log(1-u1))
  (s1[i]=sum(f1))
 }
x=s1
hist(x,freq=F)
curve(dgamma(x,n,lam),col=2,add=T)
m/lam
mean(x)

#Gere uma amostra da Beta(8,6)
set.seed(1234)
n1=8
n2=6
lam=1
b1=lam
b2=lam
m=100
s1=NULL
s2=NULL
B=NULL
for (i in 1:m){
  u1=runif(n1)
  u2=runif(n2)
  (f1=-(1/b1)*log(1-u1))
  (f2=-(1/b2)*log(1-u2))
  (s1[i]=sum(f1))
  (s2[i]=sum(f2))
  B[i]=s1[i]/(s1[i]+s2[i])
}
x=B
hist(x,freq = F)
curve(dbeta(x,n1,n2),col=2,add=T)
mean(x)
n1/(n1+n2)
