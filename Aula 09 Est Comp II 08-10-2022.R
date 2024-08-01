#Escreva um algoritmo para gerar n = 200 valores de uma variável aleatória X com função de probabilidade Binomial Negativa(5,1/3), onde X = nº de tentativas até ocorrer oquintosucesso,apartirdeumaamostraaleatóriadaU(0,1). 
set.seed(1310)
n=200
p=1/3
r=5
x=NULL
for(i in 1:n){
x[i]=sum(trunc(log(1-runif(r))/(log(1-p)))+1)
}
hist(x)

#Obtenha uma amostra aleatória de tamanho n=100 de uma variável aleatória com a distribuição discreta a seguir usando a Uniforme Discreta 1:10, pelo método de aceitação/rejeição: 
set.seed(1898)
n=10000
fx=cbind(1:10,c(0.11,0.12,0.09,0.08,0.12,0.1,0.09,0.09,0.10,0.1))
#gx=(trunc(10*runif(10))+1)
c=max(fx[,2]/rep(0.1,10))
x=NULL
i=1
j=0
while(i<=n) {
  j=j+1
  u=runif(1)
  gx=(trunc(10*runif(1))+1)
  if(u<=fx[gx,2]/(c*0.1)){
    x[i]=gx
    i=i+1
  }
}
hist(x,freq=F)
i/j
var(x)

#Considere uma amostra aleatória U1,...,Uk da U(0,1). Para k suficientemente grande, peloTCL
set.seed(1898)
k=1000
n=200
x=NULL
for (i in 1:n) {
  x[i]=(mean(runif(k))-0.5)/12*k
}
hist(x,freq=F)

