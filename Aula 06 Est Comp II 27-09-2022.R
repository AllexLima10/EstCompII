#Escreva um algoritmo para gerar n = 200 valores de uma variável aleatória X com função de probabilidade Geométrica(1/3), onde X = nº de tentativas até ocorrer o primeirosucesso,apartirdeumaamostraaleatóriadaU(0,1)
set.seed(2709)
n=200
yu=runif(200)
p=1/3
y=trunc((log(1-yu))/(log(1-p)))+1
hist(y)
mean(y)
plot(ecdf(y))
plot(stepfun(c(1:max(y)),c(0,pgeom(c(0:max(y-1)),p))),verticals=F,col=2,lwd=3,lty=3,add=T,pch=19)

#Escreva um algoritmo para gerar n = 200 valores de uma variável aleatória X com função de probabilidade Uniforme discreta (de 1 a 6) a partir de uma amostra aleatória da U(0,1).
set.seed(2709)
n=200
u=runif(n)
ud=trunc(6*u)+1
hist(ud)
mean(ud)
