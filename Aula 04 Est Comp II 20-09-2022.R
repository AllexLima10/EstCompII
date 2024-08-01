#Obtenha uma amostra aleatória de tamanho n=100 de uma variável aleatória Beta de parâmetros α=2 e β=1,usando a U(0,1), pelo método de aceitação/rejeição
set.seed(1322)
x=NULL
c=2
i=0
n=100
while(length(x)<n){
i=i+1
u=runif(1)
y=runif(1)
ry=(2*y) #ry aqui é a razão entre a função desejada e a função de aceitação (fx(y)/gy(y))
if(u<ry/c){
  x=c(x,y)
}
}
print(c('Taxa de aceitação teorica', 1/c))
print(c('Taxa de aceitação prática', n/i))
hist(x,freq=F)
curve(dbeta(x,2,1), col=2, add=T)
