#Gere uma amostra aleatória da distribuição Normal truncada no 0, conhecida como Half-normal, considerando como proposta a N(0,1)
set.seed(1322)
x=NULL
c=2
i=0
n=100
while(length(x)<n){
  i=i+1
  (un=runif(1))
  t=sqrt(log(1/un^2))
  c0=2.515517
  c1=0.802853
  c2=0.010328
  d1=1.432788
  d2=0.189269
  d3=0.001308
  (y=-(t- ( (c0+c1*t+c2*t^2) / (1+d1*t+d2*t^2+d3*t^3) ) )) # gerando variáveis com dist. normal
  hist(y,freq = F)
  ry=ifelse(y>0,2,0) # Aplicando a razão fx(y)/gy(y)
  u=runif(1)
  if(u<ry/c){x=c(x,y)}
}
print(c('Taxa de aceitação teorica', 1/c))
print(c('Taxa de aceitação prática', n/i))
hist(x,freq=F)


install.packages("fdrtool")              # Para gerar a linha da curva, esse pacote é necessário
library("fdrtool")
curve(dhalfnorm,col=2,add=T)

#Gere valores de uma amostra aleatória de n = 100 da Normal Padrão com o método da aceitação/rejeição usandoaU[-10,10
set.seed(1322)
x=NULL
c=(1/sqrt(2*pi))/(1/20) #O maior valor possível para a imagem da função normal em um ponto é 1/raiz de 2*pi (x=0) enquanto no domínio [-10,10] a função é uma constante igual a 1/20
i=0
n=200
while(length(x)<n){
  u=runif(1)
  i=i+1
  y=runif(1)*20-10
  fy=(1/sqrt(2*pi))*exp(-1/2*(y^2))
  gy=ifelse(y>-10 & y<10,1/20,0)
  ry=fy/gy
  if(u<ry/c){x=c(x,y)}
}
print(c('Taxa de aceitação teorica', 1/c))
print(c('Taxa de aceitação prática', n/i))
hist(x,freq=F)
curve(dnorm,col=2,add = T)