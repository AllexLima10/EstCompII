#Gere 3 valores independentes da variável aleatória X cuja densidade é dada por F(X)=0.5(2-x) 0<x<2 
set.seed(7015)
u=runif(30)
f=2-2*sqrt(1-u)
hist(f)

#Com o método da aula passada
x0=7015
a=-0.5
c=1
M=30
x[1]=(a*x0+c)%%M
i=0
n=30
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
(u2=x/M)
(f2=(2-2*sqrt(1-u2)))
hist(f2)

#Escreva o algoritmo para gerar valores aleatórios para X tal que𝑋~𝑈(10,20)
set.seed(1234)
u3=runif(1000)
(f3=10*u3+10)
hist(f3,freq=F)
x=f3
curve(dunif(x,10,20),add=T,col=2)

# Gere uma amostra aleatória de tamanho 10000 pela transformação inversa, usando semente 1234 , daExp(0,25). Compare o histograma com a densidade desta distribuição e compare a média,mediana e os quartis da amostra com os da distribuição teórica. 
set.seed(1234)
u4=runif(10000)
(f4=-4*log(1-u4))
hist(f4)
x=f4
hist(x,freq = F, xlim = c(0,30))
print(c('Media Empírica: ', mean(x)))
curve(dexp(x,0.25),add=T,col=2)

#Gerar 3 valores da N(0,1) (slide 26 da unid. 1). 
set.seed(1234)
(u5=runif(3))
t=sqrt(log(1/u5^2))
c0=2.515517
c1=0.802853
c2=0.010328
d1=1.432788
d2=0.189269
d3=0.001308
(f5=-(t- ( (c0+c1*t+c2*t^2) / (1+d1*t+d2*t^2+d3*t^3) ) ))
x=f5
hist(x)
print(c('Media Empírica: ', mean(x)))
