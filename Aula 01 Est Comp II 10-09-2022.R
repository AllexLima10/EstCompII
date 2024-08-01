#Mostre a sequência dos 5 primeiros números inteiros gerados pelo método LCG usando x0=27,a=17,c=43 e M=100.Use-a para gerar 5 números pseudo-aleatórios entre 0 e 1.
x0=27
a=17
c=43
M=100
x[1]=(a*x0+c)%%M
i=0
n=10
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
x/M

#Mostre a sequência de inteiros gerada pelo método LCG, usando x0 = 1, a = 6, c = 0 e M=11.Repita o exercício fazendo o mesmo valor para c e M,e alterando a para 3.
x=NULL
x0=1
a=6
c=0
M=11
x[1]=(a*x0+c)%%M
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
x/M

#Considere o exercício anterior com x0 =2
x=NULL
x0=2
a=6
c=0
M=11
x[1]=(a*x0+c)%%M
for (i in 2:n) {
  x[i]=(a*x[i-1]+c)%%M
}
x/M

#Encontre o período para o gerador com os seguintes parâmetros:a=13, c=0, M=26,e x0= 1, 2, 3 e 4.(Com as sementes ímpares (1 e 3), é possível a obtenção de períodos com 16 elementos (P= M/4 = 64/4 = 16). Para as sementes pares (2 e 4), os períodos obtidos têm comprimentos 8 e 4,respectivamente.)
c=0
M=2^6
a=13
x=NULL
for(j in 1:4){
  x0=j
  x[1]=(a*x0+c)%%M
  for (i in 2:n) {
    x[i]=(a*x[i-1]+c)%%M
  }
  print(x/M)
}

#forma function
LCG <- function(x0,a,c,M){
  x=NULL
  x[1]=(a*x0+c)%%M
  n=10
  for (i in 2:n) {
    x[i]=(a*x[i-1]+c)%%M
  }
  return(x/M)
}
