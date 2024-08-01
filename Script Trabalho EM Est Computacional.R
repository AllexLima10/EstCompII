#Inserção dos dados
x <- c(12,12,1,3,3,5,2,0,5,1,0,3,13,13,1,0,4)
y_cens <- rep(4,3)
#Passos do Algoritmo EM
teta=NULL
n <- c(length(x),length(y_cens))
#Q <- mean(sum(n)*log(teta)-teta*(sum(c(x,y))))
Ey <- rep(,3) #os valores esperado de cada yj sao iguais
Q <- (n[1]+n[2])*log(teta)-teta*(sum(x)+sum(Ey))
