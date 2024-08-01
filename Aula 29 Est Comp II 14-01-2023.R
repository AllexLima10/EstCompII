#Baixando pacote relacionado ao bootstrap para obter os dados
install.packages("bootstrap")
library("bootstrap")
data(law)
dados <- law
n <- nrow(law)
c <- ncol(law)
set.seed(140123)
b=0
B=200
thetab=NULL
theta_hat=cor(law$LSAT,law$GPA)
for(b in 1:B){
  xj=dados[sample(1:n,size=n,replace=TRUE),]
  thetab[b] <- cor(xj[,1],xj[,2])
}
biascboot=mean(thetab)-theta_hat
sdboot=sqrt(var(thetab))
alfa=0.05

quantile((thetab),c(alfa/2,1-(alfa/2)),type=6)
mean(thetab)

library(boot)
dados=law
theta1=function(x,i){
  cor(x[i,1],x[i,2])
}
tss=boot(dados,theta1,stype="i",R=200)
mean(tss$t)
sd(tss$t)
quantile(tss$t,c(alfa/2,1-(alfa/2)),type=6)
boot.ci(tss,conf=0.95,type="perc")


#Em uma amostra de 15 turmas de uma escola de direito duas medidas foram consideradas: LSAT, o escore médiodaturma no exame nacional deadmissão ao curso, e GPA, a nota média do curso de graduação. Os dados law da biblioteca bootstrap de Efron e Tibishirani contém estes dados . O parâmetro de interesse é o coeficiente de correlação ρ. A estimativa para esse coeficiente é o coeficiente de correlação amostral 0,776. Use 200 amostras bootstrap para estimar o erro padrão associado a essa estimativa.
escore_exame=law$LSAT
escore_grad=law$GPA
escores <- law
ro_chapeu <- cor(escore_exame,escore_grad)
rob=NULL
set.seed(140123)
for(i in 1:200){
  xj=escores[sample(1:nrow(escores),size=nrow(escores),replace = T),]
  rob[i] <- cor(xj[,1],xj[,2])
}
Vies_escores_boot=mean(rob)-ro_chapeu
desvio_escores_boot <- sqrt(var(rob))
rob
alfa=0.05
desvio_escores_boot

#forma function
Bootstrap <- function(dado1,dado2,tamanho){
  theta_chapeu <- cor(dado1,dado2)
  dados=cbind(dado1,dado2)
  thetab=NULL
for(i in 1:tamanho){
  xj=escores[sample(1:nrow(dados),size=nrow(dados),replace = T),]
  thetab[i] <- cor(xj[,1],xj[,2])
}
  Vies_theta_boot=mean(thetab)-theta_chapeu
  desvio_theta_boot <- sqrt(var(thetab))
  return(list(Estimativa=thetab,Vies=Vies_theta_boot,desvio=desvio_theta_boot))
}
Bootstrap(law$LSAT,law$GPA,200)
