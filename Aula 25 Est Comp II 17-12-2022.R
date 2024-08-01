#EncontreoestimadordemáximaverossimilhançadebdaCauchy(0,b)usandoa amostraaleatóriaaseguir:
aa=c(-4.357190,48.154301,-1.416055,17.669024,16.156122,-2.718964,4.347039,1.997200,-8.244982,3.239288,3.547857,-2.037914,121.505840,13.558586,13.014515,-1.868535,-15.167708,-1.068031,-6.761203,-10.574216)
EMV <- function(x,teta,tetah,eps=10^(-8),err=1000){
  n <- length(x)
  ss <- sum(x)
  pnovo=tetah
  np=length(pnovo)
  i=0
  derivada <- function(x,b){(n/b)-sum((2*b)/(b^2+x^2))}
  derivada2 <- function(x,b){(-n/(b^2))-sum(2/(b^2+x^2))-sum((4*b^2)/(b^2+x^2)^2)}
  err=eps+1

  while (err > eps & i<=1000){
    i=i+1
    pant=pnovo
    hh=derivada2(aa,pant)
    ih=solve(hh)
    print(ih)
    pnovo=as.numeric(pant-ih%*%(derivada(x,pant)))
    err=abs(((pant-pnovo)/pant))
    print(pnovo)
  }
  return(list(c(Numero_iteracoes=i,Estimadores=pnovo)))
}
n=length(aa)
np=1
b1=(fivenum(aa)[4]-fivenum(aa)[2])/2
soma_aa2=sum(aa^2)
EMV(aa,"b",b1)

derivada(aa,b)
x=aa

summary(aa)
