#Encontre o estimador de máxima verossimilhança de r e λ da Gama(r,λ) usando a amostra aleatória a seguir: aa=c(50322.89,64917.48,30622.40,16531.30,20840.32,29858.56,39970.50,45663.37,382 37.81,54602.77,26881.53,23965.48,96881.61,37289.45,37815.77,69124.83,20521.47,46 308.03,44479.01,27328.56)
Metodo_Newton <- function(vetor_inicial,fx,jx,eps=10^-8,max_iter=1000){
  err <- eps+1
  i <- 0
  xnovo <- vetor_inicial
  while(err>eps & i<max_iter){
    i <- i+1
    xant <- xnovo
    nj <- jx(xant)
    xnovo <- xant-solve(nj)%*%fx(xant)
    err <- norm((xant-xnovo)/xant)
  }
  return(list(Vetor_Raiz=xnovo,Numero_iteracoes=i,Erro=err))  
}

aa=c(50322.89,64917.48,30622.40,16531.30,20840.32,29858.56,39970.50,45663.37,382,37.81,54602.77,26881.53,23965.48,96881.61,37289.45,37815.77,69124.83,20521.47,46,308.03,44479.01,27328.56)



EMV <- function(x){
  ssl=NULL
  ssl=208.3912
    #sum(log(x))
  ss=738391.1
    #sum((x))
  n=length(aa)
  LogVero_gama <- expression(n*r*log(gamma(r))+(r-1)*208.3912-lam*738391.1)
  dd=deriv3(LogVero_gama,c("r","lam"),func=T)
    dlogvero_gama <-function(vetor){matrix(attr(dd(vetor[1],vetor[2]),"gradient")[1:2],ncol=1)}
  hessiano <- function(vetor){matrix(attr(dd(vetor[1],vetor[2]),"hessian")[1:4],ncol=2,byrow=T)}
  Metodo_Newton <- function(vetor_inicial,fx,jx,eps=10^-8,max_iter=100){
    err <- eps+1
    i <- 0
    xnovo <- vetor_inicial
    while(err>eps & i<max_iter){
      i <- i+1
      xant <- xnovo
      nj <- jx(xant)
      print(nj)
      print(fx(xant))
      xnovo <- xant-solve(nj)%*%fx(xant)
      print(xnovo)
      err <- norm((xant-xnovo)/xant)
      print(err)
      print(i)
    }
   
    return(list(Vetor_Raiz=xnovo,Numero_iteracoes=i,Erro=err))  
  }
  return(Metodo_Newton(c(4.5,0.00001),fx=function(vetor){dlogvero_gama(vetor)},jx=hessiano))
}


EMV(aa)
?#c(mean(aa)^2/(mean(aa^2)-mean(aa)^2),mean(aa)/(mean(aa^2)-mean(aa)^2))
sum(aa)
sum(log(aa))
################################################################################################################

aa=c(50322.89,64917.48,30622.40,16531.30,20840.32,29858.56,39970.50,45663.37,38237.81,
     54602.77,26881.53,23965.48,96881.61,37289.45,37815.77,69124.83,20521.47,46308.03,
     44479.01,27328.56)



nn=length(aa)
ss=sum(aa)
ssl=sum(log(aa))
nn
ss
ssl



r1=mean(aa)^2/((sum(aa^2)/length(aa))-mean(aa)^2)
lam1=mean(aa)/((sum(aa^2)/length(aa))-mean(aa)^2)
tetah=c(r1,lam1)
tetah



eplv=expression(nn*r*log(lam)-nn*log(gamma(r))+(r-1)*ssl-lam*ss)
dd=deriv3(eplv,c("r","lam"),func=TRUE)
pnovo = tetah
np=length(pnovo)
eps=10^-8
err=1000
i=0



while (err>eps&i<=1000){
  i=i+1
  pant=pnovo
  hh=matrix(attr(dd(pant[1],pant[2]),"hessian")[1:I(np*np)],ncol=np,byrow=T)
  jj=matrix(attr(dd(pant[1],pant[2]),"gradient")[1:np],ncol=1)
  print(jj)
  ih=solve(hh)
  print(ih)
  pnovo=pant-ih%*%(jj)
  err=norm(((pant-pnovo)/pant))
  print(pnovo)
}
print(i)
#######################################################################################################

#EMV da Gama(r,lam)

tt=c(50322.89,64917.48,30622.40,
     
     16531.30,20840.32,29858.56,
     
     39970.50,45663.37,38237.81,
     
     54602.77,26881.53,23965.48,
     
     96881.61,37289.45,37815.77,
     
     69124.83,20521.47,46308.03,
     
     44479.01,27328.56)

nn=length(tt)

ss=sum(tt)

ssl=sum(log(tt))

nn

ss

ssl

r1=mean(tt)^2/((sum(tt^2)/length(tt))-mean(tt)^2)

lam1=mean(tt)/((sum(tt^2)/length(tt))-mean(tt)^2)

tetah=c(r1,lam1)

tetah



eplv=expression(nna*r*log(lam)-nna*log(gamma(r))+(r-1)*ssl-lam*ss)

dd=deriv3(eplv,c("r","lam"),func=TRUE)

pnovo=tetah

np=length(pnovo)

eps=10^(-8)

err=1000

i=0



while (err>eps&i<=1000){
  
  i=i+1
  
  pant=pnovo
  
  hh=matrix(attr(dd(pant[1],pant[2]),"hessian")[1:I(np*np)],ncol=np,byrow=T)
  
  jj=matrix(attr(dd(pant[1],pant[2]),"gradient")[1:np],ncol=1)
  
  print(jj)
  
  ih=solve(hh)
  
  print(ih)
  
  pnovo=pant-ih%*%(jj)
  
  err=norm(((pant-pnovo)/pant))
  
  print(pnovo)
  
}

print(i)

print(pnovo)

r=pnovo[1]

lam=pnovo[2]

print(eval(eplv))

hh=matrix(attr(dd(r,lam),"hessian")[1:I(np*np)],ncol=np,byrow=T)

jj=matrix(attr(dd(r,lam),"gradient")[1:np],ncol=1)

ih=solve(hh)

print(jj)

print(hh)

print(ih)

#################################################################################

EMV <- function(x,teta,tetah,eplv,eps=10^(-8),err=1000){
  n <- length(x)
  ss <- sum(x)
  ssl <- sum(log(x))
  dd=deriv3(eplv,teta,func=TRUE)
  pnovo=tetah
  np=length(pnovo)
  i=0
  while (err>eps&i<=1000){
    i=i+1
    pant=pnovo
    hh=matrix(attr(dd(pant[1],pant[2]),"hessian")[1:I(np*np)],ncol=np,byrow=T)
    jj=matrix(attr(dd(pant[1],pant[2]),"gradient")[1:np],ncol=1)
    print(jj)
    ih=solve(hh)
    print(ih)
    pnovo=pant-ih%*%(jj)
    err=norm(((pant-pnovo)/pant))
    print(pnovo)
  }
  return(list(c(Numero_iteracoes=i,Estimadores=pnovo)))
}
n=length(tt)
r1=mean(tt)^2/((sum(tt^2)/length(tt))-mean(tt)^2)
lam1=mean(tt)/((sum(tt^2)/length(tt))-mean(tt)^2)
nna=length(tt)
ssl=sum(log(tt))
ss=sum(tt)
EMV(tt,c("r","lam"),c(r1,lam1),expression(n*r*log(lam)-n*log(gamma(r))+(r-1)*ssl-lam*ss))
