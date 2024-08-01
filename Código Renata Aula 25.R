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



eplv=expression(nn*r*log(lam)-nn*log(gamma(r))+(r-1)*ssl-lam*ss)

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