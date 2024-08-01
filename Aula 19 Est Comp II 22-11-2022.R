#Calcular o valor esperado da Qui-Quadrado(k), onde k=2g.l.:
integral_trapezio <- function(f,a,b,pedacos){
x=a+((0:(pedacos))*((b-a)/(pedacos)))
#x=seq(a,b,l=I(pedacos+1))
print(c(x,length(x)))
inttrap=0
for(i in 1:(pedacos)){
  inttrap=inttrap+((f(x[i])+f(x[i+1]))/2)*(x[i+1]-x[i])
  }
return(inttrap)
}
g <- function(x){(x*exp(-x/2))/2}
integral_trapezio(g,0,20,100)

integral_Simpson <- function(f,a,b,pedacos){
  x=a+((0:(pedacos))*((b-a)/(pedacos)))
  #x=seq(a,b,l=I(pedacos+1))
  print(c(x,length(x)))
  intsimp=0
  for(i in 1:(pedacos)){
    intsimp=intsimp+((x[i+1]-x[i])/6)*(f(x[i])+4*f((x[i]+x[i+1])/2)+f(x[i+1]))
  }
  return(intsimp)
}
g <- function(x){(x*exp(-x/2))/2}
integral_Simpson(g,0,20,100)
