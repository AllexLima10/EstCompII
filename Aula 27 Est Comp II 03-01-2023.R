#Em um artigo, Rao usou dados de 197 animais distribuídos em 4 categorias, tal que os dados observados consistem nos resultados de uma variável multinomial.
#Os valores observados foram xt= 125,18,20,34
#Um modelo genético para a população especifica as probabilidades de cada célula
eps=10^-8
err=eps+1
teta=1
teta_antigo=teta
while(err<eps){
  teta_antigo=teta
  teta=(125*teta/(teta+2)+34)/(125*teta/(teta+2)+72)
  err=abs(teta-teta_antigo)
}
teta