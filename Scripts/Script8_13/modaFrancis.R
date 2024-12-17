#Yamamoto (2020, p. 245)
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
h<-hist(dados$Zgauss,breaks=c(2.957,5.573,8.189,10.805,13.420,16.036,18.652,21.268,23.884,26.500))
m<-length(dados$Zgauss)
contagens<-h$counts
quebras<-h$breaks
n<-length(contagens)
#transformando contagens em frequencias
for (i in 1:n) {contagens[i]<-100*contagens[i]/m}
contagens
maximo<-max(contagens)
indice<-which.max(contagens)
largura<-quebras[2]-quebras[1]
if (indice==1) {d1<-0} else
{d1<-contagens[indice]-contagens[indice-1]}
if (indice==n) {d2<-0} else
d2<-contagens[indice]-contagens[indice+1]
moda<-quebras[indice]+(d1/(d1+d2))*largura
moda