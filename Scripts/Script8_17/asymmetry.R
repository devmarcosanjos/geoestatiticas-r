#Yamamoto (2020, p. 247)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
n<-length(dados$Zgauss)
media<-mean(dados$Zgauss)
desvio<-0; soma3<-0; soma4<-0
for (i in 1:n){
  soma3<-soma3+(dados$Zgauss[i]-media)**3
  soma4<-soma4+(dados$Zgauss[i]-media)**4
  desvio<-desvio+(dados$Zgauss[i]-media)**2
}
desvio<-sqrt(desvio/n)
desvio
assimetria<-(1/n)*soma3/(desvio**3)
assimetria
curtose<-(1/n)*soma4/(desvio**4)
curtose