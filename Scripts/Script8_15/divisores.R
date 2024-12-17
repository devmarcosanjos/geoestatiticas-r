#Yamamoto (2020, p. 246)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
variavel = dados$Zgauss
quantile(variavel)
quantile(variavel, prob=seq(0,1,length=11),type=5)
quantile(variavel, prob=seq(0,1,length=101),type=5)
