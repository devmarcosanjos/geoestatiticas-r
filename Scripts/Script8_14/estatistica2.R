#Yamamoto (2020, p. 245-246)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
var(dados$Zgauss)
sd(dados$Zgauss)
summary(dados$Zgauss)
IQR(dados$Zgauss)
cv <- sd(dados$Zgauss)/mean(dados$Zgauss)
cv
