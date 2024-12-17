#Yamamoto (2020, p. 249-250)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
media <- mean(dados$Zgauss)
desvio <- sd(dados$Zgauss)
n <- length(dados$Zgauss)
erro <- qnorm(0.975)*desvio/sqrt(n)
inferior <- media-erro
superior <- media+erro
media
inferior
superior
