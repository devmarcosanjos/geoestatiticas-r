#Yamamoto (2020, p. 249)
setwd("C:\\geoEspacial\\DataR\\simetrica25")
z25 <- read.csv("simetrica25.csv",sep=";",header=TRUE)
media <- mean(z25$Zgauss)
desvio <- sd(z25$Zgauss)
n <- length(z25$Zgauss)
erro <- qt(0.975,df=n-1)*desvio/sqrt(n)
inferior <- media-erro
superior <- media+erro
media
inferior
superior
