#Yamamoto (2020, p. 241-242)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
variavel = dados$Zgauss
breaks=c(2.957,5.573,8.189,10.805,13.420,
16.036,18.652,21.268,23.884,26.500)
variavel.cut = cut(variavel, breaks, right=FALSE) 
variavel.freq = table(variavel.cut)
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_14.pdf",width=5,height=5)
cumfreq0 = c(0, cumsum(variavel.freq)) 
plot(breaks, cumfreq0,main="Zgauss",xlab="Zgauss",
ylab="Frequência acumulada")
lines(breaks, cumfreq0)
#dev.off()
