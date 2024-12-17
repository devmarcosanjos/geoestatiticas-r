#Yamamoto (2020, p. 238)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_13.pdf",width=5,height=5)
hist(dados$Zgauss,col="orange",
breaks=c(2.957,5.573,8.189,10.805,13.420,16.036,18.652,21.268,23.884,26.500))
#dev.off()