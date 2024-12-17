#Yamamoto (2020, p. 240)
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_12.pdf",width=5,height=5)
hist(dados$Zgauss,freq=FALSE,col="green",
breaks=c(2.957,5.573,8.189,10.805,13.420,16.036,
18.652,21.268,23.884,26.500))
curve(dnorm(x,mean=mean(dados$Zgauss),
sd=sd(dados$Zgauss)),add=TRUE,col="darkblue",lwd=2)
#dev.off()
