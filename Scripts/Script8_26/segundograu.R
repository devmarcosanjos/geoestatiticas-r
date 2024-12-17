#Yamamoto (2020, p. 252-253)
setwd("C:\\geoEspacial\\DataR\\tempo")
parabola <- read.csv("parabola.csv",sep=";",header=TRUE)
#setwd("C:\\AJKY\\geoestatistica\\capitulos_2020\\figuras\\cap8\\pdf")
#pdf("Fig8_21.pdf",width=5,height=5)
fit <- lm(parabola$Y ~ 1 + parabola$X + I(parabola$X^2))
fit
plot(parabola$X,parabola$Y,pch=16,cex=1.5,col="blue",xlab="X",ylab="Y")
pontos <- seq(0,5,length=101)
funcao <- 0.7329+2.5044*pontos-0.4397*pontos^2
lines(pontos,funcao,col="red",lwd=2)
#dev.off()
