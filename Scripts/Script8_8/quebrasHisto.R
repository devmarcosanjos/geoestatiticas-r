#Yamamoto (2020, p. 240-241)
setwd("C:\\geoEspacial\\DataR\\teste150")
getwd()
dados <- read.csv("teste150.csv",sep=";",header=TRUE)
zmin<-min(dados$Zgauss)
zmax<-max(dados$Zgauss)
n<-length(dados$Zgauss)
nc<-ceiling(1+3.222*log10(n))
nc
quebra<-c(rep(0,nc+1))
#  calculando o numero de quebras nc+1
zc<-(zmax-zmin)/nc
for (i in 1:(nc+1)){quebra[i]<-zmin+(i-1)*zc}
quebra
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_13.pdf",width=5,height=5)
hist(dados$Zgauss,col="blue",breaks=quebra)
#dev.off()