#Yamamoto (2020, p. 47-49)
setwd("C:\\geoEspacial\\DataR\\teste150")
dados <- read.csv("teste150.csv",sep=";",header=TRUE)
x <- dados$X; y <- dados$Y; z <- dados$Zgauss
n <- length(z)
# x, y e z são vetores de comprimento n.
par<- read.csv("parametros.csv",sep=";",header=TRUE);
xmin <- par$Xmin; xmax <- par$Xmax
ymin <- par$Ymin; ymax <- par$Ymax
dx <- par$DX; dy <- par$DY
mcel<-length(xmin)
mcel
#faz a verificacao dos pontosXparametros
inX=TRUE; inY=TRUE
for (i in 1:n){
  for (j in 1:mcel){
    if (x[i] < xmin[j] | x[i] > xmax[j]){inX=FALSE}
    if (y[i] < ymin[j] | y[i] > ymax[j]){inY=FALSE}
  }
}
if (inX & inY){
medias <- c(rep(0,mcel))
for (m in 1:mcel){
  nx <- round((xmax[m]-xmin[m])/dx[m],digits=0)
  ny <- round((ymax[m]-ymin[m])/dy[m],digits=0)
  celula <- c(rep(0,nx*ny))
  wcel <- c(rep(0,n))
  for (i in 1:n){
    kx <- trunc((x[i]-xmin[m])/dx[m])+1
    ky <- trunc((y[i]-ymin[m])/dy[m])+1
    kcel <- kx+(ky-1)*nx
    celula[kcel] <- celula[kcel]+1
    wcel[i] <- kcel
  }
  k <- 0;jota <- 0
  for (i in 1:ny) {
    for (j in 1:nx){
      k<-k+1
      if (celula[k] > 0)
        {jota<- jota+1}
    }
  }
  peso <- c(rep(0,n))
  for (i in 1:n)
  {
    kcel <- wcel[i]
    peso[i] <- 1/(celula[kcel]*jota)
  }
  soma<-0
  soma2<-0
  for (i in 1:n) {soma<-soma+peso[i]*z[i]}
  media<-soma
  for (i in 1:n) {soma2<-soma2+(peso[i])*((z[i]-media)^2)}
  soma2  #e a variancia para a media calculada
  medias[m] <- media
}
#setwd("C:\\geoEspacial\\DataR\\teste150")
#pdf("Fig8_7.pdf",width=5,height=5)
medias
mediaOtima <- min(medias)
mediaOtima
indice <- which.min(medias)
dx[indice]
print(c(mediaOtima,dx[indice]))
par(mar=c(5,5,2,2))
plot(dx,medias,pch=16,cex=1.5,col="red",xlab="DX",ylab="MÉDIA",
cex.lab=1.25,cex.main=1.25,main="Média Ótima",cex.axis=1.0)
lines(dx,medias,col="green",lwd=2)
#dev.off()
} else print("Pontos fora dos limites definidos no arquivo de parâmetros!")