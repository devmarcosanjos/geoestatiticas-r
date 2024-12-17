#Yamamoto (2020, p. 257)
library(sp)
#script para verificacao dos nos que pertencem a fronteira convexa
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
#leitura dos pontos da fronteira convexa - 
#CSV gerado no R tem "," como separador
fronteira <- read.csv("simetrica100_CVX.csv",sep=",",header=TRUE)
nhull=length(fronteira$X)
xhull=c(rep(0,nhull)); yhull=c(rep(0,nhull))
xhull=fronteira$X; yhull=fronteira$Y
#geracao da malha regular de nx por ny
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  xp[kb]=xmin+(j-1)*dx+dx/2
  yp[kb]=ymin+(i-1)*dy+dy/2
  }
}
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_25.pdf",width=5,height=5)
#xhull e yhull devem estar orientados no sentido antihorario
ordem=c(rep(0,nx*ny))
ordem=point.in.polygon(xp,yp,xhull,yhull)
plot(x=xhull,y=yhull,xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="Leste", ylab="Norte")
lines(x=xhull,y=yhull)
#plotagem dos pontos que pertencem a fronteira convexa
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  if (ordem[kb] > 0) {points(x=xp[kb],y=yp[kb],col="blue",pch=19,cex=0.45)} else{
    points(x=xp[kb],y=yp[kb],col="red",pch=19,cex=0.45)}
  }
}
#dev.off()
