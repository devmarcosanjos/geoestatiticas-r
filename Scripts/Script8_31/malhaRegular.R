#Yamamoto (2020, p. 255)
#script para definicao de uma malha regular
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  xp[kb]=xmin+(j-1)*dx+dx/2
  yp[kb]=ymin+(i-1)*dy+dy/2
  }
}
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_22.pdf",width=5,height=5)
plot(NA,NA,xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="Leste", ylab="Norte")
#plotagem dos pontos que pertencem a fronteira convexa
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  points(x=xp[kb],y=yp[kb],col="blue",pch=19,cex=0.45)}
}
#dev.off()
