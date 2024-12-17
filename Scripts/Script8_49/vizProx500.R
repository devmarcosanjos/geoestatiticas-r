library(RANN)     #nn2 function
require(RANN)
# fonte nn2: https://www.rforge.net/doc/packages/RANN/nn.html
#script escrito por Jorge Kazuo Yamamoto
library(sp)
#script para verificacao dos nos que pertencem a fronteira convexa
setwd("C:\\geoEspacial\\DataR\\simetrica100")
getwd()
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par500.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code  #valor nao calculado
#leitura dos pontos da fronteira convexa - 
#CSV gerado no R tem "," como separador
fronteira <- read.csv("simetrica100_CVX.csv",sep=",",header=TRUE)
nhull=length(fronteira$X)
xhull=c(rep(0,nhull)); yhull=c(rep(0,nhull))
xhull=fronteira$X; yhull=fronteira$Y
#geracao da malha regular de nx por ny
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  xp[kb]=xmin+(j-1)*dx+dx/2
  yp[kb]=ymin+(i-1)*dy+dy/2
  }
}
#xhull e yhull devem estar orientados no sentido antihorario
ordem=c(rep(0,nx*ny))
ordem=point.in.polygon(xp,yp,xhull,yhull)
plot(x=xhull,y=yhull,xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="Leste", ylab="Norte")
lines(x=xhull,y=yhull)
#plotagem dos pontos que pertencem a fronteira convexa
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  if (ordem[kb] > 0) {points(x=xp[kb],y=yp[kb],col="blue",pch=19)} else{
    points(x=xp[kb],y=yp[kb],col="red",pch=19)}
  }
}
#leitura do arquivo de dados
dados <- read.csv("simetrica100.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; z=dados$Zgauss
zmin=min(z); zmax=max(z); n=length(z)
cat("comeca a malha regular")
#interpolacao da malha regular
dadosXY=data.frame(x,y)
for (i in 1:ny){
  for (j in 1:nx){
    kb=j+(i-1)*nx
    if (ordem[kb] > 0) {
      x0=xp[kb]
      y0=yp[kb]
      ponto=matrix(data=c(x0,y0),nrow=1,ncol=2)
      nearest=nn2(dadosXY,ponto,k=1)
      relacao=nearest$nn.idx
      relacao=nearest$nn.idx
      kb=j+(i-1)*nx
      zp[kb]=z[relacao[1]]
    } else {zp[kb]=code}
  }  
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica100_vizProx500.csv",row.names=FALSE)
cat("fim")
