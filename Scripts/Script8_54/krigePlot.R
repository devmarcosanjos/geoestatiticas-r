#Yamamoto (2020, p. 280-281)
setwd("C:\\geoEspacial\\DataR\\cores")
cores <- read.csv("cores.csv",sep=";",header=TRUE)
r=cores$r; g=cores$g; b=cores$b; ncores=length(r)
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code
#leitura do arquivo de dados
#o arquivo csv gerado pelo R tem a "," como separador
dados <- read.csv("simetrica100_krig.csv",sep=",",header=TRUE)
x=dados[,1]; y=dados[,2]; z=dados[,3]   
n=length(z)
#define as dimensoes do mapa
if ((xmax-xmin) > (ymax-ymin)){
  cx=5
  cy=(ymax-ymin)*cx/(xmax-xmin)
} else {
  cy=5
  cx=(xmax-xmin)*cy/(ymax-ymin)
}
#entra a fronteira convexa para limitar o plot
#leitura dos pontos da fronteira convexa - 
#CSV gerado no R tem "," como separador
setwd("C:\\geoEspacial\\DataR\\simetrica100") 
fronteira <- read.csv("simetrica100_CVX.csv",sep=",",header=TRUE)
nhull=length(fronteira$X)
xhull=c(rep(0,nhull)); yhull=c(rep(0,nhull))
xhull=fronteira$X; yhull=fronteira$Y
require(raster)
require(gstat) 
require(RANN)
require(sfsmisc)
require(sp)
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
#plot(x=xhull,y=yhull,xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="Leste", ylab="Norte")
#lines(x=xhull,y=yhull)
#verifica zmin e zmax dentro da fronteira convexa
zmin=9.9e+20; zmax=-zmin
for (j in 1:nx){
  for (i in 1:ny){
    kb=j+(i-1)*nx;
    if (ordem[kb] > 0){
    if (z[kb] < zmin){zmin=z[kb]}
    if (z[kb] > zmax){zmax=z[kb]}
    }
  }
}
dev.new(width=cx, height=cy, unit="in")
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_42.pdf",width=cx,height=cy)
par(mar=c(5,5,10,10), xpd=TRUE,cex=0.75)
plot(NA,NA,xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n", 
frame=TRUE, xlab=colnames(dados[1]), ylab=colnames(dados[2]))
delta=(zmax-zmin)*1.0000001
#plotagem como retangulos
for (j in 1:nx){
  for (i in 1:ny){
    kb=j+(i-1)*nx;
    if (ordem[kb] > 0){
      x1=x[kb]-dx/2
      y1=y[kb]-dy/2
      x2=x[kb]+dx/2
      y2=y[kb]+dy/2
      cor=trunc((z[kb]-zmin)*ncores/delta)+1
      rect(x1,y1,x2,y2,border=NA,col=rgb(r[cor],g[cor],b[cor])) 
    }
  }
}

#plotagem da legenda de cores
dx1=(xmax-xmin)*0.10; dx2=(xmax-xmin)*0.14
eixox=c(rep(xmax+dx2,ncores+1))
eixoy=c(rep(0,ncores+1))
deltay=(ymax-ymin)/ncores
for (i in 1:(ncores+1))
{eixoy[i]=ymin+(i-1)*deltay}
eixoz=c(rep(0,ncores+1))
deltaz=(zmax-zmin)/ncores
for (i in 1:(ncores+1))
{eixoz[i]=zmin+(i-1)*deltaz}
vetorz=c(rep(0,ncores+1))
vetorz=sprintf("%.3f",eixoz)
text(x=c(eixox),y=c(eixoy),c(vetorz),xpd=NA,cex=0.85, pos=4)
for (i in 1:ncores)
{rect(xmax+dx1,eixoy[i],xmax+dx2,eixoy[i+1],border=NA,col=rgb(r[i],g[i],b[i]))}
#dev.off()
