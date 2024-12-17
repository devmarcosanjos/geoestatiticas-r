#Yamamoto (2020, p. 267)
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
dados <- read.csv("simetrica100_id.csv",sep=",",header=TRUE)
#as colunas estao definidas na saida do superTrend.R
x=dados[,1]; y=dados[,2]; z=dados[,3]; n=length(z)
#define as dimensoes do mapa
if ((xmax-xmin) > (ymax-ymin)){
  cx=5
  cy=(ymax-ymin)*cx/(xmax-xmin)
} else {
  cy=5  
  cx=(xmax-xmin)*cy/(ymax-ymin)
}
dev.new(width=cx, height=cy, unit="in")
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_30.pdf",width=cx,height=cy)
par(mar=c(5,5,10,10), xpd=TRUE,cex=0.75)
plot(NA,NA,xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n", 
frame=TRUE, xlab=colnames(dados[1]), ylab=colnames(dados[2]))
#
zmin=9.9e+20; zmax=-zmin
for (i in 1:n) {
  if (z[i] != code) {
      if (z[i] < zmin) {zmin=z[i]}
      if (z[i] > zmax) {zmax=z[i]}
    }
}
delta=(zmax-zmin)*1.0000001
#plotagem como retangulos
for (i in 1:n)  {
  if (z[i] != code) {
    x1=x[i]-dx/2
    y1=y[i]-dy/2
    x2=x[i]+dx/2
    y2=y[i]+dy/2
    cor=trunc((z[i]-zmin)*ncores/delta)+1
    rect(x1,y1,x2,y2,border=NA,col=rgb(r[cor],g[cor],b[cor])) 
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
