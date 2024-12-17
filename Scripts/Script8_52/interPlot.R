#Yamamoto (2020, p. 278-279)
#script para plotagem de mapa com variavel categorica
#escrito por Jorge Kazuo Yamamoto
library(svDialogs)
setwd("C:\\geoEspacial\\DataR\\cores")
cores <- read.csv("coresCat.csv",sep=";",header=TRUE)
r=cores$r; g=cores$g; b=cores$b
setwd("C:\\geoEspacial\\DataR\\geolitos")
#leitura do arquivo de parametros
par<- read.csv("geoLitos_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code
#leitura do arquivo de dados
#o arquivo csv gerado pelo R tem a "," como separador
dados <- read.csv("geoLitos_interCat.csv",sep=",",header=TRUE)
valor=dlg_input("Qual a opcao: 1) Lito mais provavel ou 
2) Lito com incerteza?", default="1", Sys.info()["valor"])$res
opcao=as.integer(valor)
print(opcao)
if (opcao==1){z=dados[,3]} else {z=dados[,4]}
x=dados[,1]; y=dados[,2]; n=length(z)
#define as dimensoes do mapa
if ((xmax-xmin) > (ymax-ymin)){
  cx=5
  cy=(ymax-ymin)*cx/(xmax-xmin)
} else {
  cy=5
  cx=(xmax-xmin)*cy/(ymax-ymin)
}
#Fig8_38 sem zona de incerteza
#Fig8_39 com zona de incerteza
dev.new(width=cx, height=cy, unit="in")
setwd("C:\\geoEspacial\\DataR\\tempo")
pdf("Fig8_39.pdf",width=cx,height=cy)
par(mar=c(5,5,10,10), xpd=TRUE,cex=0.75)
plot(NA,NA,xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n", 
frame=TRUE, xlab=colnames(dados[1]), ylab=colnames(dados[2]))
zmin=9.9e+20; zmax=-zmin
for (i in 1:n){
  if (z[i] != code)    {
      if (z[i] < zmin) {zmin=z[i]}
      if (z[i] > zmax) {zmax=z[i]}
    }
}
delta=(zmax-zmin)*1.0000001
#plotagem como retangulos
ncores=trunc(zmax)
for (i in 1:n)  {
  if (z[i] != code) {
    x1=x[i]-dx/2
    y1=y[i]-dy/2
    x2=x[i]+dx/2
    y2=y[i]+dy/2
    cor=trunc(z[i])
    if (cor==0){rect(x1,y1,x2,y2,border=NA,col=rgb(0.502,0.502,0)) 
      } else {
      rect(x1,y1,x2,y2,border=NA,col=rgb(r[cor],g[cor],b[cor]))}
  }
}
#plotagem da legenda de cores
dx1=(xmax-xmin)*0.10; dx2=(xmax-xmin)*0.14
eixox=c(rep(xmax+dx2,ncores+1))
eixoy=c(rep(0,ncores+1))
deltay=(ymax-ymin)/ncores
for (i in 1:(ncores+1))
{eixoy[i]=ymin+(i-1)*deltay}
eixoz=c(rep(0,ncores))
for (i in 1:ncores)
{rect(xmax+dx1,eixoy[i],xmax+dx2,eixoy[i+1],border=NA,col=rgb(r[i],g[i],b[i]))}
eixox=c(rep(xmax+dx2,ncores))
eixoy=c(rep(0,ncores))
deltay=(ymax-ymin)/ncores
for (i in 1:(ncores))
{eixoy[i]=ymin+(i-1)*deltay+deltay/2}
for (i in 1:(ncores))
{eixoz[i]=i}
vetorz=c(rep(0,ncores))
vetorz=sprintf("%.0f",eixoz)
text(x=c(eixox),y=c(eixoy),c(vetorz),xpd=NA,cex=0.85, pos=4)
if (opcao==2){
  x1=xmin+(xmax-xmin)/10
  x2=xmin+3*(xmax-xmin)/10
  y1=ymax+2.5
  y2=ymax+5
  rect(x1,y1,x2,y2,border=NA,col=rgb(0.502,0.502,0))
  text(x2,(0.5*(y1+y2)),"ZONA DE INCERTEZA",xpd=NA,cex=1,pos=4)
}
dev.off()
