setwd("C:\\geoEspacial\\DataR\\cores")
cores <- read.csv("cores.csv",sep=";",header=TRUE)
r=cores$r
g=cores$g
b=cores$b
ncores=length(r)
setwd("C:\\geoStats\\dados\\gravimetria\\dadosPoli")
getwd()
#leitura do arquivo de parametros
parametros <- read.csv("graviAnhembi_par.csv",sep=";",header=TRUE)
if (length(parametros)>8) {code=-999.00} else {code=-99.00}
xmin=parametros$xmin
xmax=parametros$xmax
dx=parametros$dx
nx=parametros$nx
ymin=parametros$ymin
ymax=parametros$ymax
dy=parametros$dy
ny=parametros$ny
#leitura do arquivo de dados
#o arquivo csv gerado pelo R tem a "," como separador
dados <- read.csv("graviAnhembi_eqmqLocal.csv",sep=",",header=TRUE)
#as colunas estao definidas na saida do superTrend.R
x=dados[,1]
y=dados[,2]
z=dados[,3]
n=length(z)
n
##define as dimensoes do mapa
if ((xmax-xmin) > (ymax-ymin))
{
  cx=40
  cy=(ymax-ymin)*cx/(xmax-xmin)
} else 
{
  cy=40
  cx=(xmax-xmin)*cy/(ymax-ymin)
}
cx
cy
dev.new(width=cx, height=cy, unit="cm")
par(mar=c(5,5,10,10), xpd=TRUE)
plot(NA,NA,xlim=c(xmin,xmax), ylim=c(ymin,ymax), type="n", 
frame=TRUE, xlab=colnames(dados[1]), ylab=colnames(dados[2]))
#
zmin=9.9e+20
zmax=-zmin
for (i in 1:n)
{
  if (z[i] != code)
    {
      if (z[i] < zmin) {zmin=z[i]}
      if (z[i] > zmax) {zmax=z[i]}
    }
}
zmin
zmax
delta=(zmax-zmin)*1.0000001
#plotagem como retangulos
ncores
for (i in 1:n)  {
  if (z[i] != -99.000) {
    x1=x[i]-dx/2
    y1=y[i]-dy/2
    x2=x[i]+dx/2
    y2=y[i]+dy/2
    cor=trunc((z[i]-zmin)*ncores/delta)+1
    rect(x1,y1,x2,y2,border=NA,col=rgb(r[cor],g[cor],b[cor])) 
  }
}
#plotagem da legenda de cores
eixox=c(rep(xmax+7,ncores+1))
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
text(x=c(eixox),y=c(eixoy),c(vetorz),xpd=NA,cex=1, pos=4)
for (i in 1:ncores)
{rect(xmax+5,eixoy[i],xmax+7,eixoy[i+1],border=NA,col=rgb(r[i],g[i],b[i]))}
x1=xmin-13.5
x2=xmax+18.5
y1=ymin-13.5
y2=ymax+7.5
segments(x1,y1,x2,y1,col="red")
segments(x2,y1,x2,y2,col="red")
segments(x2,y2,x1,y2,col="red")
segments(x1,y2,x1,y1,col="red")
require(tcltk)
msgBox <- tkmessageBox(title = "Visualize gráfico",
                       message = "Para gravar e encerrar, OK.", icon = "info", type = "ok")
#fonte: https://stackoverflow.com/questions/7144118/how-to-save-a-plot-as-image-on-the-disk
dev.copy(png,filename="simetrica100_trend.png")
graphics.off()
cat("Arquivo gráfico gravado com sucesso")
