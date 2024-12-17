#Yamamoto (2020, p. 269-270)
library(RANN)     #nn2 function
require(RANN)
library(sfsmisc)  #quadrant function
require(sfsmisc)

qVizinhos<-function(x,y,z,x0,y0,nppq,npvq){
      print(c(x0,y0))
      dadosXY=data.frame(x,y)
      ponto=matrix(data=c(x0,y0),nrow=1,ncol=2)
      nearest=nn2(dadosXY,ponto,k=npvq)
      relacao=nearest$nn.idx
      #print(c(relacao))
      nprox=length(relacao)
      xpp=c(rep(0,nprox)); ypp=c(rep(0,nprox)); zpp=c(rep(0,nprox))
      for (k in 1:nprox){
        xpp[k]=x[relacao[k]]-x0
        ypp[k]=y[relacao[k]]-y0
        zpp[k]=z[relacao[k]]
      }
      #faz a classificacao por quadrantes
      dataProx=data.frame(xpp,ypp)
      quad=quadrant(dataProx)
      #acrescenta zpp,relacao e quad ao data frame
      dataProx$zpp=round(zpp,digits=2)
      relacaoVet=matrix(data=c(relacao),nrow=nprox,ncol=1)
      dataProx$relacao=relacaoVet
      dataProx$quad=quad
print(dataProx)
print("===============================")
      #faz o sort por quadrantes (em relacao a x0,y0)
      classQuad=dataProx[order(dataProx$quad),]
      print(classQuad)
      #cria vetores para guardar a relacao de pontos prox. por quadrante
      index1=c(rep(0,nppq)); index2=c(rep(0,nppq))
      index3=c(rep(0,nppq)); index4=c(rep(0,nppq))
      vetQuad=classQuad$quad; vetRelacao=classQuad$relacao
      npa=c(rep(0,4))
      for (k in 1:nprox){
        npa[vetQuad[k]]=npa[vetQuad[k]]+1
        if (npa[vetQuad[k]]<=nppq) {
          if (vetQuad[k]==1) {index1[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==2) {index2[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==3) {index3[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==4) {index4[npa[vetQuad[k]]]=vetRelacao[k]}
          }
      }
      #verifica o numero de pontos knt nos quadrantes
      knt=0
      for (k in 1:nppq){
        if (index1[k] > 0){knt=knt+1}
        if (index2[k] > 0){knt=knt+1}
        if (index3[k] > 0){knt=knt+1}
        if (index4[k] > 0){knt=knt+1}
      }
      indices=c(rep(0,knt))
      knt=0
      for (k in 1:nppq){
        if (index1[k] > 0) {
          knt=knt+1
          indices[knt]=index1[k]}
      }
      for (k in 1:nppq){
        if (index2[k] > 0) {
          knt=knt+1
          indices[knt]=index2[k]}
     }
      for (k in 1:nppq){
        if (index3[k] > 0) {
          knt=knt+1
          indices[knt]=index3[k]}
      }
      for (k in 1:nppq){
        if (index4[k] > 0) {
          knt=knt+1
          indices[knt]=index4[k]}
      }
return(indices)
}
library(sp)
#script para verificacao dos nos que pertencem a fronteira convexa
setwd("C:\\geoEspacial\\DataR\\simetrica25")
#leitura do arquivo de parametros
par<- read.csv("simetrica25_par5.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code  #valor nao calculado
#leitura dos pontos da fronteira convexa - 
#CSV gerado no R tem "," como separador
fronteira <- read.csv("simetrica25_CVX.csv",sep=",",header=TRUE)
nhull=length(xhull); xhull=c(rep(0,nhull)); yhull=c(rep(0,nhull))
xhull=fronteira$X; yhull=fronteira$Y
#geracao da malha regular de nx por ny
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(-99,nx*ny))
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
dados <- read.csv("simetrica25.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; z=dados$Zgauss
zmin=min(z); zmax=max(z); n=length(z)
dadosXY=data.frame(x,y)
#cat("comeca a malha regular")
#interpolacao da malha regular
nppq=2  #pesquisa com 2 pontos por quadrante
npvq=min(4*nppq*4,n)
for (i in 1:ny){
  for (j in 1:nx){
    kb=j+(i-1)*nx
    if (ordem[kb] > 0) {
      x0=xp[kb]
      y0=yp[kb]
      indices=qVizinhos(x,y,z,x0,y0,nppq,npvq)
      print(indices)
    }  #if ordem>0
  }  
}
cat("fim")

