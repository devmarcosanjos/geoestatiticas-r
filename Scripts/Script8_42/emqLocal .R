#Yamamoto (2020, p. 270-271)
library(RANN)     #nn2 function
require(RANN)
library(sfsmisc)  #quadrant function
require(sfsmisc)
# fonte nn2: https://www.rforge.net/doc/packages/RANN/nn.html
# fonte quadrant: https://www.rdocumentation.org/packages/sfsmisc/versions/1.1-5/topics/quadrant
#escrito por Jorge Kazuo Yamamoto
qVizinhos<-function(x,y,z,x0,y0,nppq,npvq){
      #print(c(x0,y0))
      dadosXY=data.frame(x,y)
      ponto=matrix(data=c(x0,y0),nrow=1,ncol=2)
      #localiza os npvq pontos + proximos
      nearest=nn2(dadosXY,ponto,k=npvq)
      relacao=nearest$nn.idx
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
      dataProx$zpp=zpp
      relacaoVet=matrix(data=c(relacao),nrow=nprox,ncol=1)
      dataProx$relacao=relacaoVet
      dataProx$quad=quad
      #faz o sort por quadrantes (em relacao a x0,y0)
      classQuad=dataProx[order(dataProx$quad),]
      #print(classQuad)
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
      #indices=localizacao dos pontos nos dados de entrada
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
setwd("C:\\geoEspacial\\DataR\\simetrica100")
getwd()
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
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
nppq=1  #pesquisa com 1 ponto por quadrante
npvq=min(4*nppq*4,n)
konst=0.1  #definicao da constante multiquadrica
for (i in 1:ny){
  for (j in 1:nx){
    kb=j+(i-1)*nx
    if (ordem[kb] > 0) {
      x0=xp[kb]
      y0=yp[kb]
      indices=qVizinhos(x,y,z,x0,y0,nppq,npvq)
      knt=length(indices)
      xpx=c(rep(0,knt)); ypx=c(rep(0,knt)); zpx=c(rep(0,knt))
      for (k in 1:knt){
        xpx[k]=x[indices[k]]
        ypx[k]=y[indices[k]]
        zpx[k]=z[indices[k]]
      }
      knt1=knt+1
      b=c(rep(0,knt1)); a=c(rep(0,knt1*knt1))
      #matriz dos coeficientes
      for (l in 1:knt){
        for (k in 1:knt){
          kb=k+(l-1)*knt1
          a[kb]=sqrt((xpx[l]-xpx[k])^2+(ypx[l]-ypx[k])^2+konst)
        }
      }
      #condicao de nao vies
      for (k in 1:knt){
        kb=k+(knt1-1)*knt1
        a[kb]=1
      }
      for (l in 1:knt){
        kb=knt1+(l-1)*knt1
        a[kb]=1
      }
      kb=knt1+(knt1-1)*knt1
      a[kb]=0
      #vetor Y
      for (l in 1:knt){
        b[l]=sqrt((xpx[l]-x0)^2+(ypx[l]-y0)^2+konst)}
      b[knt1]=1
      A<-matrix(data=c(a),nrow=knt1,ncol=knt1,byrow=TRUE)
      C<-matrix(data=c(b),nrow=knt1,ncol=1,byrow=FALSE)
      X<-solve(A,C)
      #calculando o valor do grid (x0,y0)
      sz=0
      for (k in 1:knt){sz=sz+X[k]*zpx[k]}
    } else {sz=code}
    kb=j+(i-1)*nx
    zp[kb]=sz
  }  
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica100_eqmqLocal.csv",row.names=FALSE)
cat("fim")
