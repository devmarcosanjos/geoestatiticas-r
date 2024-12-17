#Yamamoto (2020, p. 274-275)
library(sp)
#script para interpolacao em triangulos de Delaunay
setwd("C:\\geoEspacial\\DataR\\simetrica25")
#leitura do arquivo de parametros
par<- read.csv("simetrica25_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code
deltaX=(xmax-xmin)*1.0000001; deltaY=(ymax-ymin)*1.0000001
#leitura do arquivo de dados
dados <- read.csv("simetrica25.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; z=dados$Zgauss; n=length(z)
#leitura do arquivo de triangulos, conforme arquivo de dados acima
triangulos <- read.csv("simetrica25_tri.csv",sep=",",header=TRUE)
ntri=length(triangulos$i1)
#geracao da malha regular de nx por ny
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(code,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
  kb=j+(i-1)*nx
  xp[kb]=xmin+(j-1)*dx+dx/2
  yp[kb]=ymin+(i-1)*dy+dy/2
  }
}
xhull=c(rep(0,4)); yhull=c(rep(0,4)); zhull=c(rep(0,4))
for (it in 1:ntri){
  #processa triangulo por triangulo
  for (jt in 1:3){
  xhull[jt]=x[triangulos[it,jt]]; yhull[jt]=y[triangulos[it,jt]]; zhull[jt]=z[triangulos[it,jt]]
  }
  #poligono fechado
  xhull[4]=xhull[1]; yhull[4]=yhull[1]; zhull[4]=zhull[1]
  xtrmin=min(xhull); xtrmax=max(xhull); ytrmin=min(yhull); ytrmax=max(yhull); 
  i1=trunc((ytrmin-ymin)*ny/deltaY)+1; i2=trunc((ytrmax-ymin)*ny/deltaY)+1
  j1=trunc((xtrmin-xmin)*nx/deltaX)+1; j2=trunc((xtrmax-xmin)*nx/deltaX)+1
  #ajuste do plano nos vertices do triangulo
  a=c(rep(0,9)); b=c(rep(0,3))
  for (jt in 1:3){
    vetor=c(1,xhull[jt],yhull[jt])
    for (k in 1:3){b[k]=b[k]+vetor[k]*zhull[jt]}
    for (l in 1:3){
      for (k in 1:3){
        km=k+(l-1)*3
        a[km]=a[km]+vetor[l]*vetor[k]
      }
    }
  }
  #resolucao do sistema de equacoes
  A<-matrix(data=c(a),nrow=3,ncol=3,byrow=TRUE)
  C<-matrix(data=c(b),nrow=3,ncol=1,byrow=FALSE)
  X=solve(A,C)
  #faz a interpolacao das celulas que pertencem ao triangulo
  for (lin in i1:i2){
    for (col in j1:j2){
      kb=col+(lin-1)*nx
      x0=c(xp[kb])
      y0=c(yp[kb])
      loc=point.in.polygon(x0,y0,xhull,yhull)
      if (loc > 0){zp[kb]=X[1]+X[2]*x0+X[3]*y0}
    }
  }
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica25_interTri.csv",row.names=FALSE)
cat("fim")
