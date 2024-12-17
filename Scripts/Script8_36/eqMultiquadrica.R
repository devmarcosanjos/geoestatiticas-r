#Yamamoto (2020, p. 264-266)
#script para interpolacao por equacoes multiquadricas
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code  #valor nao calculado
#leitura do arquivo de dados
dados <- read.csv("simetrica100.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; z=dados$Zgauss
zmin=min(z); zmax=max(z); n=length(z)
#calculo dos coeficientes das equacoes multiquadricas
b=c(rep(0,n)); a=c(rep(0,n*n))
m=0
konst=0.1  #constante multiquadrica pode ser zero ou um valor pequeno
for (l in 1:n) {
  for (k in 1:n) {
    m=m+1
    a[m]=sqrt((x[l]-x[k])^2+(y[l]-y[k])^2+konst)
  }
}
for (l in 1:n) {b[l]=z[l]}
A<-matrix(data=c(a),nrow=n,ncol=n,byrow=TRUE)
C<-matrix(data=c(b),nrow=n,ncol=1,byrow=FALSE)
X=solve(A,C)
#calculo da malha regular conforme os parametros
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
    kb=j+(i-1)*nx
    xp[kb]=xmin+(j-1)*dx+dx/2
    yp[kb]=ymin+(i-1)*dy+dy/2
    zp[kb]=0
    for (k in 1:n) {zp[kb]=zp[kb]+X[k]*sqrt((x[k]-xp[kb])^2+(y[k]-yp[kb])^2+konst)}
  }
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica100_eqmq.csv",row.names=FALSE)
