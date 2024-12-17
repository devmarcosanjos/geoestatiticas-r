#Yamamoto (2020, p. 259-261)
#script para calcular superficies de tendencia = polinomios bivariados
#escrito por Jorge Kazuo Yamamoto
funcaoPolinomio<-function(grau,xp,yp){
nc=(grau+1)*(grau+2)/2
vet=c(rep(0,nc))
vet[1]=1
vet[2]=xp
vet[3]=yp
h=3
if (grau > 1) {
  for (i in 2:grau) {
    vet[h+1]=vet[h+1-i]*xp
    j1=h+2
    j2=h+i+1
    for (j in j1:j2) {vet[j]=vet[j-i-1]*yp}
    h=h+i+1
  }
}
return(vet)
}
setwd("C:\\geoEspacial\\DataR\\simetrica100")
getwd()
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code  #valor nao calculado
#code nao e usado nos metodos globais
#leitura do arquivo de dados
dados <- read.csv("simetrica100.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; z=dados$Zgauss
zmin=min(z); zmax=max(z); n=length(z)
#calculo dos coeficientes da superficie de tendencia
grau=3  #defina aqui o grau da superficie
h=(grau+1)*(grau+2)/2
b=c(rep(0,h)); a=c(rep(0,h*h))
for (i in 1:n) {
  vetor=funcaoPolinomio(grau,x[i],y[i])
  for (j in 1:h) {b[j]=b[j]+vetor[j]*z[i]}
  for (l in 1:h) {
    for (k in 1:h) {
      km=k+(l-1)*h
      a[km]=a[km]+vetor[l]*vetor[k]
    }
  } 
}
A<-matrix(data=c(a),nrow=h,ncol=h,byrow=TRUE)
C<-matrix(data=c(b),nrow=h,ncol=1,byrow=FALSE)
X=solve(A,C)
cat("Coeficientes do polinomio de grau: ",grau,"\n")
X
#calculo dos valores computados nos pontos de dados
zc=c(rep(0,n))
for (i in 1:n) {
  vetor=funcaoPolinomio(grau,x[i],y[i])
  for (j in 1:h) {zc[i]=zc[i]+vetor[j]*X[j]}
}
#calculo da malha regular conforme os parametros
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
    kb=j+(i-1)*nx
    xp[kb]=xmin+(j-1)*dx+dx/2
    yp[kb]=ymin+(i-1)*dy+dy/2
    zp[kb]=0
    vetor=funcaoPolinomio(grau,xp[kb],yp[kb])
    for (k in 1:h) {zp[kb]=zp[kb]+vetor[k]*X[k]}
  }
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica100_trend.csv",row.names=FALSE)
