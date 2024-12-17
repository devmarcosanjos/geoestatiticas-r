#Yamamoto (2020, p. 256)
#script para determinacao da fronteira convexa de um conjunto planar
#https://astrostatistics.psu.edu/datasets/R/html/graphics/html/chull.html
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de dados
dados <- read.csv("simetrica100.csv",sep=";",header=TRUE)
x=dados$X; y=dados$Y; n=length(x)
A<-matrix(data=c(x,y),nrow=n,ncol=2,byrow=FALSE)
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_23.pdf",width=5,height=5)
plot(A, cex = 0.5,xlab=colnames(dados)[1],ylab=colnames(dados)[2])
hpts <- chull(x,y)
hpts <- c(hpts, hpts[1])
nhull=length(hpts)
lines(A[hpts, ])
xhull=c(rep(0,nhull)); yhull=c(rep(0,nhull))
#reordenando (xhull,yhull) em antihorario para o point in polygon
j=0; i=nhull
while (i >=1) {
  j=j+1
  xhull[j]=x[hpts[i]]
  yhull[j]=y[hpts[i]]
  i=i-1
}
saida=data.frame(xhull,yhull)
names(saida)[1]=paste("X")
names(saida)[2]=paste("Y")
write.csv(saida,file="simetrica100_CVX.csv",row.names=FALSE)
#dev.off()