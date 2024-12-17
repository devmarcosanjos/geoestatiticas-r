#Yamamoto (2020, p. 266-267)
#script para interpolacao pelo inverso da distancia
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
n=length(z)
potencia=2  #definicao do ponderador do inverso da distancia
#calculo da malha regular conforme os parametros
xp=c(rep(0,nx*ny)); yp=c(rep(0,nx*ny)); zp=c(rep(0,nx*ny))
for (i in 1:ny) {
  for (j in 1:nx) {
    kb=j+(i-1)*nx
    xp[kb]=xmin+(j-1)*dx+dx/2
    yp[kb]=ymin+(i-1)*dy+dy/2
    zp[kb]=0
    sw=0  #acumula soma dos pesos
    k=1
    while (k <= n) {
      xpl1=xp[kb]-x[k]
      ypl1=yp[kb]-y[k]
      w=sqrt(xpl1^2+ypl1^2)
      if (w > 0) {
        w=1/(w^potencia)
        zp[kb]=zp[kb]+z[k]*w  #formula da media ponderada
        sw=sw+w
      } else  #ha um ponto de dado coincidente: distancia=zero
      { sw=1.00
        zp[kb]=z[k]
        k=n }
      k=k+1
    }
    zp[kb]=zp[kb]/sw  #divide pela soma dos pesos
  }
}
saida=data.frame(xp,yp,zp)
names(saida)[1]=paste("XP")
names(saida)[2]=paste("YP")
names(saida)[3]=paste("ZP")
write.csv(saida,file="simetrica100_id.csv",row.names=FALSE)
