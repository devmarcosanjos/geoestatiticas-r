#Yamamoto (2020, p. 242-243)
#programa para plotagem curva prob. arit. - eixo X em escala aritmetica
#escrito por Jorge Kazuo Yamamoto
#{altere as suas variaveis:
setwd("C:\\geoEspacial\\DataR\\simetrica225")
dados <- read.csv("simetrica225.csv",sep=";",header=TRUE)
names(dados)
zmin=min(dados$Zgauss,na.rm=TRUE)
zmax=max(dados$Zgauss,na.rm=TRUE)
z=dados$Zgauss
titulo=colnames(dados[3])
#ate aqui}
zsort=sort(z,decreasing=FALSE)
nc=length(zsort)
freq=c(rep(1,nc))
freq_acum=c(rep(0,nc))
freq_acum[1]=freq[1]/(nc+1)
for (j in 2:nc) {freq_acum[j]=freq_acum[j-1]+freq[j]/(nc+1)}
acum_gauss=qnorm(freq_acum)
#limites minimo e maximo no eixo Y: 0.01 a 99.99%
ymin=-4.7534243
ymax=4.7534243
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_15.pdf",width=5,height=5)
plot(NA,NA,xlim=c(zmin,zmax), ylim=c(ymin,ymax), type="n", 
xaxt="n", yaxt="n",frame=FALSE, xlab=titulo, ylab="Frequência acumulada")
nx=5
zc=(zmax-zmin)/nx
eixox=c(rep(0,nx+1))
for (i in 1:(nx+1))
{eixox[i]=round(zmin+(i-1)*zc,digits=3)}
axis(side=1, labels=FALSE, at=c(eixox),line=0., tick=TRUE,outer=FALSE)
nl=length(eixox)
deltax=0.10
text(x=c(eixox)+deltax,y=rep(ymin-0.90,5)+0.08,c(as.character(eixox)),xpd=NA,cex=0.7)
porcentagens<-c(rep(0,25))
porcentagens<-c(0.0001,0.001,0.01,0.05,0.10,0.50,1.00,5.00,10.00,20.00,30.00,40.00,50.00,
60.00,70.00,80.00,90.00,95.00,99.00,99.50,99.90,99.95,99.99,99.999,99.9999)
for (i in 1:25) {porcentagens[i]=porcentagens[i]/100}
eixoy=c(rep(0,25))
eixoy=qnorm(porcentagens)
eixoy
axis(side=2,at=c(eixoy),labels=NA,line=0,tck=-0.01)
dz=0.0524*(zmax-zmin)
text(x=rep(zmin-dz,25),y=c(eixoy),cex=0.575,xpd=NA,labels=c("0.0001","0.001","0.01","0.05","0.10","0.50",
"1.00","5.00","10.00","20.00","30.00","40.00","50.00","60.00","70.00","80.00",
"90.00","95.00","99.00","99.50","99.90","99.95","99.99","99.999","99.9999"),adj=1)
#plotagem dos pontos
points(x=zsort,y=acum_gauss,col="red",pch=19,cex=0.75)
abline(lm(acum_gauss~zsort),col="blue",lwd=2)
box()
#dev.off()