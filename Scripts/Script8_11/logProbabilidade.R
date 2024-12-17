#Yamamoto (2020, p. 243-244)
#programa para plotagem curva log-prob. arit.
#escrito por Jorge Kazuo Yamamoto
#{altere as suas variaveis
setwd("C:\\geoEspacial\\DataR\\positive100")
dados <- read.csv("positive100.CSV",sep=";",header=TRUE)
zmin=min(dados$Zlog,na.rm=TRUE)
zmax=max(dados$Zlog,na.rm=TRUE)
z=dados$Zlog
titulo=colnames(dados[3])
# ate aqui}
zsort=sort(z,decreasing=FALSE)
nc=length(zsort)
freq=c(rep(1,nc))
freq_acum=c(rep(0,nc))
freq_acum[1]=freq[1]/(nc+1)
for (j in 2:nc) {freq_acum[j]=freq_acum[j-1]+freq[j]/(nc+1)}
acum_gauss=qnorm(freq_acum)
#transformando os dados base em log10
zlog10=log10(zsort)
#encontrando min e max em termos de decadas logaritmicas
logZmin=log10(zmin)
decMin=floor(logZmin)
logZmax=log10(zmax)
decMax=ceiling(logZmax)
if (logZmax < 0) {decMax=ceiling(-logZmax)}
if (logZmax < 0) {decMax=-decMax}
ymin=-4.7534243
ymax=4.7534243
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_16.pdf",width=5,height=5)
plot(NA,NA,xlim=c(decMin,decMax), ylim=c(ymin,ymax), type="n", 
xaxt="n", yaxt="n",frame=FALSE, xlab=titulo, ylab="Frequência acumulada")
eixox=seq(decMin,decMax)
axis(side=1, labels=FALSE, at=c(eixox),line=0., tick=TRUE,outer=FALSE)
nl=length(eixox)
deltax=0.10
if (decMax-decMin < 3) {deltax=0.05}
#codigo de plotagem dos labels dos eixos baseado em Annis (2020)
#https://stat.ethz.ch/pipermail/r-help/2004-February/045370.html#
text(x=c(eixox),y=rep(ymin-0.8,nl),rep("10",nl), xpd=NA, cex=0.8)
text(x=c(eixox)+deltax,y=rep(ymin-0.6,5)+0.08,c(as.character(eixox)),xpd=NA,cex=0.6)
for (i in eixox[1]:(eixox[nl]-1)){
  axis(side=1, at=log10(c(2, 3, 4, 5, 6, 7, 8, 9))+ i,   cex=1, labels=NA,
  line = 0, tck = -0.01)
  text(x=log10(c(2, 3, 4, 5, 6, 7, 8))+ i, y=rep(ymin-0.6, 7), c("2", "3",
  "4", "5", "6", "7", "8"), xpd = NA, cex=0.7)
}
porcentagens<-c(rep(0,25))
porcentagens<-c(0.0001,0.001,0.01,0.05,0.10,0.50,1.00,5.00,10.00,20.00,30.00,40.00,50.00,
60.00,70.00,80.00,90.00,95.00,99.00,99.50,99.90,99.95,99.99,99.999,99.9999)
#for (i in 1:25) {porcentagens[i]=porcentagens[i]/100}
porcentagens=porcentagens/100
eixoy=c(rep(0,25))
eixoy=qnorm(porcentagens)
#calculo do delta no eixo Y para os labels
deltay=-0.004+0.054*(decMax-decMin)
axis(side=2,at=c(eixoy),labels=NA,line=0,tck=-0.01)
text(x=rep(decMin-deltay,25),y=c(eixoy),cex=0.575,xpd=NA,labels=c("0.0001","0.001","0.01","0.05","0.10","0.50",
"1.00","5.00","10.00","20.00","30.00","40.00","50.00","60.00","70.00","80.00",
"90.00","95.00","99.00","99.50","99.90","99.95","99.99","99.999","99.9999"),adj=1)
#plotagem dos pontos
points(x=zlog10,y=acum_gauss,col="darkgreen",pch=16)
abline(lm(acum_gauss~zlog10),col="darkblue",lwd=2)
box()
#dev.off()