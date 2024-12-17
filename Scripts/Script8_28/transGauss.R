#Yamamoto (2020, p. 254)
#script para transformacao gaussiana de dados
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de dados
dados <- read.csv("simetrica100.CSV",sep=";",header=TRUE)
n=length(dados$X)
#fazendo o sort conforme Zgauss=coluna da variavel
sorteado=dados[order(dados$Zgauss),]
sorteado
#calcula frequencias acumuladas
freq_acum=c(rep(0,n))
for (i in 1:n)
{freq_acum[i]=i/(n+1)}
#transformando frequencias acumuladas em escores normais
novoZ=qnorm(freq_acum)
#criando uma nova coluna
sorteado$NewZ=novoZ
write.csv(sorteado,file="simetrica100_gauss.csv")
