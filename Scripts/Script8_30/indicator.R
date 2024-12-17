#Yamamoto (2020, p. 254-255)
#script para transformacao indicadora conforme o teor de corte
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\positive100")
#leitura do arquivo de dados
dados <- read.csv("positive100.CSV",sep=";",header=TRUE)
n=length(dados$X)
z=dados$Zlog
teor=median(z)  #qualquer outro divisor pode ser usado
indicadora=c(rep(0,n))
for (i in 1:n) { if (z[i] < teor) {indicadora[i]=1} else {indicadora[i]=0}}
#criando uma nova coluna
dados$Indicadora=indicadora
write.csv(dados,file="positive100_ind.csv",row.names=FALSE)
