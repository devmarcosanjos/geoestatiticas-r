#Yamamoto (2020, p. 254)
#script para transformacao para escores uniformes
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\simetrica100")
#leitura do arquivo de dados
dados <- read.csv("simetrica100.CSV",sep=";",header=TRUE)
n=length(dados$X)
#fazendo o sort conforme Zgauss=coluna da variavel
sorteado=dados[order(dados$Zgauss),]  #ordena todas as colunas do dataframe
#calcula frequencias acumuladas
uniformes=c(rep(0,n))
for (i in 1:n)
{uniformes[i]=(i/n)-(1/(2*n))}
#criando uma nova coluna
sorteado$Ranked=uniformes
write.csv(sorteado,file="simetrica100_rank.csv",row.names=FALSE)
