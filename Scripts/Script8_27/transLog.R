#Yamamoto (2020, p. 253)
#script para transformacao logaritmica
#escrito por Jorge Kazuo Yamamoto
setwd("C:\\geoEspacial\\DataR\\positive100")
getwd()
#leitura do arquivo de dados
dados <- read.csv("positive100.CSV",sep=";",header=TRUE)
n=length(dados$X)
#calculando a transformada logaritmica
transformado=log(dados$Zlog)
#criando uma nova coluna
dados$NewZ=transformado
write.csv(dados,file="positive100_log.csv",row.names=FALSE)
