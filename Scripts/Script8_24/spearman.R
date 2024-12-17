#Yamamoto (2020, p. 251)
setwd("C:\\geoEspacial\\DataR\\goovaerts_jura10")
cadzin <- read.csv("goovaerts.csv",sep=";",header=TRUE)
correlacao <- cor.test(x=cadzin$Cd,y=cadzin$Zn,method='spearman')
correlacao
