#Yamamoto (2020, p. 279-280)
setwd("C:\\geoEspacial\\DataR\\simetrica100") 
require(raster)
require(gstat) 
require(RANN)
require(sfsmisc)
require(sp)
#leitura do arquivo de parametros
par<- read.csv("simetrica100_par.csv",sep=";",header=TRUE)
xmin=par$xmin; xmax=par$xmax; dx=par$dx; nx=par$nx
ymin=par$ymin; ymax=par$ymax; dy=par$dy; ny=par$ny
code=par$code
require(raster)
require(gstat) 
require(RANN)
require(sfsmisc)
require(sp)
#leitura do arquivo de dados
data <- read.csv('simetrica100.csv', header = TRUE, sep = ";")
coordinates(data) <- ~X+Y
# mapa de localizacao de pontos
rbPal <- colorRampPalette(c('blue','red'))
data$Col <- rbPal(10)[as.numeric(cut(data$Zgauss,breaks = 10))]
dev.new(width=9, height=7)
plot(data, axes=T, main="Amostras", xlab="X", ylab="Y")
points(data$X,data$Y, col= data$Col, pch=19)
legend("topleft",title="Legenda",legend=c(round(min(data$Zgauss),2),"","","","","","","","", round(max(data$Zgauss),2)),col =rbPal(10),pch=19)

# variograma
dev.new(width=10, height=5)
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_40.pdf",width=10,height=7.5)
par(mar=c(5,5,10,10), xpd=TRUE)
exp_var <- variogram(data$Zgauss~1, data, cutoff=20)
mod_var <- vgm(20.4, "Sph", 12, 0)	#var model (sill, model, range, nugget)
plot(exp_var, mod_var, col='red', pch=19, main="Variograma")
#dev.off()
#define as dimensoes do mapa
if ((xmax-xmin) > (ymax-ymin)){
  cx=10
  cy=(ymax-ymin)*cx/(xmax-xmin)
} else {
  cy=10
  cx=(xmax-xmin)*cy/(ymax-ymin)
}
dev.new(width=cx, height=cy, unit="cm")
#setwd("C:\\geoEspacial\\DataR\\tempo")
#pdf("Fig8_41.pdf",width=cx,height=cy)
par(mar=c(5,5,10,10), xpd=TRUE,cex=1)
#krigagem
raster <- raster(xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax,nrows=ny,ncols=nx)
grid <- SpatialPoints(raster)
krig <- krige(data$Zgauss~1, data, grid, model = mod_var)
raster_krig <- rasterFromXYZ(krig)
plot(raster_krig, main= 'Krigagem Ordinária')
#dev.off()



