#Yamamoto (2020, p. 267-269)
# fonte nn2: https://www.rforge.net/doc/packages/RANN/nn.html
# fonte quadrant: https://www.rdocumentation.org/packages/sfsmisc/versions/1.1-5/topics/quadrant
# escrito por Jorge Kazuo Yamamoto
qVizinhos<-function(x,y,z,x0,y0,nppq,npvq){
      dadosXY=data.frame(x,y)
      ponto=matrix(data=c(x0,y0),nrow=1,ncol=2)
      #localiza os npvq pontos + proximos
      nearest=nn2(dadosXY,ponto,k=npvq)
      relacao=nearest$nn.idx
      nprox=length(relacao)
      xpp=c(rep(0,nprox)); ypp=c(rep(0,nprox)); zpp=c(rep(0,nprox))
      for (k in 1:nprox){
        xpp[k]=x[relacao[k]]-x0
        ypp[k]=y[relacao[k]]-y0
        zpp[k]=z[relacao[k]]
      }
      #faz a classificacao por quadrantes
      dataProx=data.frame(xpp,ypp)
      quad=quadrant(dataProx)
      #acrescenta zpp,relacao e quad ao data frame
      dataProx$zpp=zpp
      relacaoVet=matrix(data=c(relacao),nrow=nprox,ncol=1)
      dataProx$relacao=relacaoVet
      dataProx$quad=quad
      #faz o sort por quadrantes (em relacao a x0,y0)
      classQuad=dataProx[order(dataProx$quad),]
      print(classQuad)
      #cria vetores para guardar a relacao de pontos prox. por quadrante
      index1=c(rep(0,nppq)); index2=c(rep(0,nppq))
      index3=c(rep(0,nppq)); index4=c(rep(0,nppq))
      vetQuad=classQuad$quad; vetRelacao=classQuad$relacao
      npa=c(rep(0,4))
      for (k in 1:nprox){
        npa[vetQuad[k]]=npa[vetQuad[k]]+1
        if (npa[vetQuad[k]]<=nppq) {
          if (vetQuad[k]==1) {index1[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==2) {index2[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==3) {index3[npa[vetQuad[k]]]=vetRelacao[k]}
          if (vetQuad[k]==4) {index4[npa[vetQuad[k]]]=vetRelacao[k]}
          }
      }
      #verifica o numero de pontos knt nos quadrantes
      knt=0
      for (k in 1:nppq){
        if (index1[k] > 0){knt=knt+1}
        if (index2[k] > 0){knt=knt+1}
        if (index3[k] > 0){knt=knt+1}
        if (index4[k] > 0){knt=knt+1}
      }
      #indices=localizacao dos pontos nos dados de entrada
      indices=c(rep(0,knt))
      knt=0
      for (k in 1:nppq){
        if (index1[k] > 0) {
          knt=knt+1
          indices[knt]=index1[k]}
      }
      for (k in 1:nppq){
        if (index2[k] > 0) {
          knt=knt+1
          indices[knt]=index2[k]}
     }
      for (k in 1:nppq){
        if (index3[k] > 0) {
          knt=knt+1
          indices[knt]=index3[k]}
      }
      for (k in 1:nppq){
        if (index4[k] > 0) {
          knt=knt+1
          indices[knt]=index4[k]}
      }
return(indices)
}