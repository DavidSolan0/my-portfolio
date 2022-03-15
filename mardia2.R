mardia2<-function(datos){
  n<-nrow(datos)
  k<-ncol(datos)
  
  x<-as.matrix(datos)
  media<-colMeans(datos)
  matriz.media<-matrix(media,nrow=n,ncol=k,byrow=TRUE)
  
  S<-cov(datos)*(n-1)/n
  SI<-solve(S)   #Inversa de la matriz de covarianzas
  
  distm<-((x-matriz.media)%*%SI)%*%t((x-matriz.media))
  
  gl<-k*(k+1)*(k+2)/6
  
  # Medida de Asimetría Multivariada
  AM<-sum(distm^3)/n^2
  
  if (n<30){
    # Test Multivariado de Asimetría de Mardia - Muestras Pequeñas
    c<-(n+1)*(n+3)*(k+1)/(n*((n+1)*(k+1)-6))
    TAM<-n*c/6*AM           
    valor_p1<-pchisq(TAM,gl,lower.tail = FALSE)
  } else { 
    # Test Multivariado de Asimetríia de Mardia - Muestras grandes
    TAM<-n/6*AM           
    valor_p1<-pchisq(TAM,gl,lower.tail = FALSE)
  }
  #Medida de Kurtosis Multivariado
  KM<-sum((diag(distm))^2)/n
  
  #Test Multivariado de Kurtosis de Mardia
  TKM<-(KM-k*(k+2))*sqrt(n/(8*k*(k+2)))
  TKM2<-TKM^2
  valor_p2<-pchisq(TKM2,1,lower.tail = FALSE)
  
  cat("Test de Normalidad Multivariada", "\n" ,
      "Ho: Los datos siguen una Distribución Normal Multivariada","\n")
  cat("----------------------------------------------------------------","\n")
  cat("Prueba de Mardia - Asimetría","\n", 
      "Asimetría Multivariada =", AM, "\n",
      "Valor de TAM =",TAM, "\n", 
      "Valor p=", valor_p1, "\n")
  cat("----------------------------------------------------------------","\n")
  cat("Prueba de Mardia - Curtosis","\n", 
      "Curtosis Multivariado =", KM, "\n",
      "Valor de TKM =",TKM2, "\n", 
      "Valor p=", valor_p2, "\n")
}