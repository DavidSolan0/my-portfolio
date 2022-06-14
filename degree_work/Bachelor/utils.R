#############################################################
############### Funciones ###################################
#############################################################

#* Uniforme discretra 

runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=TRUE)

#* Paso 1 algoritmo estimación cutoff

step1 <- function(fdataobj,depths)
{
  o <- fda::fbplot(t(fdataobj$data),plot=FALSE,depth = depths)$outpoint
  
  if(length(o)!=0)
    fdataobj[["data"]] <- fdataobj[["data"]][-o,]
  
  return(fdataobj)
}

#* Curvas del bloque i

Bi <- function(i,l) i:(i+l-1)

#* Modulo 

B <- function(i,n,l){
  vector <-  i:(i+l-1)%%n
  mod <- vector%%n
  cam <- mod==0
  vector[cam] <- n
  vector
}


multistep1 <- function(x,depths){
  
  procesos <- x  
  dim <- length(procesos)
  
  o = NULL
  for(i in 1:dim){
    o0 <- fda::fbplot(t(procesos[[i]]),plot=FALSE,depth = depths)$outpoint
    o <- unique(c(o0,o))
  }
  
  o <- unique(o)
  
  if(length(o)!=0){
    for(i in 1:dim) procesos[[i]] <- procesos[[i]][-o,]
  }
  return(procesos)
}
