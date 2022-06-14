##############################################################
########## Bootstraps ########################################
##############################################################

#* SmBoD 

SmBoD<- function (x, dfunc = MBD, nb = 200, smo = 0.05, ns = 0.01) 
{
  if (!is.fdata(x)) 
    x <- fdata(x)
  
  dat <- x[["data"]]
  tt <- x[["argvals"]]
  rtt <- x[["rangeval"]]
  n <- nrow(x)
  m <- ncol(x)
  
  if (is.null(n) && is.null(m)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  
  depths <- dfunc(x[["data"]])
  muestra.trim <- step1(x,depths)
  nn <- nrow(muestra.trim)
  cuantiles <- numeric(nb)
  vv = var(dat)
  for (i in 1:nb) {
    bmuestra <- muestra.trim[sample(1:nn, size = n, 
                                    replace = TRUE), ]
    
    bmuestra[["data"]] <- bmuestra[["data"]] + mvrnorm(n = n, 
                                                       rep(0, m), vv * smo)
    
    d   <- dfunc(bmuestra[["data"]])
    cuantiles[i] <- quantile(d, probs = ns,type=8)
  }
  return(cuantiles)
}

#MBBo 

MBBo <- function(x,l=4,nb=200,dfunc=MBD,ns=0.01,smo=0){
  
  n <- nrow.fdata(x)
  k = ceiling(n/l)
  
  depths <- dfunc(x[["data"]])
  muestra.trim <- step1(x,depths)
  nn <- nrow.fdata(muestra.trim)
  
  i= 1:(nn-l+1)
  
  cuantiles <- numeric(nb)                                  
  
  for (j in 1:nb){ 
    
    I <- runifdisc(k,min=min(i),max=max(i))
    
    bmuestra.ind <- c()
    
    for(m in 1:k){
      bmuestra.ind <- c(bmuestra.ind,Bi(I[m],l=l))
    }
    
    bmuestra.ind <- bmuestra.ind[1:n]  
    bmuestra <- muestra.trim[bmuestra.ind, ]
    d <- dfunc(bmuestra[["data"]])
    
    cuantiles[j] <- quantile(d, probs = ns,type=8)
    
  }
  return(cuantiles)
}

#* StBo 

StBo <- function(x,p=0.1,nb=200,dfunc=MBD,ns=0.01,smo=0){
  
  n  <- nrow.fdata(x)
  depths <- dfunc(x[["data"]])
  
  muestra.trim <- step1(x,depths)
  nn <- nrow.fdata(muestra.trim)
  
  i= 1:nn
  min = 1; max = nn
  cuantiles <- numeric(nb)
  
  for (j in 1:nb){
    
    lengths <- c();i = 1
    while(sum(lengths)<n)
    {
      lengths[i] <- rgeom(1,prob=p) + 1
      i = i + 1
    }
    
    l.ls <- length(lengths)
    
    I <- runifdisc(l.ls,min=min,max=max)
    
    Bi<- c()
    for(m in 1:l.ls)
    {
      Bi <- c(Bi,B(I[m],nn,lengths[m]))
    }
    
    bmuestra <- muestra.trim[Bi, ]
    d <- dfunc(bmuestra[["data"]])
    
    cuantiles[j] <- quantile(d, probs = ns,type=8)
    
  }
  return(cuantiles)
}
