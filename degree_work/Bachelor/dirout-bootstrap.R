#*MBBo.Dir_Out

MBBo.DirOut <- function(x,l=4,nb=200,ns=0.99,smo=0,dfunc='RP'){
  
  n <- nrow.fdata(x)
  k = ceiling(n/l)
  
  muestra.trim <- x[['data']] 
  
  i= 1:(n-l+1)
  
  cuantiles.avr <- cuantiles.var  <- numeric(nb)                                  

  for (j in 1:nb){ 
    
    I <- runifdisc(k,min=min(i),max=max(i))
    
    bmuestra.ind <- c()
    
    for(m in 1:k){
      bmuestra.ind <- c(bmuestra.ind,Bi(I[m],l=l))
    }
    
    bmuestra.ind <- bmuestra.ind[1:n]  
    bmuestra <- muestra.trim[bmuestra.ind, ]
    obj <- DirOut(bmuestra)
    d.avr <- obj$out_avr
    d.var <- obj$out_var
    
    cuantiles.avr[j] <- quantile(d.avr, probs = ns,type=8)
    cuantiles.var[j] <- quantile(d.var, probs = ns,type=8)
  }
  return(list(q_avr = cuantiles.avr,q_var = cuantiles.var))
}

#* StBo Dir_Out

StBo_DirOut <- function(x,p=0.1,nb=200,dfunc='RP',ns=0.01,smo=0){
  
  n  <- nrow.fdata(x)
  muestra.trim <- x[['data']]
  
  i= 1:n
  min = 1; max = n
  
  cuantiles.avr <- cuantiles.var <- numeric(nb)                                  
  
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
      Bi <- c(Bi,B(I[m],n,lengths[m]))
    }
    
    bmuestra <- muestra.trim[Bi, ]
    
    obj <- DirOut(bmuestra)
    d.avr <- obj$out_avr
    d.var <- obj$out_var
    
    cuantiles.avr[j] <- quantile(d.avr, probs = ns,type=8)
    cuantiles.var[j] <- quantile(d.var, probs = ns,type=8)
  }
  return(list(q_avr = cuantiles.avr,q_var = cuantiles.var))
}
