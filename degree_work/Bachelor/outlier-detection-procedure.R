########################################################
############# Algoritmos ###############################
########################################################

#* Outlier.Dir_out

Outlier.DirOut <- function(fdataobj,nb=200,quan=0.5,l=4,ns=0.99,dfunc='RP',boot=MBBo.DirOut) 
{
  if (!is.fdata(fdataobj)) 
    fdataobj <- fdata(fdataobj)
  nas1 <- is.na(fdataobj)
  if (any(nas1)) 
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  x <- fdataobj[["data"]]
  tt <- fdataobj[["argvals"]]
  rtt <- fdataobj[["rangeval"]]
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)            
  if (is.null(n) && is.null(m)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  if (is.null(row.names(fdataobj[["data"]]))) 
    row.names(fdataobj[["data"]]) = 1:n
  
  boot_obj <- boot(fdataobj,nb = nb, smo = smo,ns=ns)
  cutoff_avr <- quantile(boot_obj$q_avr,probs = quan)
  cutoff_var <- quantile(boot_obj$q_var,probs = quan)
  
  outliers <- dep.out_avr <- dep.out_var <- ite <- c()
  curvasgood <- fdataobj
  
  dirout_obj <- DirOut(curvasgood[['data']])
  d_avr <- dirout_obj$out_avr
  d_var <- dirout_obj$out_var
  
  #Magnitud 
  
  cutt <- cutoff_avr < d_avr
  fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
  elim <- which(cutt)
  if (length(elim) > 0) {
    dep.out_avr <- c(dep.out_avr, d_avr[elim])
    outliers <- c(outliers, fecha)
  }
  
  #Forma 
  
  cutt <- cutoff_var < d_var
  fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
  elim <- which(cutt)
  if (length(elim) > 0) {
    dep.out_var <- c(dep.out_var, d_var[elim])
    outliers <- c(outliers, fecha)
  }
  
  outliers <- rownames(fdataobj[["data"]])[unique(outliers)]
  names(dep.out_var) <- names(dep.out_avr) <- NULL
  return(list(outliers = as.numeric(outliers),dep.out_avr=dep.out_avr,dep.out_var=dep.out_var, q_avr = cutoff_avr, q_var = cutoff_var))
}

#* Outlier.boot

Outlier.boot <- function(fdataobj, nb = 200, smo = 0.05, quan=0.5,
                         dfunc = MBD,l=4,p=0.1,ns=0.01,boot=SmBoD) 
{
  if (!is.fdata(fdataobj)) 
    fdataobj <- fdata(fdataobj)
  nas1 <- is.na.fdata(fdataobj)
  if (any(nas1)) 
    stop("fdataobj contain ", sum(nas1), " curves with some NA value \n")
  x <- fdataobj[["data"]]
  tt <- fdataobj[["argvals"]]
  rtt <- fdataobj[["rangeval"]]
  n <- nrow(fdataobj)
  m <- ncol(fdataobj)
  if (is.null(n) && is.null(m)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  if (is.null(row.names(fdataobj[["data"]]))) 
    row.names(fdataobj[["data"]]) = 1:n
  
  cutoff <- quantile(boot(fdataobj, dfunc = dfunc,nb = nb, smo = smo,ns=ns),probs = quan)
  
  hay <- 1
  outliers <- dep.out <- ite <- c()
  ii <- 1
  curvasgood <- fdataobj
  
  d <- dfunc(curvasgood[["data"]])
  
  rwn = names(d) = rownames(curvasgood[["data"]]) = 1:n
  while (hay == 1) {
    if (is.null(outliers)) {
      dtotal <- d
    }
    cutt <- d < cutoff
    fecha <- as.numeric(rownames(curvasgood[["data"]])[cutt])
    elim <- which(cutt)
    if (length(elim) > 0) {
      dep.out <- c(dep.out, d[elim])
      curvasgood <- curvasgood[-elim, ]
      outliers <- c(outliers, fecha)
    }
    if (length(elim) == 0 || length(outliers) > n/5) {
      hay <- 0
    }
    else {
      d <- dfunc(curvasgood[["data"]])
    }
    ite <- c(ite, rep(ii, length(elim)))
    ii <- ii + 1
  }
  outliers <- rownames(fdataobj[["data"]])[outliers]
  names(dep.out) <- NULL
  return(list(outliers = as.numeric(outliers),dep.out=dep.out ,iteration = ite, 
              quantile = cutoff, Dep = dtotal))
}