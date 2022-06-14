################################################
################ MD ############################
################################################

MD <- function (fdataobj, fdataori = fdataobj, trim = 0, metric = metric.lp, 
                h = NULL, scale = FALSE, draw = FALSE) 
{
  if (!is.fdata(fdataobj)) 
    fdataobj <- fdata(fdataobj)
  if (is.fdata(fdataobj)) {
    fdat <- TRUE
    if (is.null(rownames(fdataobj$data))) 
      rownames(fdataobj$data) <- 1:nrow(fdataobj$data)
    nms <- rownames(fdataobj$data)
    m0 <- nrow(fdataobj)
    fdataobj <- na.omit.fdata(fdataobj)
    fdataori <- na.omit.fdata(fdataori)
    nas <- na.action(fdataobj)
    nullans <- !is.null(nas)
    data <- fdataobj[["data"]]
    data2 <- fdataori[["data"]]
    names1 <- names2 <- names <- fdataobj[["names"]]
    names1$main <- "depth.mode median"
    names2$main <- paste("depth.mode trim ", trim * 100, 
                         "%", sep = "")
    tt = fdataobj[["argvals"]]
    rtt <- fdataobj[["rangeval"]]
  }
  else {
    stop("no fdata class object")
    data <- fdataobj
    data2 <- fdataori
    fdat <- FALSE
  }
  n <- nrow(data)
  m <- ncol(data)
  m2 <- ncol(data2)
  n2 <- nrow(data2)
  if (is.null(n) && is.null(m)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  if (is.null(m) && is.null(m2)) 
    stop("ERROR IN THE DATA DIMENSIONS")
  if (is.matrix(metric)) 
    mdist = metric
  else mdist = metric(fdataori, fdataori)
  class(mdist) <- "matrix"
  if (n == n2 & m == m2) {
    equal <- all(fdataobj$data == fdataori$data)
    if (equal) 
      mdist2 <- mdist
    else mdist2 <- metric(fdataobj, fdataori)
  }
  else mdist2 <- metric(fdataobj, fdataori)
  if (is.null(h)) {
    h <- 0.15
    hq2 = quantile(mdist, probs = h, na.rm = TRUE)
  }
  else {
    if (is.numeric(h)) 
      hq2 <- h
    else hq2 = quantile(mdist, probs = as.numeric(substring(h, 
                                                            first = 3)), na.rm = TRUE)
  }
  class(mdist) <- class(mdist2) <- c("matrix", "fdist")
  ans <- Ker.norm(mdist2/hq2)
  ans <- apply(ans, 1, sum, na.rm = TRUE)
  if (scale) {
    mn <- min(ans, na.rm = TRUE)
    mx <- max(ans, na.rm = TRUE)
    ans = as.vector(ans/mx)
  }
  k = which.max(ans)
  med = data[k, ]
  lista = which(ans >= quantile(ans, probs = trim, na.rm = T))
  if (nullans) {
    ans1 <- rep(NA, len = m0)
    ans1[-nas] <- ans
    ans <- ans1
  }
  names(ans) <- nms
  if (length(lista) == 1) {
    mtrim <- data[lista, ]
    if (draw) {
      draw = FALSE
      warning("The plot is not shown")
    }
  }
  else mtrim = apply(fdataobj[lista]$data, 2, mean, na.rm = TRUE)
  tr <- paste("mode.tr", trim * 100, "%", sep = "")
  if (fdat) {
    med <- fdata(med, tt, rtt, names1)
    mtrim <- fdata(mtrim, tt, rtt, names2)
    rownames(med$data) <- "mode.med"
    rownames(mtrim$data) <- tr
    if (draw) {
      if (!scale) {
        mn <- min(ans, na.rm = TRUE)
        mx <- max(ans, na.rm = TRUE)
        scl <- mx - mn
      }
      ind1 <- !is.nan(ans)
      ans[is.nan(ans)] = NA
      cgray = 1 - (ans - mn)/(scl)
      plot(fdataori, col = gray(0.9), lty = 1, main = "mode Depth")
      lines(fdataobj[ind1, ], col = gray(cgray[ind1]))
      lines(mtrim, lwd = 2, col = "yellow")
      lines(med, col = "red", lwd = 2)
      legend("topleft", legend = c(tr, "Median"), lwd = 2, 
             col = c("yellow", "red"), box.col = 0)
    }
  }
  out <- list(median = med, lmed = k, mtrim = mtrim, ltrim = lista, 
              dep = ans, hq = hq2)
  if (scale) 
    out$dscale = mx
  return(dep=ans)
}

###############################################
################ MFD ##########################
###############################################

MFD<- function (mfdataobj, z = NULL, type = "hdepth", alpha = 0, time = NULL, 
                diagnostic = FALSE, depthOptions = NULL,weights) 
{
  if(is.matrix(mfdataobj)){
    n <- nrow(mfdataobj)
    m <- ncol(mfdataobj)
    x <- array(dim= c(m,n,1))
    x[,,1] <- t(mfdataobj)
  }
  
  if(is.list(mfdataobj)){
    dim <- length(mfdataobj)
    n <- nrow(mfdataobj[[1]])
    m <- ncol(mfdataobj[[1]])
    x <- array(dim= c(m,n,dim))
    for(i in 1:dim) x[,,i] <- t(mfdataobj[[i]])
  }
  
  
  if (missing(x)) {
    stop("Input argument x is required.")
  }
  if (!is.array(x)) {
    stop("x must be a three dimensional array.")
  }
  if (length(dim(x)) != 3) {
    stop("x must be a three dimensional array.")
  }
  if (sum(is.nan(x)) != 0) {
    stop("x contains missing cases.")
  }
  t1 <- dim(x)[1]
  n1 <- dim(x)[2]
  p1 <- dim(x)[3]
  if (is.null(z)) {
    z <- x
  }
  if (!is.array(z)) {
    stop("z must be a three dimensional array.")
  }
  if (length(dim(z)) != 3) {
    stop("z must be a three dimensional array.")
  }
  if (sum(is.nan(z)) != 0) {
    stop("z contains missing cases.")
  }
  t2 <- dim(z)[1]
  n2 <- dim(z)[2]
  p2 <- dim(z)[3]
  if (p1 != p2) {
    stop("The p dimension of x and z must match.")
  }
  if (t1 != t2) {
    stop("The t dimension of x and z must match.")
  }
  Indtype <- match(type, c("hdepth", "projdepth", "sprojdepth", 
                           "dprojdepth", "sdepth"))[1]
  if (is.na(Indtype)) {
    stop("type should be one of hdepth, projdepth , sprojdepth, dprojdepth or sdepth.")
  }
  if (Indtype == 5 && p1 > 2) {
    stop("sdepth depth only implemented for p<=2.")
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric")
  }
  if (is.vector(alpha)) {
    if (alpha < 0) {
      stop("alpha should be part of [0,1]")
    }
    if (alpha > 1) {
      stop("alpha should be part of [0,1]")
    }
  }
  else if (is.matrix(alpha)) {
    NRowAlpha <- dim(alpha)[1]
    NColAlpha <- dim(alpha)[2]
    if (NRowAlpha != 1 || NColAlpha != t1) {
      stop("alpha must be a (1xt)-row matrix.")
    }
  }
  else {
    stop("alpha must be either a number or a (1xt)-row matrix.")
  }
  if (is.null(time)) {
    time <- 1:t1
  }
  if (!is.numeric(time) || !is.vector(time)) {
    stop("time should be a numeric vector.")
  }
  if (length(time) != t1) {
    stop("time should contain t elements")
  }
  if (length(time) != 1) {
    dTime <- diff(c(time[1], time, time[t1]), lag = 2)
    if (min(dTime) <= 0) {
      stop("time should be strictly increasing.")
    }
  }
  else {
    dTime <- 1
  }
  if (!is.logical(diagnostic)) {
    stop("diagnostic should be a logical")
  }
  if (is.null(depthOptions)) {
    depthOptions <- list()
  }
  if (!is.list(depthOptions)) {
    stop("depthOptions must be a list")
  }
  weights <- rep(1, t1)
  depthsTimeX <- matrix(NA, nrow = n1, ncol = t1)
  depthsTimeZ <- matrix(0, nrow = n2, ncol = t2)
  locOutlX <- matrix(NA, nrow = n1, ncol = t1)
  locOutlZ <- matrix(NA, nrow = n1, ncol = t1)
  if (is.matrix(alpha)) {
    weights <- alpha
  }
  warningFlagFit <- warningFlagBag <- warningFlagIso <- warningFlagAlpha <- 0
  warningIndFit <- warningIndBag <- warningIndIso <- warningIndAlpha <- c()
  Original <- options(warn = -1)
  for (j in 1:t1) {
    exactfit <- 0
    if (p1 == 1) {
      xTimePoint <- matrix(x[j, , 1])
      zTimePoint <- matrix(z[j, , 1])
    }
    else {
      xTimePoint <- x[j, , , drop = TRUE]
      zTimePoint <- z[j, , , drop = TRUE]
    }
    if (type == "hdepth") {
      temp <- hdepth(x = xTimePoint, z = zTimePoint, options = depthOptions)
      if (!is.list(temp)) {
        temp <- list()
      }
      if (!is.null(temp$depthZ)) {
        depthsTimeX[, j] <- temp$depthZ
        depthsTimeZ[, j] <- temp$depthZ
      }
      else {
        exactfit <- 1
      }
      if (diagnostic & p1 == 2 & exactfit == FALSE) {
        temp <- compBagplot(x = xTimePoint, type = type)
        if (sum(is.nan(temp$flag)) == 0) {
          locOutlX[, j] <- temp$flag
        }
        else {
          warningFlagBag <- 1
          warningIndBag <- c(warningIndBag, j)
        }
      }
    }
    else if (type == "projdepth") {
      temp <- projdepth(x = xTimePoint, z = zTimePoint, 
                        options = depthOptions)
      if (!is.null(temp$depthZ)) {
        depthsTimeX[, j] <- temp$depthX
        depthsTimeZ[, j] <- temp$depthZ
        locOutlX[, j] <- as.numeric(!temp$flagX)
        locOutlZ[, j] <- as.numeric(!temp$flagZ)
      }
      else {
        exactfit <- 1
      }
    }
    else if (type == "sprojdepth") {
      temp <- sprojdepth(x = xTimePoint, z = zTimePoint, 
                         options = depthOptions)
      if (!is.null(temp$depthZ)) {
        depthsTimeX[, j] <- temp$depthX
        depthsTimeZ[, j] <- temp$depthZ
        locOutlX[, j] <- as.numeric(!temp$flagX)
        locOutlZ[, j] <- as.numeric(!temp$flagZ)
      }
      else {
        exactfit <- 1
      }
    }
    else if (type == "dprojdepth") {
      temp <- dprojdepth(x = xTimePoint, z = zTimePoint, 
                         options = depthOptions)
      if (!is.null(temp$depthZ)) {
        depthsTimeX[, j] <- temp$depthX
        depthsTimeZ[, j] <- temp$depthZ
        locOutlX[, j] <- as.numeric(!temp$flagX)
        locOutlZ[, j] <- as.numeric(!temp$flagZ)
      }
      else {
        exactfit <- 1
      }
    }
    else {
      temp <- sdepth(x = xTimePoint, z = zTimePoint)
      if (!is.null(temp$depth)) {
        depthsTimeZ[, j] <- temp$depth
      }
      else {
        exactfit <- 1
      }
    }
    if (exactfit) {
      weights[j] <- 0
      warningFlagFit <- 1
      warningIndFit <- c(warningIndFit, j)
    }
    if (!is.matrix(alpha)) {
      if (alpha != 0 && exactfit == 0) {
        temp <- depthContour(x = xTimePoint, alpha, type = type)
        Vert <- temp[[1]]$vertices
        if (sum(is.nan(Vert)) == 0) {
          if (nrow(Vert) != nrow(unique(Vert))) {
            warningFlagIso <- 1
            warningIndIso <- c(warningIndIso, j)
          }
          else {
            if (p1 == 1) {
              temp <- max(Vert) - min(Vert)
            }
            else {
              temp <- try(convhulln(matrix(Vert, ncol = p1), 
                                    "FA")$vol, silent = TRUE)
            }
            if (!is.numeric(temp)) {
              warningFlagAlpha <- 1
              warningIndAlpha <- c(warningIndAlpha, j)
            }
            else {
              weights[j] <- temp
            }
          }
        }
        else {
          weights[j] <- 0
          warningFlagIso <- 1
          warningIndIso <- c(warningIndIso, j)
        }
      }
    }
  }
  options(warn = Original$warn)
  weights <- weights * dTime
  weights <- weights/sum(weights)
  depthsX <- depthsTimeX %*% weights
  depthsZ <- depthsTimeZ %*% weights
  Result <- list(MFDdepthX = depthsX, MFDdepthZ = depthsZ, 
                 weights = weights)
  if (diagnostic) {
    Result$crossdepthX <- depthsTimeX
    Result$crossdepthZ <- depthsTimeZ
    Result$locOutlX <- locOutlX
    Result$locOutlZ <- locOutlZ
  }
  Result$depthType <- type
  class(Result) <- c("mrfDepth", "mfd")
  if (warningFlagFit == 1) {
    warning(paste("Exact fits were detected for certain time points.", 
                  "Their weights will be set to zero.", "Check the returned results"), 
            call. = FALSE)
    Result$IndFlagExactFit <- warningIndFit
  }
  if (warningFlagBag == 1) {
    warning(paste("The bagplot could not be computed at all time points.", 
                  "Their weights will be set to zero.", "Check the returned results"), 
            call. = FALSE)
    Result$IndFlagBag <- warningIndBag
  }
  if (warningFlagIso == 1) {
    warning(paste("The isohdepth contours could not be computed at all", 
                  "time points. Their weights will be set to zero.", 
                  "Check the returned results"), call. = FALSE)
    Result$IndFlagIso <- warningIndIso
  }
  if (warningFlagAlpha == 1) {
    warning(paste("The specified alpha is too large at all time points.", 
                  "Their weights will be set to zero.", "Check the returned results"), 
            call. = FALSE)
    Result$IndFlagAlpha <- warningIndAlpha
  }
  return(as.vector(depthsX))
}