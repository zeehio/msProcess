################################################
## S+Proteome peak quantification functions.
##  
##  msQuantify
##  msQuantifyIntensity
##  msQuantifyCount
##  
################################################

###
# msQuantify
###

"msQuantify" <- function(x, xnew=NULL, measure="intensity")
{
  if (is.null(x$peak.class)){
    stop("The input has no peak.class attribute, ",
      "which can be generated by running msAlign() or",
      "msPeak() with use.mean=TRUE.\n")
  }
	
  z <- switch(match.arg(measure, choices=c("intensity", "count")),
    intensity = msQuantifyIntensity(x, xnew),
    count     = msQuantifyCount(x, xnew))
    
  # append history event
  eventHistory(z, "Peak Quantification"=list(xnew=!is.null(xnew), measure=measure))    
}

###
# msQuantifyCount
###

"msQuantifyCount" <- function(x, xnew=NULL)
{
  # output counts of peaks
  peak.class <- x$peak.class
  n.class    <- nrow(peak.class)

  if (is.null(xnew)) {
    intensity <- x$intensity
    peak.list <- x$peak.list    
  }
  else {
    intensity <- xnew$intensity
    peak.list <- xnew$peak.list    
  }
	
  if (is.null(peak.list))
    stop("The input has no peak.list attribute,",
      "which can be generated by running msPeak() with use.mean=FALSE.\n")
  
  n.spectra <- ncol(intensity)
  nmz       <- nrow(intensity)
	
  # count peaks for each peak class
  peak.logic <- matrix(0, nrow=nmz, ncol=n.spectra)

  for (i in seq(n.spectra)){
    peak.logic[peak.list[[i]][, "tick.loc"], i] <- 1
  }

  peak.matrix <- apply(matrix(1:n.class), 1,
    function(i, peak.logic, peak.class)
      apply(peak.logic[peak.class[i, "tick.left"]:peak.class[i, "tick.right"],,drop=FALSE], 2, sum), 
        peak.logic=peak.logic, peak.class=peak.class)
  dimnames(peak.matrix)[[2]] <- peak.class[, "mass.loc"]
  dimnames(peak.matrix)[[1]] <- dimnames(intensity)[[2]]
	
  attr(peak.matrix, "measure") <- "peak count"
  
  # assign peak class to each peak
  class.logic <- rep(0, nmz)
  for (i in 1:numRows(peak.class)) {
    class.logic[peak.class[i, "tick.left"]:peak.class[i, "tick.right"]] <- i
  }
	
  # update peak.list	
  class.id <- peak.logic * class.logic	

  for (i in seq(n.spectra)){
    peak.list[[i]][, "class.id"] <- class.id[peak.list[[i]][, "tick.loc"], i]
  }	

  msSet(if (is.null(xnew)) x else xnew, 
    peak.list=peak.list, peak.matrix=peak.matrix)
}

###
# msQuantifyIntensity
###

"msQuantifyIntensity" <- function(x, xnew=NULL)
{
  # output intensity of peaks
  peak.class <- x$peak.class
  n.class    <- nrow(peak.class)
  
  if (is.null(xnew)) intensity <- x$intensity
  else intensity <- xnew$intensity
	
  peak.matrix <- apply(matrix(1:n.class), 1,
    function(i, intensity, peak.class)
      apply(intensity[peak.class[i, "tick.left"]:peak.class[i, "tick.right"],,drop=FALSE], 2, max),
        intensity=intensity, peak.class=peak.class)
  dimnames(peak.matrix)[[2]] <- peak.class[, "mass.loc"]
	
  attr(peak.matrix, "measure") <- "spectrum intensity"

  # add peak.matrix to the msSet object
  msSet(if (is.null(xnew)) x else xnew, peak.matrix=peak.matrix)
}

