################################################
## Class msQualify
## Constructor function: msQualify
## Methods:
##
##  predict.msQualify
##
################################################

###
# msQualify
###

"msQualify" <- function(x, FUN="princomp", ...)
{
  # principal component analysis
  d <- dim(x)
	n <- d[1]	
	p <- d[2]
		
  FUN <- if (is.R()) "princomp" else match.arg(FUN, choices=c("princomp", "princompRob"))
  x.prc <- switch(FUN,
    princomp    = if (!is.R() || n>p) princomp(x, ...) else  princomp2(x, ...),
    princompRob	= princompRob(x, ...))

  # construct return value
  z <- list(prc=x.prc)
  oldClass(z) <- "msQualify"

  # record history event
  z <- eventHistory(z, "Quality Assessment"=
   list(process="msQualify"))

  z
}

###
# predict.msQualify
###

"predict.msQualify" <- function(object, newdata, criterion="cattell", threshold=0.9, ...)
{
  # decide how many PC's to keep, see S-PLUS 7 Guide to Statistics, Vol.2
  criterion <- match.arg(lowerCase(criterion), choices=c("cattell", "kaiser"))
  prc.eigen <- object$prc$sdev^2
  n.keep <- switch(criterion,
    cattell = sum(cumsum(prc.eigen)/sum(prc.eigen) < threshold) + 1,
    kaiser  = sum(prc.eigen >= mean(prc.eigen)))

  # project the new data to the PCA space, see S-PLUS 7 Guide to Statistics, Vol.2
  newdata.projected <- if (!is.R()) predict.princomp(object$prc, newdata=newdata)
    else stats:::predict.princomp(object$prc, newdata=newdata)
    
  # compute mahalanobis distance from the projected new data
  # to the center of the reduced PCA space
  newdata.dist <- mahalanobis(x=newdata.projected[,1:n.keep],
    center=rep(0,n.keep), cov=diag(object$prc$sdev[1:n.keep]^2))

  # compute the threshold distance and perform test
  threshold.dist <- qchisq(p=threshold, df=n.keep)
  pass <- (newdata.dist < threshold.dist)

  # construct return value
  list(dist=newdata.dist, df=n.keep, pass=pass)
}
