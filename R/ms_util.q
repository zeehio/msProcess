################################################
## msProcess package utility functions
##
##  argNames
##  matchObject
##  msExtrema
##  msPlot
##	princomp2
##  rescale
##  zeroCross
##
################################################

###
# argNames
###

"argNames" <- function(x)
{
  if (is.R()){

    formalArgs(x)

  } else{

    switch(mode(x),
  	"function" = names(x)[ - length(x)],
  	call = names(x)[-1],
  	character = {
  		x <- getFunction(x)
  		names(x)[ - length(x)]
  	}
  	,
  	stop("mode must be 'function', 'call', or 'character'"))
  }
}

###
# matchObject
###

"matchObject" <- function(what, ignore.case=TRUE)
{
  for (i in seq(along=search())){

   objs <- ls(, i)
   imatch <- grep(pattern=paste("^", what, "$", sep=""), objs, ignore.case=ignore.case)
   if (length(imatch))
     return(objs[imatch])
  }
  stop("No matching object found in any database on the search() path")
  invisible(NULL)
}

###
# msExtrema
###

"msExtrema" <- function(x, span=3)
{
  # find local maxima
  index1 <- peaks(x, span=span, strict=FALSE)

  # find local minima
  index2 <- peaks(-x, span=span, strict=FALSE)

  # remove the interior of plateaus
  index.max <- index1 & !index2
  index.min <- index2 & !index1

  # construct output
  list(index.max=index.max, index.min=index.min)
}

###
# msPlot
###

"msPlot" <- function(matlines.=NULL, matpoints.=NULL, lines.=NULL, points.=NULL,
  image.=NULL, abline.=NULL, text.=NULL, offset=NULL, recenter=FALSE,
  offset.fun=function(x){3 * stdev(x, na.rm=TRUE)},
  xlab="", ylab="", yref=TRUE, main="", add=FALSE, ...)
{
  # define local functions
  "checkArgStruct" <- function(x){

    vars    <- list(xy=c("x","y"), xyz=c("x","y","z"), hv=c("h","v"))
    reqargs <- list(matlines=vars$xy, matpoints=vars$xy, lines=vars$xy,
      points=vars$xy, abline=vars$hv, text=vars$xy, image=vars$xyz)

    # validate structure of each data set
    for (fun in names(x)){

      if (is.element(fun,"abline")) comp <- any
      else comp <- all

      data <- x[[fun]]
      req  <- reqargs[[fun]]

      if (!is.list(data))
        stop(fun, " must be a list")

      # data could be a single list like:
      #
      #   z <- list(x=matrix, y=matrix, ...)
      #
      # or it could be a list of lists like:
      #
      #   zz <- list(z, z)
      #
      # we need to probe all the way down to the matrix level

      if (is.list(data[[1]])){

        pass <- unlist(lapply(data,
          function(x, req, comp){
            (comp(is.element(req, names(x))) & is.list(x))
          }, req=req, comp=comp ))

      }
      else pass <- (comp(is.element(req, names(data))) & is.list(data))

      if (any(!pass))
        stop(fun, " must be a list containing ", switch(fun, abline="one","all"),
          " of the named objects: ",  paste(req, collapse=" "))
    }

    invisible(NULL)
  }

  "transformXY" <- function(obj, recenter, offset, is.scaled)
  {
    # force x and y objects to be matrices.
    # recenter y values if requested.
    # offset y data if requested.

    # initialize variables
    z <- list(x=NULL,y=NULL)

    # force vectors and matrices with
    # a single row into a single column matrix
    if (isVectorAtomic(obj$x))
      z$x <- matrix(obj$x, ncol=1)
    else
      z$x <- as.matrix(obj$x)
    if (isVectorAtomic(obj$y))
      z$y <- matrix(obj$y, ncol=1)
    else
      z$y <- as.matrix(obj$y)

    # recenter y data if requested
    if (recenter)
      z$y <- sweep(z$y, MARGIN=2, colMins(z$y))

    # offset y data if requested
    if (is.scaled && offset > 0){

      yoff <- offset * seq(0, numCols(z$y) - 1)
      z$y  <- sweep(z$y, MARGIN=2, yoff, FUN="+")
    }

    z
  }

  # collect data into groups
  scaled   <- list(matlines=matlines., matpoints=matpoints.)
  unscaled <- list(lines=lines., points=points.)
  other    <- list(abline=abline., image=image., text=text.)

  # initialize variables
  exist.scaled   <- !unlist(lapply(scaled, is.null))
  exist.unscaled <- !unlist(lapply(unscaled, is.null))
  exist.other    <- !unlist(lapply(other, is.null))
  is.scaled      <- any(exist.scaled)
  is.unscaled    <- any(exist.unscaled)
  is.other       <- any(exist.other)

  # concatenate data lists
  if (is.scaled)
    data <- scaled[exist.scaled]
  else
    data <- unscaled[exist.unscaled]
  if (is.other)
    data <- c(data, other[exist.other])

  # append add to the image function
  if (is.element("image", names(data)))
    data$image$add <- TRUE

  # check input arguments
  if (is.scaled && is.unscaled)
    stop("Cannot mixed scaled and unscaled plot types")
  if (!is.scaled && !is.unscaled)
    return(invisible(NULL)) # do nothing if no data supplied
  if (!is.function(offset.fun))
    stop("offset.fun input must be a function")
  checkArgStruct(data)

  # define offset
  if (is.scaled && is.null(offset)){
    flatdata <- unlist(data)
    nms   <- names(flatdata)
    token <- "^(matlines|matpoints|lines|points|abline|image|text).y"
    ydata <- flatdata[if (is.R()) grep(token, nms) else regMatch(nms, token)]

    if (length(ydata))
      offset <- offset.fun(as.numeric(ydata))
    else
      offset <- 0
  }

  # initialize variables
  xlim <- ylim <- NULL
  xy   <- c("x","y")
  funs <- names(data)

  # transform XY data
  for (i in seq(along=data)){

    scale.data <- any(is.element(funs[i], names(scaled)))

    ## TODO: check for consistent number of columns
    if (!is.atomic(data[[i]][[1]])){  # list of lists
      for (j in seq(along=data[[i]])){
        obj <- data[[i]][[j]]
        if (all(is.element(xy, names(obj))))
          data[[i]][[j]][xy] <- transformXY(obj, recenter, offset, scale.data)

        # update limits
        xlim <- range(c(xlim, range(data[[i]][[j]]$x)))
        ylim <- range(c(ylim, range(data[[i]][[j]]$y)))
      }
    }
    else{ # single list
      obj <- data[[i]]
      if (all(is.element(xy, names(obj))))
        data[[i]][xy] <- transformXY(obj, recenter, offset, scale.data)

      # update limits
      xlim <- range(xlim, data[[i]]$x, na.rm=TRUE)
      ylim <- range(ylim, data[[i]]$y, na.rm=TRUE)
    }
  }

  # create an empty plot with sufficiently large limits
  if (!add){
    if (is.element("abline", names(data))){
      xlim  <- range(xlim, data$abline$v, na.rm=TRUE)
      ylim  <- range(ylim, data$abline$h, na.rm=TRUE)
    }

    plot(xlim, ylim, type="n", xlab=xlab, ylab=ylab, main=main, ...)
  }


  # initialize y-axis label vector
  if (yref)
    xstart <- ystart <- ygroup <- NULL


  # plot the data
  for (fun in names(data)){

    if (!is.atomic(data[[fun]][[1]])){  # list of lists
      dum <- lapply(data[[fun]],
        function(x, fun) do.call(fun, args=x), fun=fun)

      if (yref && is.element(fun, c("matlines","lines"))){
        xstart <- c(xstart, unlist(lapply(data[[fun]], function(z) z$x[1])))
        ystart <- c(ystart, unlist(lapply(data[[fun]], function(z) z$y[1,])))
        ygroup <- c(ygroup, rep(length(data[[fun]]),numCols(data[[fun]][[1]]$y)))
      }
    }
    else{
      if (yref && is.element(fun, c("matlines","lines"))){
        xstart <- c(xstart, data[[fun]]$x[1])
        ystart <- c(ystart, data[[fun]]$y[1,])
        ygroup <- c(ygroup, rep(1,numCols(data[[fun]]$y)))
      }
      do.call(fun, args=data[[fun]])
    }
  }

  # add yref labels for matlines and lines entities
  if (!add && yref){

    xstart  <- rep(xstart,length(ystart))[seq(along=ystart)]
    yrefmin <- ifelse1(ylim[1] > 0, min(ystart), 0)
    yoffset <- cumsum(c(yrefmin, rep(offset,length(ygroup)-1)))
    y1      <- rep(yoffset, ygroup[1])
    color   <-	if (!is.atomic(data[[1]][[1]])){ # list of lists
    							data[[1]][[1]]$col
    						}	else { # single list
    							data[[1]]$col	
    						} 						
    if (is.null(color)) color <- rep(seq(ygroup),ygroup[1])

    for (i in seq(along=ystart)){
      lines(approx(c(par("usr")[1], xstart[i]), c(y1[i], ystart[i]), n=5),
        pch=".", cex=2, type="p",col=color[i],lty=1)
    }
  }

  invisible(offset)
}

###
#	princomp2
###
# note: adapted from function fx.pca.matrix in package dimeR
# In S-PLUS, PCA can be computed no matter whether samples or features are more.
# In R, PCA can be computed only if there are more samples than features.
# If there are more features than samples, PCA can be computed 
# 	more efficiently from the transposed x.
# samples are rows and feature are columns
"princomp2" <-
function(x, ...)
{
	d <- dim(x)
	n <- d[1]	
	p <- d[2]

	# transpose and center x by feature
	center <- colMeans(x)
	txc <- t(x) - center
		
	# compute eigen in the transposed space
	if (!is.R())
		ans <- eigen(cov.wt(txc, cor=FALSE, center=FALSE)$cov, symmetric=TRUE)
	else
		ans <- eigen(cov.wt(txc, cor=FALSE, center=FALSE, method="ML")$cov, symmetric=TRUE)

	# compute sdev for the original samples
	ans$values <- sqrt(ans$values*p/n)
	names(ans) <- c("sdev", "loadings")[match(c("values", "vectors"), names(ans))]
	ans$sdev[is.na(ans$sdev)] <- 0
	ans$sdev[ans$sdev<0] <- 0

	# convert loadings to the original space
	ans$loadings <- txc %*% ans$loadings	
	ans$loadings <- apply(ans$loadings, 2, function(x) x/sqrt(sum(x^2)))
	oldClass(ans$loadings) <- "loadings"

	# compute scores for the original samples
	ans$scores <- t(txc) %*% ans$loadings
		
	cnames <- paste("Comp.", 1:n, sep = "")
	if(!length(rnames <- dimnames(x)[[2]]))
		rnames <- paste(if (!is.R()) "X" else "V", 1:p, sep = "")
	names(ans$sdev) <- cnames
	dimnames(ans$loadings) <- list(rnames, cnames)
	dimnames(ans$scores) <- list(dimnames(ans$scores)[[1]], cnames)

	ans$center <- center
	names(ans$center) <- rnames
	ans$scale <- rep(1, p)
	ans$n.obs <- n
#	ans$terms <- Terms
	ans$call <- match.call()
#	ans$factor.sdev <- ans$sdev
	ans$coef <- ans$loadings			
	oldClass(ans) <- "princomp"
		
	ans
}

###
# rescale
###

"rescale" <- function(x, range.=c(0,1), ...)
{
  if (!isVectorAtomic(x) && !is(x,"signalSeries") && !is.matrix(x))
    stop("Input must a vector, signalSeries, or matrix")
  checkVectorType(x,"numeric")
  if (length(range.) != 2)
    stop("Range must be a two-element numeric vector")

  range. <- sort(range.)
  drange <- diff(range.)

  minx <- min(x)
  x    <- x - minx
  maxx <- max(x)
  x    <- x / maxx * drange + range.[1]

  z <- list(...)

  if (length(z)){

  	z <- lapply(z, function(x, minx, xscale, offset)
      (x - minx) * xscale + offset,
  	  minx=minx, xscale=drange/maxx, offset=range.[1])

    return(c(list(x), z))
  }
  else
    return(x)
}

###
# zeroCross
###

"zeroCross" <- function(x, slope="positive")
{
  checkVectorType(x,"numeric")
  checkScalarType(slope,"character")
  slope <- match.arg(slope,c("positive","negative"))
  slope <- match.arg(lowerCase(slope), c("positive","negative"))

  ipost  <- ifelse1(slope == "negative", sort(which(c(x, 0) < 0 & c(0, x) > 0)),
    sort(which(c(x, 0) > 0 & c(0, x) < 0)))
  offset <- apply(matrix(abs(x[c(ipost-1, ipost)]), nrow=2, byrow=TRUE), MARGIN=2, order)[1,] - 2
  ipost + offset
}


