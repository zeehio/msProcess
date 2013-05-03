################################################
## msProcess S3 classes and methods
##
## Class msList
## Constructor function: msList
## Methods:
##
##  [.msList
##  plot.msList
##  print.msList
##  print.summary.msList
##  summary.msList
##  merge.msList
##
## Class: msSet
## Constructor function: msSet
## Methods:
##
##  [.msSet
##  [[.msSet
##  apply.msSet
##  image.msSet
##  plot.msSet
##  print.msSet
##  summary.msSet
##  print.summary.msSet
##
################################################

###
# [.msList
###

setOldClass("msList")

"[.msList" <- function(x, ..., drop=FALSE)
{
  narg <- nargs() - !missing(drop)
  if (narg < 2)
    stop("Incorrect indexing format. Should be of the form x[i]")

  len <- length(x)
  if (max(..1)>len)
    stop("Array subscript (", max(..1), ") out of bounds, should be at most ", len)

  z <- oldUnclass(x)[..1]
  attr(z, "type") <- attr(x, "type")[..1]
  oldClass(z) <- "msList"
  z
}

if (!is.R()){
  setMethod("[", "msList",
  function(x, ..., drop = TRUE)
  {
  	if (missing (drop)) {
  		"[.msList"(x, ...)
  	} else {
  		"[.msList"(x, ..., drop = drop)
  	}
  })
} else {
  setMethod("[", "msList",
  function(x, i, j, ..., drop = TRUE)
  {
  	if (missing (drop)) {
  		"[.msList"(x, i, j, ...)
  	} else {
  		"[.msList"(x, i, j, ..., drop = drop)
  	}
  })
}

###
# plot.msList
###

"plot.msList" <- function(x, index=1, type="l", add=FALSE, ...)
{
  if (length(index) != 1)
    stop("Spectra in an object of class msList ",
	 "can be plotted only one at a time.")

  if (!add && !all(par("mfrow")==c(1,1))){
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow=c(1,1))
  }

  plot(x[[index]], ..., type=type)
}

###
# print.msList
###

"print.msList" <- function(x, justify="left", sep=":", ...)
{
  main <- "An object of class msList"
  cat(main,"\n")
  cat(rep("-",nchar(main)),"\n",sep="")

  categories <- c(
   "Number of spectra",
   "Class type(s)",
   "Spectra per class")

  values <- list(
    length(x),
    levels(attr(x, "type")),
    table(attr(x, "type")))

  categories <- format(categories, justify=justify)

  for (i in seq(along=categories))
    cat(categories[i], sep, values[[i]], "\n", sep=" ")

  invisible(x)
}

###
# print.summary.msList
###

"print.summary.msList" <- function(x, ...)
{
  NextMethod("print")
  invisible(x)
}

###
# summary.msList
###
setOldClass(c("summary.msList", "data.frame"))
"summary.msList" <- function(object, ...)
{
  n.mz   <- sapply(object, numRows)
  mz.min <- sapply(object, function(x) x[1,1])
  mz.max <- sapply(object, function(x) x[numRows(x),1])
  intensity.min <- sapply(object, function(x) min(x[,2]))
  intensity.max <- sapply(object, function(x) max(x[,2]))
  type <- attr(object, "type")
  value <- data.frame(n.mz, mz.min, mz.max, intensity.min, intensity.max, type)
  oldClass(value) <- c("summary.msList", "data.frame")
  value
}

###
# merge.msList
###
"merge.msList" = function(...){

	## not all msList objects	
	ll = list(...)
	classes = sapply(ll, class)
	if(any(classes != "msList")) stop("all arguments must be of class msList")
	
	## length = 1 case		
	if(length(ll) == 1)
		return(ll[[1]])

	## length > 1 case
	types = unlist(sapply(ll, function(x)as.character(attr(x, "type"))))
	result = c(...)
	attr(result, "type") = factor(types)
	oldClass(result) = "msList"
	result
}


################################################
## Class: msSet
## Constructor function: msSet
## Methods:
##
##  [.msSet
##  [[.msSet
##  apply.msSet
##  image.msSet
##  plot.msSet
##  print.msSet
##  summary.msSet
##  print.summary.msSet
##
################################################

###
# msSet
###

"msSet" <- function(x, mz=NULL, type=NULL, data.name=NULL, ...)
{
  # define local functions
  "checkDimensions" <- function(vec,mat,by="rows"){
    by <- match.arg(by, c("rows","cols"))
    name <- deparseText(substitute(vec))
    if (!isVectorAtomic(vec))
      stop(name, "must be a vector or a matrix with a single column or row")
    if (by == "cols"){
      if (NROW(vec) != NCOL(mat))
        stop("Length of ", name, " vector must match the number of ",
          "columns of input matrix")
    }
    else{
      if (NROW(vec) != NROW(mat))
        stop("Length of", name, "vector must match the number of",
          "rows of input matrix")
    }

    invisible(NULL)
  }

  # check x input and convert to matrix
  if (is(x,"msSet")){
     z <- list(...)
     if (length(z)){
       nms <- names(z)
       for (i in seq(along=z))
         x[[nms[i]]] <- z[[i]]
     }
     return(x)
  }

  # check remaining inputs
  if (!(isVectorAtomic(x) || is.matrix(x)))
    stop("x can be a vector, a matrix, a data.frame, ",
     "or an object of class msSet")

  # obtain data name
  if (is.null(data.name))
    data.name <- deparseText(substitute(x))
  checkScalarType(data.name, "character")

  x <- as.matrix(x)

  # check optional inputs
  if (is.null(mz))
    mz <- seq(length=numRows(x))
  if (is.null(type))
    type <- rep("unclassified", length=numCols(x))
  checkDimensions(mz, x, "rows")
  checkDimensions(type, x, "cols")

  z <- list(mz=as.vector(mz), intensity=x, type=as.factor(type))
  oldClass(z) <- "msSet"

  # attach additional elements
  z <- msSet(z, ...)

  # assign data name attribute
  attr(z, "data.name") <- data.name

  z
}

###
# [.msSet
###

setOldClass("msSet")

"[.msSet" <- function(x, ..., drop=FALSE)
{
  narg <- nargs() - !missing(drop)
  if (narg < 3)
    stop("Incorrect indexing format. Should be of the form x[i,j,drop=FALSE]")

  x$mz <- x$mz[..1]
  x$type <- x$type[..2]
  x$intensity <- x$intensity[..1, ..2, drop=drop]
  x
}

if (!is.R()) {
	setMethod("[", "msSet",
	function(x, ..., drop=TRUE)
	{
		if (missing (drop)) {
			"[.msSet"(x, ...)
		} else {
			"[.msSet"(x, ..., drop=drop)
		}
	})
} else {
	setMethod("[", "msSet",
	function(x, i, j, ..., drop=TRUE)
	{
		if (missing (drop)) {
			"[.msSet"(x, i, j, ...)
		} else {
		"[.msSet"(x, i, j, ..., drop=drop)
	}})
}


if (is.R()){

  ## apply is not an S3 Generic in R, so we overload its definition here
  ## so that apply.msSet is called when apply(x, ...) is invoked where x
  ## is an object of class "msSet"
  apply.default <- base::apply
#  apply <- function(X, MARGIN, FUN, ...) UseMethod("apply")
  apply <- function(X, MARGIN, FUN, ..., type="intensity", pre=NULL, covar=NULL) UseMethod("apply")
}

###
# apply.msSet
###

"apply.msSet" <- function(X, MARGIN, FUN, ...,
  type="intensity", pre=NULL, covar=NULL)
{
  # define local functions
  isRegularList <- function(x)
    return( (length(unique(unlist(lapply(x,numRows)))) == 1) &&
      all(unlist(lapply(x, isVectorAtomic))))

  # check inputs
  if (!is.character(type))
  stop("type must be a character string")

  # extract data
  mat <- X[[type]]

  # format the data matrix if requested
  if (is.function(pre)) mat <- pre(mat)

  # apply
  if (is.null(covar)){
    z <- apply(mat, MARGIN=MARGIN, FUN=FUN, ...)
  }
  else{
    # verify structure of covariates
    if (!is.list(covar))
      stop("covar must be a list")

    # verify dimensions
    if (!all(unlist(lapply(covar, is.matrix))))
      stop("All covar objects must be matrices")
    if (!all(unlist(lapply(covar,
      function(x, MARGIN){dim(x)[MARGIN]},
      MARGIN=MARGIN)) == dim(X)[MARGIN]))
      stop("All covar objects must have matrices ",
        "whose MARGIN dimension is consistent with that of X")

    # extract common arg names
    common.args <- intersect(argNames(FUN), names(covar))

    if (is.missing(common.args))
      stop("covar names do not match the args of FUN")

    covar  <- covar[common.args]
    covnms <- names(covar)

    #initialize variables
    nz <- dim(mat)[MARGIN]
    z  <- vector("list", length=nz)

    if (is.function(FUN)) funname <- "FUN"
    else funname <- FUN

    # loop over each dimension

    if (MARGIN == 1){
      for (i in seq(nz)){
        covargs <- lapply(covar, "[", i,)
        names(covargs) <- covnms

        Args   <- c(list(mat[i,]), list(...), covargs)
        z[[i]] <- do.call(what=funname, args=Args)
      }

      # if a regular list, convert to a matrix
      # otherwise, return the list
      if (isRegularList(z)){
        z <- as.matrix(rbind.data.frame(z))
        dimnames(z)
      }
    }
    else{
      for (i in seq(nz)){
        covargs <- lapply(covar, "[",,i)
        names(covargs) <- covnms

        Args   <- c(list(mat[,i]), list(...), covargs)
        z[[i]] <- do.call(what=funname, args=Args)
      }

      # if a regular list, convert to a matrix
      # otherwise, return the list
      if (isRegularList(z)){
        z <- as.matrix(cbind.data.frame(z))
        dimnames(z)
      }
    }
  }

  z
}


###
# image.msSet
###

"image.msSet" <- function(x, what="spectra", subset=NULL, xaxis="mass",
	xlim=NULL, add=FALSE, xaxs="i", yaxs="i",  main = paste(what, "for", deparse(substitute(x))), ...)
{
  if (!add && !all(par("mfrow")==c(1,1))){
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow=c(1,1))
  }

  xaxis <- match.arg(xaxis, choices=c("mass", "time"))

  mz <- x$mz
  nmz <- length(mz)
  mass.range <- range(mz)

  if (is.null(xlim))
  	xlim <- switch(xaxis, mass=mass.range, time=c(1, nmz))
  else if (length(xlim) != 2)
  	stop("xlim should be a vector of length 2")
  else if (any(is.na(xlim)))
    xlim[is.na(xlim)] <- switch(xaxis, mass=mass.range[is.na(xlim)], time=c(1, nmz)[is.na(xlim)])

  if (xlim[1]>=xlim[2])
    stop("the first element of xlim should be less than its second element")

  if (is.null(subset)) subset <- seq(ncol(x$intensity))
  n.subset <- length(subset)

  # construct the region of interest
  if (xaxis=="mass"){
    if (xlim[1]<mass.range[1] || xlim[2]>mass.range[2]){
      stop("xlim should be within [", mass.range[1], ", ", mass.range[2], "].")
    }
    tick.range <- which(mz>=xlim[1])[1]:which(mz>=xlim[2])[1]
    xvector <- mz[tick.range]
    xlab <- "mass/charge"
  }
  else if (xaxis=="time"){
    if (xlim[1]<1 || xlim[2]>nmz)
      stop("xlim should be within [1, ", nmz, "].")
    xvector <- tick.range <- xlim[1]:xlim[2]
    xlab <- "clock tick"
  }

  # extract the information of interest
  what <- match.arg(what, choices=c("spectra", "noise", "noise.local",
	"baseline", "peak.list", "peak.matrix"))
#  main <- paste(what, "for", deparse(substitute(x)))

  if (what=="spectra"){
    ymatrix <- x$intensity[tick.range, subset, drop=FALSE]
    image(xvector, seq(n.subset), ymatrix, xaxs=xaxs, yaxs=yaxs,
	  main=main, xlab=xlab, ylab="spectrum index", ...)
  }
  else if (what=="peak.list") {	# not visualized well due to aliasing?
    peak.list <- x$peak.list

    if (is.null(peak.list))
      stop("The input has no peak.list attribute")

    # count peaks for each peak class
    peak.logic <- matrix(0, nrow=nmz, ncol=n.subset)
    for (i in seq(n.subset)) {
      peak.logic[peak.list[[subset[i]]][, "tick.loc"], i] <- 1
    }

    ymatrix <-  peak.logic[tick.range, , drop=FALSE]
    image(xvector, seq(n.subset), ymatrix, xaxs=xaxs, yaxs=yaxs,
	  main=main, xlab=xlab, ylab="spectrum index", ...)
  }
  else if (what=="peak.matrix"){ # xlim is ignored
    peak.matrix <- x$peak.matrix
    if (is.null(peak.matrix))
      stop("The input has no peak.matrix attribute")

    pmatrix <- t(peak.matrix[subset, , drop=FALSE])
    image(seq(numRows(pmatrix)), seq(ncol(pmatrix)), pmatrix, xaxs=xaxs, yaxs=yaxs,
      main=main, xlab="peak class index", ylab="spectrum index",
      sub=paste("measure =", attr(peak.matrix, "measure")), ...)
  }
  else {
    roi <- x[[what]]
    if (is.null(roi))
      stop("The input has no ", what, " attribute.\n")
    ymatrix <- roi[tick.range, subset, drop=FALSE]
    image(xvector, seq(n.subset), ymatrix, xaxs=xaxs, yaxs=yaxs,
      main=main, xlab=xlab, ylab="spectrum index", ...)
  }

  invisible(x)
}

###
# plot.msSet
###

# TODO: add legend using key().

"plot.msSet" <- function(x, process="msPrepare", subset=1, offset=NULL,
	xaxis="mass", xlim=NULL, type="l", pch=1, lty=1:2, col=1:8, lwd=1, add=FALSE, main = paste(process, "for", deparse(substitute(x))), ...)
{
  # define local functions
  "msStop" <- function(arg,func){
     stop("The input has no attached \"", arg, "\" object, ",
       "which could be generated by running ", func, "()")
  }

  if (!add && !all(par("mfrow")==c(1,1))){
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow=c(1,1))
  }

  process <- match.arg(process, choices=c("msPrepare", "msDenoise",
   "msNoise", "msDetrend", "msNormalize",
   "msPeak", "msAlign"))

  # define common variables
  xaxis <- match.arg(xaxis, choices=c("mass", "time"))

  mz         <- x$mz
  nmz        <- length(mz)
  mass.range <- range(mz)

  if (is.null(xlim))
    xlim <- switch(xaxis, mass=mass.range, time=c(1, nmz))
  else if (length(xlim) != 2)
  	stop("xlim should be a vector of length 2")
  else if (any(is.na(xlim)))
    xlim[is.na(xlim)] <- switch(xaxis, mass=mass.range[is.na(xlim)], time=c(1, nmz)[is.na(xlim)])

  if (xlim[1]>=xlim[2])
    stop("the first element of xlim should be less than its second element")

  # construct the region of interest
  if (xaxis == "mass"){

    if (xlim[1] < mass.range[1] || xlim[2] > mass.range[2])
      stop("xlim should be within [", mass.range[1], ", ", mass.range[2], "].")

     tick.range <- which(mz>=xlim[1])[1]:which(mz>=xlim[2])[1]
     xvector    <- mz[tick.range]
     xlab       <- "mass/charge"
  }
  else if (xaxis == "time"){

    if (xlim[1] < 1 || xlim[2] > nmz)
      stop("xlim should be within [1, ", nmz, "].")

    xvector <- tick.range <- xlim[1]:xlim[2]
    xlab    <- "clock tick"
  }

  if (is.null(subset)) subset <- 1:numCols(x$intensity)
  col <- rep(col, length.out=length(subset))

  intensity <- x$intensity[, subset, drop=FALSE]
  ymatrix   <- intensity[tick.range, ]
  n.xlim    <- length(tick.range)
  n.spectra <- length(subset)

  ##
  # gather information to be plotted
  ##

  # reverse normalization
  #TODO: alter normalization based on new MRD technique and new mother function
  if (is.element(process,
    c("msPrepare", "msDenoise", "msDetrend","msNormalize"))){

    tic <- x$tic

    if (!is.null(tic)){
      unnormalized <- (ymatrix / median(tic)) *
        rep(tic[subset], each=n.xlim)
    }
    else if (process == "msNormalize"){
      #TODO: need to change this TIC check since msNormalizeMRD
      # does not register TIC information.
#      msStop("tic", "msNormalize")
    }
    else unnormalized <- ymatrix
  }

  # reverse baseline subtraction
  if (is.element(process,
		 c("msPrepare", "msDenoise", "msDetrend"))){
    baseline <- x$baseline
    if (!is.null(baseline)){
      baseline    <- baseline[tick.range, subset]
      undetrended <- unnormalized + baseline
    }
    else if (process == "msDetrend"){
      msStop("baseline", "msDetrend")
    }
    else undetrended <- unnormalized
  }

  # reverse denoising, noise
  if (is.element(process, c("msPrepare", "msDenoise", "msNoise"))){
    noise <- x$noise
    if (!is.null(noise)){
      noise <- noise[tick.range, subset]
      if (process != "msNoise")
        undenoised <- undetrended + noise
    }
    else if (is.element(process, c("msDenoise", "msNoise"))){
      msStop("noise", "msDenoise")
    }
    else if (process!="msNoise") undenoised <- undetrended
  }

  # noise.local
  if (process == "msNoise"){
    noise.local <- x$noise.local
    if (is.null(noise.local))
      msStop("noise.local", "msNoise")
    noise.local <- noise.local[tick.range, subset]
  }

  # peak.list
  use.mean <- ifelse(is.null(x$use.mean), FALSE, x$use.mean)

  if (is.element(process, c("msPeak", "msAlign"))){
    peak.list <- x$peak.list
    if (!is.null(peak.list)){
      peak.list  <- peak.list[subset]
      peak.loc   <- lapply(peak.list,
          function(x, tick.range)
          {
          	# return NULL if no peak detected 
            if (NROW(x)==0) return(NULL)
            peak.loc <- x[, "tick.loc"]
            peak.loc[(peak.loc >= tick.range[1]) &
              (peak.loc <= tick.range[length(tick.range)])]
          }, tick.range=tick.range)
      n.peak.max <- max(sapply(peak.loc, length))
      pxmatrix <- pymatrix <- matrix(ncol=n.spectra, nrow=n.peak.max)
      for (i in seq(n.spectra)){
        loc <- peak.loc[[i]]
        n.peak <- length(loc)
        if (n.peak != 0) {
	        pxmatrix[1:n.peak, i] <- switch(xaxis, mass=mz[loc], time=loc)
	        pymatrix[1:n.peak, i] <- intensity[loc, i]
        }
      }
    }
    else if (process=="msPeak" && !use.mean){
      msStop("peak.list", "msPeak")
    }
  }

  # peak.class
  if (process == "msAlign" || (process == "msPeak" && use.mean)){
    peak.class <- x$peak.class
    if (is.null(peak.class))
      msStop("peak.class", "msAlign")
    class.loc <- peak.class[, "tick.loc"]
    class.loc <- class.loc[(class.loc >= tick.range[1]) &
                           (class.loc <= tick.range[length(tick.range)])]
    pxvector <- switch(xaxis, mass=mz[class.loc], time=class.loc)

    if (process == "msPeak" && use.mean) {
      ymatrix <- x$intensity.mean[tick.range, ]
      pxmatrix <- pymatrix <- matrix(ncol=1, nrow=length(class.loc))
      pxmatrix[,1] <- pxvector
      pymatrix[,1] <- x$intensity.mean[class.loc, 1]
    }
  }

  # create plot list
  z <- list(matlines=NULL, matpoints=NULL, abline=NULL, text=NULL)
  lty <- rep(lty, 2)[1:2]
  lwd <- rep(lwd, 2)[1:2]

  if (process == "msPrepare"){
    z$matlines <- list(x=xvector, y=undenoised, lty=lty[1], col=col, lwd=lwd[1], ...)
  }
  else if (process == "msDenoise"){
    z$matlines <- list(
      list(x=xvector, y=undetrended, lty=lty[1], lwd=lwd[1], col=col, ...),
      list(x=xvector, y=undenoised,  lty=lty[2], lwd=lwd[2], col=col, ...))
  }
  else if (process == "msNoise"){
    z$matlines <- list(
      list(x=xvector, y=abs(noise),  lty=lty[2], lwd=lwd[2], col=col, ...),
      list(x=xvector, y=noise.local, lty=lty[1], lwd=lwd[1], col=col, ...))
  }
  else if (process == "msDetrend"){
    z$matlines <- list(
      list(x=xvector, y=undetrended, lty=lty[1], lwd=lwd[1], col=col,...),
      list(x=xvector, y=baseline,    lty=lty[2], lwd=lwd[2], col=col, ...))
  }
  else if (process == "msNormalize"){
    z$matlines <- list(
      list(x=xvector, y=ymatrix,      lty=lty[1], lwd=lwd[1], col=col),
      list(x=xvector, y=unnormalized, lty=lty[2], lwd=lwd[2], col=col))
  }
  else if (is.element(process, c("msPeak", "msAlign"))){
    z$matlines <- list(x=xvector, y=ymatrix, lty=lty[1], lwd=lwd[1], col=col)
    # peak.list
    if (!is.null(peak.list) || (process == "msPeak" && use.mean)){
      z$matpoints <- list(x=pxmatrix, y=pymatrix, pch=pch, col=col)
    }
    # peak.class
    if (process=="msAlign"){
      z$abline <- list(v=pxvector, lty=lty[2], lwd=lwd[2], col=col, ...)
    }
  }

#  main <- paste(process, "for", deparse(substitute(x)))
  msPlot(matlines=z$matlines, matpoints=z$matpoints, abline=z$abline,
    text=z$text, main=main, xlab=xlab, ylab="intensity", offset=offset, ...)
  invisible(x)
}

###
# print.msSet
###

"print.msSet" <- function(x, justify="left", sep=":", ...)
{
  main <- "Mass Spectra Data Set"
  data.name <- attr(x, "data.name")
  if (!is.null(data.name))
    main <- paste(main, data.name, sep=": ")
  cat(main,"\n")
  cat(rep("-",nchar(main)),"\n",sep="")

  categories <- c(
   "Number of spectra",
   "Class type(s)",
   "Spectra per class",
   "Number of m/z values",
   "Range of m/z values",
   "Range of intensities")

  values <- list(
    length(x$type),
    levels(x$type),
    table(x$type),
    length(x$mz),
    range(x$mz),
    range(x$intensity))

  categories <- format(categories, justify=justify)

  for (i in seq(along=categories))
    cat(categories[i], sep, values[[i]], "\n", sep=" ")

  invisible(x)
}

###
# print.summary.msSet
###

"print.summary.msSet" <- function(x, ...)
{
  if (!is.null(x$hist)){
    main <- "Data Processing History"
    data.name <- attr(x,"data.name")
    if (!is.null(data.name))
      main <- paste(main, data.name, sep=": ")
    cat(main,"\n")
    cat(rep("-",nchar(main)),"\n",sep="")
    print(x$hist)
  }
  else cat("No summary or processing history is available.\n")
  invisible(x)
}

###
# summary.msSet
###

"summary.msSet" <- function(object, ...)
{
  z <- list(hist=getHistory(object))
  oldClass(z) <- "summary.msSet"
  attr(z,"data.name") <- attr(object,"data.name")
  z
}

