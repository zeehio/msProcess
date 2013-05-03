################################################
## S+Proteome denoising and local noise
## estimation functions.
##
##  msDenoise
##    msDenoiseSmooth
##    msDenoiseMRD
##    msDenoiseWavelet
##  msDenoiseWaveletThreshold
##  msNoise
##
################################################

################################################
##  Mother denoising function: msDenoise
################################################

"msDenoise" <- function(x, FUN="wavelet", 
 attach.noise=TRUE, event="Denoising", ...)
{
	# setup
	type <- "intensity"
	MARGIN <- 2
	
  # initialize variables
  supported <- c("wavelet", "smooth", "mrd")

  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")
  if (!is.element(class(FUN), c("function","character")))
    stop("Unsupported object class for input: FUN")
  if (is.character(FUN))
    FUN <- matchObject(paste(sys.call()[[1]], match.arg(FUN, supported), sep=""))

  # prompt event queue
  throwEvent(event)

  # perform denoising
  z <- apply(x, MARGIN=MARGIN, FUN=FUN, ..., type=type)

  # add noise estimate to the msSet object
  if (attach.noise)
    x <- msSet(x, noise=(x$intensity - z))

  x <- msSet(x, intensity=z)

  # update history information
  x <- catchEvent(x)

  # add MRD meta data to msSet object for future reference
  # TODO: we need a more general mechanism to record meta
  # data evaluated in the child function.
  if (is.character(FUN) && FUN == "msDenoiseMRD"){

    # get list of all args passed into child function
	  p <- mergeList(formals(msDenoiseMRD), mergeList(as.list(match.call())[-1],
	  	c(formals(msDenoise),list(...))))

    x <- msSet(x, mrd=p[c("wavelet","levels","xform","reflect","keep.smooth",
      "normalize")])
  }

  x
}

################################################
##  Children denoising functions
##########################################z######

###
# msDenoiseSmooth
###

"msDenoiseSmooth" <- function(x, twiceit=TRUE, process="msDenoiseSmooth")
{
  # write history information (only once)
  if (!isProcessRecorded(process))
    assignEvent(list(process=process, twiceit=twiceit), process)

  if (is.R()) smooth(x, twiceit=twiceit, kind="3RS3R") else smooth(x, twiceit=twiceit)
}

###
# msDenoiseMRD
###

"msDenoiseMRD" <- function(x, wavelet="s8",
  levels=1, xform="modwt", reflect=TRUE,
  keep.smooth=TRUE, keep.details=TRUE,
  process="msDenoiseMRD")
{
  # calculate a partial sum over the set MRD details (optional) and smooth
  # via the wavMRDSum() function.
  # Rely on this function to also check the input arguments.

  z <- wavMRDSum(x, wavelet=wavelet, levels=levels,
	  xform=xform, reflect=reflect,
	  keep.smooth=keep.smooth, keep.details=keep.details)

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process,
      wavelet=wavelet,
      levels=levels,
      transform=xform,
      reflect=reflect,
      keep.smooth=keep.smooth,
      keep.details=keep.details)

    assignEvent(report, process)
  }

  z
}

###
# msDenoiseWavelet
###

"msDenoiseWavelet" <- function(x, wavelet="s8",
  n.level=as.integer(floor(logb(length(x), 2))),
  shrink.fun="hard", thresh.fun="universal",
  thresh.scale=1, xform="modwt", noise.variance=NULL,
  reflect=TRUE, process="msDenoiseWavelet", assign.attributes=FALSE)
{
  z <- wavShrink(x, wavelet=wavelet, n.level=n.level,
    shrink.fun=shrink.fun, thresh.fun=thresh.fun,
    thresh.scale=thresh.scale, xform=xform,
    noise.variance=noise.variance, reflect=reflect)

  # write history information (only once)
  if (!isProcessRecorded(process)){

    zatt <- attributes(z)

    report <- list(process=process,
      wavelet=zatt$wavelet,
      n.level=zatt$n.level,
	    shrink.fun=zatt$shrink.fun,
	    thresh.fun=zatt$thresh.fun,
	    thresh.scale=zatt$thresh.scale,
	    transform=zatt$transform,
	    noise.variance=
	     ifelse(zatt$noise.variance <= 0,"MAD estimate (internal)", zatt$noise.variance),
      reflect=zatt$reflect)

    assignEvent(report, process)
  }

  if (!assign.attributes)
    z <- as.vector(z)

  z
}

###
# msDenoiseWaveletThreshold
###

"msDenoiseWaveletThreshold" <- function(x, wavelet="s8",
  n.level=as.integer(floor(logb(length(x), 2))), shrink.fun="hard", thresh.scale=NULL,
  xform="modwt", reflect=TRUE, n.threshold=500,
  thresh.fun="universal", noise.variance=NULL,
  min.thresh=NULL, max.thresh=NULL)
  {
  # check input (some checks will be done by other called functions)
  checkScalarType(n.threshold, "integer")
  if (n.threshold < 2)
    stop("n.threshold must be at least 2")
  checkScalarType(xform, "character")
  checkScalarType(reflect, "logical")
  checkScalarType(wavelet, "character")
  xform <- match.arg(lowerCase(xform),c("modwt","dwt"))

  # obtain data name
  data.name <- deparseText(substitute(x))

  if (is.null(thresh.scale) && (is.null(min.thresh) || is.null(max.thresh))){

    wavxform <- ifelse1(xform == "modwt", wavMODWT, wavDWT)
    W <- wavxform(x, wavelet=wavelet, n.level=n.level)
    crystals <- paste("d", seq(n.level), sep="")
    thresh.range <- range(abs(unlist(W[crystals]$data)))

    if (is.null(min.thresh))
      min.thresh <- thresh.range[1]
    if (is.null(max.thresh))
      max.thresh <- thresh.range[2]
  }

  # form "infinitely smooth" reference
  reference <- msDenoiseWavelet(x, wavelet=wavelet,
    n.level=n.level, shrink.fun=shrink.fun,
    thresh.scale=max.thresh, xform=xform,
    reflect=reflect, assign.attributes=TRUE)

  # form thresh.scale vector
  if (is.null(thresh.scale))
    thresh.scale <- seq(min.thresh, max.thresh, length=n.threshold)

  checkVectorType(thresh.scale,"numeric")

  WS <- data.frame(lapply(thresh.scale,
    function(delta, x, wavelet, n.level, shrink.fun, thresh.fun, xform, reflect, noise.variance)
      msDenoiseWavelet(x, wavelet=wavelet, n.level=n.level, shrink.fun=shrink.fun,
        thresh.fun=thresh.fun, thresh.scale=delta, xform=xform, reflect=reflect, noise.variance=noise.variance),
        x=x, wavelet=wavelet, n.level=n.level, shrink.fun=shrink.fun, thresh.fun=thresh.fun,
        xform=xform, reflect=reflect, noise.variance=noise.variance))

  stat <- apply(WS, MARGIN=2, function(x, reference) abs(x - reference), reference=reference)
  z    <- list(thresh.scale=thresh.scale, WS=WS, stat=stat, reference=reference, series=x)

  oldClass(z) <- "msDenoiseWaveletThreshold"
  attr(z,"data.name")  <- data.name
  attr(z,"wavelet")    <- wavelet
  attr(z,"n.level")    <- attr(reference,"n.level")
  attr(z,"shrink.fun") <- shrink.fun
  attr(z,"thresh.fun") <- thresh.fun
  attr(z,"transform")  <- xform
  attr(z,"reflect")    <- reflect

  z
}

###
# print.msDenoiseWaveletThreshold
###

"print.msDenoiseWaveletThreshold" <- function(x, justify="left", sep=":", ...)
{
  main <- "Waveshrink Threshold Matrix"
  data.name <- attr(x, "data.name")
  if (!is.null(data.name))
    main <- paste(main, data.name, sep=": ")
  cat(main,"\n")
  cat(rep("-",nchar(main)),"\n",sep="")

  categories <- c(
   "Threshold range",
   "Wavelet transform",
   "Wavelet",
   "Decomposition levels",
   "Shrinkage function",
   "Reflection used")

  values <- list(
    paste(range(x$thresh.scale), collapse=" to "),
    upperCase(attr(x,"transform")),
    attr(x,"wavelet"),
    attr(x,"n.level"),
    upperCase(attr(x,"shrink.fun")),
    attr(x,"reflect"))

  categories <- format(categories, justify=justify)

  for (i in seq(along=categories))
    cat(categories[i], sep, values[[i]], "\n", sep=" ")

  invisible(x)
}

###
# plot.msDenoiseWaveletThreshold
###

"plot.msDenoiseWaveletThreshold" <- function(x, xlab="m/z", ylab="Waveshrink Threshold",
  lty=1, lwd=2, ...)
{
  mz <- as.numeric(names(x$series))
  image(x=mz, y=x$thresh.scale, z=x$stat, xlab=xlab, ylab=ylab)

  # rescale y range values to span middle 2/3 of current y range
  yrange <- par("usr")[3:4]
  dy     <- abs(diff(yrange))
  ylim   <- yrange + dy/6 * c(1,-1)

  z <- rescale(x$series, ylim, x$reference)

  # overlay original series
  for (i in seq(along=z)){
    lines(mz, z[[i]], lty=lty, lwd=lwd, col=i-1, ...)
  }

  invisible(NULL)
}

#########################################################
##  Mother local noise estimation function: msNoise
#########################################################

"msNoise" <- function(x, FUN="spline",
  pre=abs, detach.noise=FALSE,
  event="Local Noise Estimation", ...)
{
	# setup
	type <- "noise"
	MARGIN <- 2
	
  # initialize variable
  supported <- c("spline", "supsmu", "ksmooth", "loess", "mean")

  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")
  if (!is.element(class(FUN), c("function","character")))
    stop("Unsupported object class for input: FUN")
  if (is.character(FUN))
    FUN <- match.arg(FUN, supported)
  if (is.character(FUN))
     FUN <- properCase(match.arg(FUN, supported), pre="msSmooth")

  # prompt event queue
  throwEvent(event)

  if (!is.element("noise",names(x))){
    stop("noise component is missing in primary object\n",
      "and is needed to calculate local noise estimates:\n",
      "Call msDenoise() first to fill the void")
  }

  # perform local smoothing of noise data
  if (all(is.element(c("x","y"), argNames(FUN)))){
    z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
      type=type, pre=pre)
  }
  else{
    z <- apply(x, MARGIN=MARGIN, FUN=FUN, ...,
      type=type, pre=pre)
  }

  # add local noise estimate to the msSet object
  if (detach.noise) x <- msSet(x, noise=NULL)
  x <- msSet(x, noise.local=z)

  # update history information
  catchEvent(x)
}
