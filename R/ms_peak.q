################################################
# S+Proteome peak detection functions.
#
# Mother Functions
#  msPeak
#
# Children Functions
#  msPeakCWT
#  msPeakMRD
#  msPeakSearch
#  msPeakSimple
#
# Utility Functions
#  msPeakInfo
#  msPeakPrune
#
################################################

################################################
##  Mother peak detection function: msPeak
################################################

"msPeak" <- function(x, FUN="simple",
  use.mean=FALSE,
  event="Peak Detection", ...)
{
	# setup
	type <- "intensity" 
	MARGIN <- 2
	
  # initialize variables
  supported <- c("simple", "search", "cwt", "mrd")

  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")
  if (!is.element(class(FUN), c("function","character")))
     stop("Unsupported object class for input: FUN")
  if (is.character(FUN))
    FUN <- matchObject(paste(sys.call()[[1]], match.arg(FUN, supported), sep=""))

  # prompt event queue
  throwEvent(event)

  # if MRD, verify existence of attached MRD object in the msSet
  is.mrd <- as.logical(is.character(FUN) && FUN == "msPeakMRD")
  if (is.mrd) {
  	if (is.null(x$mrd))
  	  stop("An MRD object is not attached to the input msSet object.")
    else if (length(x$mrd$levels) != 1)
      stop("The number of MRD levels is not 1.")
  }

  # compute mean spectrum if needed
  if (use.mean && ncol(x$intensity) > 1){
    # attach mean vectors as single column matrices
    intensity   <- matrix(rowMeans(x$intensity), ncol=1)
    x <- msSet(x, intensity.mean=intensity)

    if (!is.null(x$mrd)) {
    	 noise.local <- NULL
    }
    else {
      noise       <- matrix(rowMeans(x$noise), ncol=1)
      noise.local <- matrix(msSmoothMean(abs(noise), half.span=250), ncol=1)

      x <- msSet(x, noise.mean=noise)
      x <- msSet(x, noise.local.mean=noise.local)
    }

    type   <- "intensity.mean"
    MARGIN <- 2
  }
  else
    noise.local <- x$noise.local

  # estimate peak locations
  mainargs <- c("x","y","noise.local")
  funnames <- argNames(FUN)
  if (all(is.element(mainargs[1:3], funnames))){

    if (is.null(noise.local)) {
      z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
        type=type)
    }
    else {
      z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
        type=type, covar=list(noise.local=noise.local))
    }
  }
  else if (all(is.element(mainargs[1:2], funnames))){
    if (is.mrd)
      z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, n.level=x$mrd$levels, ...,
        type=type)
    else
      z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
        type=type)
  }
  else{
      z <- apply(x, MARGIN=MARGIN, FUN=FUN, ..., type=type)
  }

	# check the number of spectra with peak detected
  n.z <- length(z)
  n.nopeak <- sum(sapply(z, NROW) == 0)
  
  if (n.nopeak==n.z) {
  	stop("no peak detected on all spectra")
  } else if (n.nopeak != 0) {
  	warning(sprintf("no peak detected on %d out of %d spectra", n.nopeak, n.z))
  }

	x <- ifelse1(use.mean, msSet(x, peak.class=z[[1]]),
	  msSet(x, peak.list=z))

  x <- msSet(x, use.mean=use.mean)

  # update history information
  catchEvent(x)
}

################################################
##  Children peak detection functions
################################################

###
# msPeakCWT
###

"msPeakCWT" <- function(x, y, n.scale=100, snr.min=3, scale.min=4,length.min=10,
  noise.span=NULL, noise.fun="quantile", noise.min=NULL,
  n.octave.min=1, tolerance=0.0, holder=TRUE, process="msPeakCWT")
{
  # check input arguments. let the CWT function check the related arguments.
  checkVectorType(y,"numeric")

  # calculate the CWT
  W <- wavCWT(y, n.scale=n.scale, wavelet="gaussian2", variance=1)

  # form the CWT tree
  W.tree <- wavCWTTree(W, n.octave.min=n.octave.min, tolerance=tolerance, type="maxima")

  # isolate the peaks
	noise.min.raw <- quantile(abs(attr(W.tree,"noise")), 
		prob=if (is.null(noise.min)) {0.05} else {noise.min})
  p <- wavCWTPeaks(W.tree, snr.min=snr.min, scale.range=c(scale.min, length(x)), length.min=length.min,
    noise.span=noise.span, noise.fun=noise.fun, noise.min=noise.min.raw)

  # calculate corresponding Holder exponents
  if (holder){
    cusps <- holderSpectrum(W.tree)
  }

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process,
       scale.min=scale.min,
       noise.fun=noise.fun,
       noise.min=noise.min,
       noise.span=noise.span,
       n.octave.min=n.octave.min,
       tolerance=tolerance,
       snr.min=snr.min,
       wavelet="Mexican hat (gaussian2)",
       holder=holder)

    assignEvent(report, process)
  }

  # create index.max vector
  nmz <- length(y)
  index.min <- index.max <- rep(FALSE, nmz)
  imax <- attr(p,"peaks")[["iendtime"]]
  index.max[imax] <- TRUE

  # create index.min vector
  # NOTE: local minima are not isolated in the wavCWTPeaks function so
  # we estimate using the midpoint of adjacent maxima.
  if (length(imax) > 1){
    dmax <- round(diff(imax)/2)
    nmax <- length(imax)
    imin <- c(max(1,imax[1]-dmax[1]), imax[1:(nmax-1)] + dmax, min(nmz, imax[nmax]+ dmax[nmax-1]))
  }
  else{
    imin <- c(max(1, imax-2), min(nmz, imax+2))
  }
  index.min[imin] <- TRUE

  # wrap the peaks for output
  z <- msPeakInfo(x, y, index.min=index.min, index.max=index.max)

  if (holder){
    ibranch <- intersect(attr(p,"peaks")[["branch"]], cusps$branch)
    z[["holder"]] <- cusps$exponent[ibranch]
  }

  z
}

###
# msPeakMRD
###

"msPeakMRD" <- function(x, y, n.level=floor(log2(length(y))),
  concavity.threshold=0, snr.thresh=0, process="msPeakMRD")
{
	# check input arguments
	checkVectorType(y,"numeric")
	checkVectorType(n.level,"integer")
	checkScalarType(concavity.threshold,"numeric")
	if (concavity.threshold < 0)
	  stop("Concavity threshold must be non-negative")

  # initialize variables
  crystal <- paste("d", n.level, sep="")

  # Given the series D obtained by summing the wavelet
  # details over the specified levels, approximate the first and
  # second derivative of D by forming (approximate) zero phase
  # shifted versions of the MODWT(D) coefficients using the
  # Haar and D4 wavelets, respectively.
  W1D <- wavShift(wavMODWT(y, wavelet="haar", n.level=n.level))[[crystal]]
  W2D <- wavShift(wavMODWT(y, wavelet="d4", n.level=n.level))[[crystal]]

  # find the zero crossings of the first derivative approximation of D
  # corresponding to local maxima in D. subject the result to a second
  # concavity test using a user-defined threshold. here we note that
  # we are looking for a positive convavity due to a natural negation of
  # the second derivative approximation.
  icandidate.neg <- zeroCross(W1D, slope="negative")
  imax <- icandidate.neg[which(W2D[icandidate.neg] > concavity.threshold)]
  index.max <- rep(FALSE, length=length(y))
  index.max[imax] <- TRUE

  # find the local minima to define the peak boundary
  icandidate.pos <- zeroCross(W1D, slope="positive")
#  imin <- icandidate.pos[which(W2D[icandidate.pos] < -concavity.threshold)]
  imin <- icandidate.pos[which(W2D[icandidate.pos] < 0)]
  index.min <- rep(FALSE, length=length(y))
  index.min[imin] <- TRUE

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process, transform="MODWT",
      n.level=n.level, concavity.threshold=concavity.threshold,
      snr.thresh=snr.thresh)

    assignEvent(report, process)
  }

  msPeakInfo(x, y, index.min=index.min, index.max=index.max, snr.thresh=snr.thresh)
}

###
# msPeakSimple
###

"msPeakSearch" <- function(x, y, noise.local=NULL, span=41, span.supsmu=0.05,
	snr.thresh=2, process="msPeakSearch")
{
  # peak detection via maximum search for a single spectrum

  # find extrema
  index <- msExtrema(y, span=span)

  # find sites whose intensity is higher than its estimated average background
  smoothed     <- msSmoothSupsmu(x, y, span=span.supsmu)
  index.supsmu <- (y > smoothed)

  # combine the two requirements
  index.combined <- (index$index.max & index.supsmu)

  # write history information (only once)
  if (!isProcessRecorded(process)){
    record <- list(process=process, span=span, span.supsmu=span.supsmu,
      snr.thresh=snr.thresh)
    assignEvent(record, process)
  }

  msPeakInfo(x, y, index.min=index$index.min, index.max=index.combined,
    noise.local=noise.local, snr.thresh=snr.thresh)
}

###
# msPeakSimple
###

"msPeakSimple" <- function(x, y, noise.local=NULL, span=3,
  snr.thresh=2, process="msPeakSimple")
{
  # peak detection via simple maximum search for a single spectrum

  # find all local minima and maxima
  index <- msExtrema(y, span=span)

  # write history information (only once)
  if (!isProcessRecorded(process)){
    record <- list(process=process, span=span, snr.thresh=snr.thresh)
    assignEvent(record, process)
  }

  msPeakInfo(x, y, index.min=index$index.min, index.max=index$index.max,
    noise.local=noise.local, snr.thresh=snr.thresh)
}

################################################
##  General peak detection functions
################################################

###
# msPeakInfo
###

"msPeakInfo" <- function(x, y, index.min, index.max, noise.local=NULL, snr.thresh=2)
{
  # check inputs
  nvar <- length(x)
  checkVectorType(x,"numeric")
  checkVectorType(y,"numeric")
  checkVectorType(index.min,"logical")
  checkVectorType(index.max,"logical")
  checkScalarType(snr.thresh,"numeric")
  if (!all(c(length(y), length(index.min), length(index.max)) == nvar))
    stop("x, y, index.min, and index.max must be vectors of equal length")
  if (!is.null(noise.local)){
    checkVectorType(noise.local,"numeric")
    if (length(noise.local) != nvar)
      stop("noise.local must be a numeric vector of length equal to that of the original mass spectrum")
  }

  # gather peak information

  # calculate signal to noise ratio
  # use intensity if noise.local is NULL, i.e., treat noise.local as 1
  # NOTE: snr is a vector containing only the snr values estimated for each peak,
  # i.e., its length will typically be must less than the length of nvar
  imax <- which(index.max)
  snr <- ifelse1(is.null(noise.local), y[imax], y[imax] / noise.local[imax])

  # remove peaks with low snr
  good.snr <- (snr > snr.thresh)
  snr <- snr[good.snr]

  tick.loc <- imax[good.snr]

  # find the tick.left and tick.right bounds of each peak
  # TODO: is there any way to get rid of the for loop?
  if ((npeak=length(tick.loc))==0) return(data.frame())
  tick.left <- tick.right <- rep(1, length(tick.loc))

  for (i in 1:length(tick.loc)) {
    for (j in tick.loc[i]:1) {
      if (index.min[j]){
        tick.left[i] <- j
        break
      }
    }

    for (j in tick.loc[i]:length(x)) {
      if (index.min[j]){
        tick.right[i] <- j
        break
      }
    }

#    # NOTE: the above two for loops could be replaced with the following code
#    # it is simpler but much slower
#    if(any(index.min[1:tick.loc[i]]))
#      tick.left[i] <- max(which(index.min[1:tick.loc[i]]))
#    if(any(index.min[tick.loc[i]:length(x)]))
#      tick.right[i] <- min(which(index.min[tick.loc[i]:length(x)])) + tick.loc[i] - 1
  }

  # special handling for the span of the last peak
  if (tick.right[length(tick.loc)]==1)
    tick.right[length(tick.loc)] <- length(x)

  # remove duplicated peaks (due to plateaus) by keeping only the most left
  # TODO: take the average location as the new location?
  keep <- !duplicated(tick.left)

  # gather peak information
  tick.loc   <- tick.loc[keep]
  tick.left  <- tick.left[keep]
  tick.right <- tick.right[keep]
  mass.left  <- x[tick.left]
  mass.right <- x[tick.right]

  # Return peak info
  data.frame(
    tick.loc, tick.left, tick.right,
    tick.span = tick.right - tick.left + 1,
    snr = snr[keep], intensity = y[tick.loc],
    mass.loc = x[tick.loc], mass.left, mass.right,
    mass.span = mass.right - mass.left,
    row.names = NULL)
}
