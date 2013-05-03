################################################
## S+Proteome baseline subtraction functions 
##  
##  msDetrend
##  msSmoothMRD
##  
################################################

##############################################################
##  Mother baseline subtraction function: msDetrend
##############################################################
 
"msDetrend" <- function(x, FUN="loess",
  attach.base=TRUE, 
  event="Baseline Correction", ...)
{
	# setup
	type <- "intensity"
	MARGIN <- 2
	pre <- NULL
	
  # initialize variables
  supported <- c("loess", "spline", "supsmu", "approx", "monotone", "mrd")

  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")
  if (!is.element(class(FUN), c("function","character")))
     stop("Unsupported object class for input: FUN")
  if (is.character(FUN))
    FUN <- matchObject(paste("msSmooth", match.arg(FUN, supported), sep=""))

  # prompt event queue
  throwEvent(event)

  # estimate baseline
  mainargs <- c("x","y","index")
  funnames <- argNames(FUN)

  if (all(is.element(mainargs[1:3], funnames))){

    index <- msExtrema(x[[type]], span=3)$index.min

    z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
      type=type, pre=pre, covar=list(index=index))
  }
  else if (all(is.element(mainargs[1:2], funnames))){
   
    z <- apply(x, MARGIN=MARGIN, FUN=FUN, x=x$mz, ...,
      type=type, pre=pre)
  }
  else{

    z <- apply(x, MARGIN=MARGIN, FUN=FUN, ...,
      type=type, pre=pre)
  }

  # remove baseline
  x <- msSet(x, intensity=x$intensity - z)
	
  # store baseline if desired
  if (attach.base) 
    x <- msSet(x, baseline=z)

  # add MRD meta data to msSet object for future reference
  # TODO: we need a more general mechanism to record meta
  # data evaluated in the child function.
  if (is.character(FUN) && FUN == "msSmoothMRD"){
  
    # get list of all args passed into child function
	  p <- mergeList(formals(msSmoothMRD), mergeList(as.list(match.call())[-1],
	  	c(formals(msDetrend),list(...))))

    x <- msSet(x, mrd=p[c("wavelet","levels","xform","reflect","keep.smooth","keep.details")])
  }

  # update history information
  catchEvent(x)
}
