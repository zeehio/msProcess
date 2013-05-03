################################################
## S+Proteome intensity normalization functions.
##
##   msNormalize
##     msNormalizeTIC
##     msNormalizeSNV
##
################################################

################################################
##  Mother denoising function: msNormalize
################################################

"msNormalize" <- function(x, FUN="tic",
 event="Intensity Normalization", ...)
{
  # initialize variables
  supported <- c("tic", "snv")

  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")
  if (!is.element(class(FUN), c("function","character")))
    stop("Unsupported object class for input: FUN")
  if (is.character(FUN))
    FUN <- paste("msNormalize", upperCase(match.arg(FUN, supported)), sep="")
  if (is.null(x$intensity))
    stop("Intensity matrix missing from msSet object")
  checkScalarType(event,"character")

  # prompt event queue
  throwEvent(event)

  if(is.character(FUN))
    FUN <- getFunction(FUN)
  else if(!is.function(FUN)){
    farg <- substitute(FUN)
    if(is.name(farg))
      FUN <- getFunction(farg)
    else
      stop(paste("\"", deparseText(farg), "\" is not a function", sep = ""))
  }

  x <- FUN(x, ...)

  # update history information
  x <- catchEvent(x)

  x
}

###
# msNormalizeTIC
###

"msNormalizeTIC" <- function(x, process="msNormalizeTIC")
{
  # compute total ion current (TIC) or area under the curve (AUC)
  tic <- colSums(x$intensity)

  # normalize
  nmz         <- length(x$mz)
  x$intensity <- x$intensity / rep(tic/median(tic), each=nmz)

  # add tic to the msSet object
  x <- msSet(x, tic=tic)

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process)
    assignEvent(report, process)
  }

  x
}

###
# msNormalizeSNV
###

"msNormalizeSNV" <- function(x, process="msNormalizeSNV")
{
  # compute standard deviation of the intensity for each spectrum
  # TODO: change tic to something more general, e.g, norm.factor
  #       only scaling is done here,
  #       need also centering (produce mean-zero spectra) first to be truly SNV.
  # NOTE: the MRD details are alrealy centered.

  tic <- sqrt(colVars(x$intensity, SumSquares=!is.R()))

  # normalize
  nmz         <- length(x$mz)
  x$intensity <- x$intensity / rep(tic/median(tic), each=nmz)

  # add tic to the msSet object
  x <- msSet(x, tic=tic)

  # write history information (only once)
  if (!isProcessRecorded(process)){

    report <- list(process=process)
    assignEvent(report, process)
  }

  x
}
