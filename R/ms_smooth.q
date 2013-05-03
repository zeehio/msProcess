################################################
## S+Proteome children smoothing functions
##
##  msSmoothApprox
##  msSmoothKsmooth
##  msSmoothLoess
##  msSmoothMonotone
##  msSmoothSpline
##  msSmoothSupsmu
##  msSmoothMRD
##
################################################

###
# msSmoothApprox
###

"msSmoothApprox" <- function(x, y, method="linear", rule=2, f=0.5,
  index=rep(TRUE, length(x)), process="msSmoothApprox")
{
  method <- match.arg(method, c("linear", "constant"))
  if (rule != 2 && rule != 3) stop("argument 'rule' can be only 2 or 3")

  if (!isProcessRecorded(process)){
    report <- list(process=process, method=method, rule=rule,
      f=f, "index range"=range(index))
    assignEvent(report, process)
  }

  approx(x[index], y[index], xout=x, method=method, rule=rule, f=f)$y
}

###
# msSmoothKsmooth
###

"msSmoothKsmooth" <- function(x, y, kernel="box", bandwidth=100,
  process="msSmoothKsmooth")
{
  kernel <- match.arg(kernel, c("box","triangle","parzen","normal"))

  if (!isProcessRecorded(process)){
    report <- list(process=process, kernel=kernel, bandwidth=bandwidth)
    assignEvent(report, process)
  }

  ksmooth(x, y, kernel, bandwidth, x.points=x)$y
}

###
# msSmoothLoess
###

"msSmoothLoess" <- function(x, y, span=0.1, degree=1, family="symmetric",
  index=rep(TRUE, length(x)), process="msSmoothLoess")
{
  family <- match.arg(family, choices=c("symmetric", "gaussian"))
  fit    <- loess.smooth(x[index], y[index],
	          span, degree, family, evaluation=length(x))

  if (!isProcessRecorded(process)){
    report <- list(process=process, span=span, degree=degree, family=family)
    assignEvent(report, process)
  }

  approx(x=fit$x, y=fit$y, xout=x, method="linear", rule=2, f=0)$y
}

###
# msSmoothMean
###

"msSmoothMean" <- function(y, half.span=250, process="msSmoothMean")
{
  # define local function
  "movingAverage" <- function(x, kernel, boundary="zero")
  {
    # check input arguments
    if (!isVectorAtomic(x) || !is.numeric(x))
      stop("Input x must be a numeric vector")
    if (!isVectorAtomic(kernel) || !is.numeric(kernel))
      stop("Input kernel must be a numeric vector")

    supported.boundary <- c("zero", "periodic", "reflection", "continue")
    boundary <- match.arg(lowerCase(boundary), supported.boundary)

    ibound <- pmatch(boundary, supported.boundary) - 1

    as.vector(.Call("RS_signal_correlate",
      as.double(x), as.double(kernel), as.integer(ibound),
      COPY=rep(FALSE,3), CLASSES = c( "matrix", "matrix", "integer"), PACKAGE="ifultools")/sum(kernel))
  }

  y      <- as.vector(y)
  len    <- length(y)
  span   <- 2 * half.span + 1
  kernel <- rep(1, span)

  fit <- movingAverage(y, kernel, boundary="reflection")
  fit <- fit[(half.span+1):(half.span+len)]

  if (!isProcessRecorded(process)){
    report <- list(process=process, half.span=half.span)
    assignEvent(report, process)
  }

  fit
}

###
# msSmoothMonotone
###

"msSmoothMonotone" <- function(x, process="msSmoothMonotone")
{
  if (!isProcessRecorded(process))
    assignEvent(list(process=process), process)

  cummin(x)
}

###
# msSmoothSpline
###

"msSmoothSpline" <- function(x, y, df=30, spar=0, cv=FALSE, all.knots=FALSE,
  df.offset=0, penalty=1, index=rep(TRUE, length(x)), process="msSmoothSpline")
{
  if (is.R()){
   fit <- smooth.spline(x[index], y[index],
     w=rep(1,length(x[index])), df=df, spar=spar, all.knots=all.knots, df.offset=df.offset, penalty=penalty)

  } else {
   fit <- smooth.spline(x[index], y[index],
     w=rep(1,length(x[index])), df=df, spar=spar, cv=cv, all.knots=all.knots, df.offset=df.offset, penalty=penalty)
  }

  if (!isProcessRecorded(process)){
    report <- list(process=process, df=df, spar=spar, cv=cv, all.knots=all.knots,
      df.offest=df.offset, penalty=penalty)
    assignEvent(report, process)
  }

  #predict.smooth.spline(fit, x=x, deriv=0)$y
  predict(fit, x=x, deriv=0)$y
}

###
# msSmoothSupsmu
###

"msSmoothSupsmu" <- function(x,	y,	span="cv", periodic=FALSE, bass=0,
  index=rep(TRUE, length(x)), process="msSmoothSupsmu")
{
  fit <- supsmu(x[index], y[index], wt=rep(1, length(y[index])), span,
    periodic, bass)

  if (!isProcessRecorded(process)){
    report <- list(process=process, span=span, periodic=periodic, bass=bass)
    assignEvent(report, process)
  }

  approx(x=fit$x, y=fit$y, xout=x, method="linear", rule=2, f=0)$y
}

###
# msSmoothMRD
###

"msSmoothMRD" <- function(x, wavelet="s8",
  levels=1, xform="modwt", reflect=TRUE,
  keep.smooth=TRUE, keep.details=FALSE,
  process="msSmoothMRD")
{
  # calculate a normalized (partial) sum over the set MRD details (optional)
  # and smooth via the wavMRDSum() function. Rely on this function to also
  # check the input arguments.

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
