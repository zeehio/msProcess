################################################
## S+Proteome mass calibration functions
## Constructor function: msCalibrate
##
## Methods:
##
##  coef.msCalibrate
##  formula.msCalibrate
##  predict.msCalibrate
##
################################################

###
# msCalibrate
###

"msCalibrate" <- function(mz, tof, u=20000, FUN="lm", digits=4,
  predict.mz=TRUE)
{
  if (is.R()){

    "poly.transform" <- function(polymat, coefs, intercept = TRUE)
    {
    	deg <- dim(polymat)[2]
    	deg1 <- deg + 1
    	if(!intercept)
    		coefs <- c(0, coefs)
    	if(length(coefs) != deg1)
    		stop("wrong length for coefs")
    	polycoefs <- attr(polymat, "coefs")
    	if(is.null(polycoefs))
    		stop("not enough information available")
    	alpha <- polycoefs$alpha
    	norm2 <- polycoefs$norm2
    	polynoms <- vector("list", deg + 1)
    	polynoms[[1]] <- c(1, rep(0, deg))
    	for(i in 1:deg) {
    		pi <- polynoms[[i]]
    		temp <- c(0, pi[ - deg1]) - alpha[i] * pi
    		if(i > 1)
    			temp <- temp - norm2[i + 1]/norm2[i] * polynoms[[i - 1]]
    		polynoms[[i + 1]] <- temp
    	}
    	polynoms
    	outmat <- coefs/c(1, sqrt(norm2[-1:-2])) * do.call("rbind", polynoms)
    	ans <- colSums(outmat)
    	names(ans) <- paste("x^", 0:deg, sep = "")
    	ans
    }
  }

  # initialize variables
  supported  <- c("lm", "lmRobMM", "ltsreg", "lmsreg", "l1fit", "rreg")
  length.min <- c(3, 1, 7, 7, 4, 3)
  coeffnames <- c("u", "t0", "a", "b")
  mzformula  <- formula(mz ~ u * (sign(t-t0) * a * (t-t0)^2 + b))

  # check input arguments
  if (is(mz, "msCalibrate"))
    return(mz)
  if (digits < 0)
    stop("digits must be non-negative")

  if (is.list(mz) || is(mz, "named") || !is.null(names(mz)) ){

    parameters <- unlist(mz)

    if (!all(is.element(coeffnames, names(parameters))))
      stop(c("Input list or name vector must have names:",
        coeffnames),collapse=" ")

    z <- list(coefficients=parameters[coeffnames], tof=tof,
      formula=mzformula)
    oldClass(z) <- "msCalibrate"

    return(predict(z, digits=digits))
  }

  if (!isVectorAtomic(mz))
    stop("mz be be a numeric vector")
  if (!isVectorAtomic(tof))
    stop("tof be be a numeric vector")
  if (length(u) > 1 || !is.numeric(u) || u <= 0 )
    stop("u must be a positive numeric scalar")
  if (!is.character(FUN))
    stop("FUN must be a character string")

  FUN <- match.arg(FUN, supported)
  minpts <- length.min[match(FUN,supported)]

  if (numRows(mz) != numRows(tof))
    stop("Lengths of mz and tof vectors must be the same")
  if (length(mz) < minpts)
    stop(FUN, " requires a minimum of ", as.character(minpts), "points")

  # convert inputs into a data.frame
  x <- data.frame(mz, tof)

  # quadratic fitting
  fit <- switch(FUN,
    lm      = lm(mz~poly(tof, 2), data=x),
    lmRobMM = lmRob(mz~poly(tof, 2), data=x),
    ltsreg  = ltsreg(mz~poly(tof, 2), data=x),
    lmsreg  = lmsreg(mz~poly(tof, 2), data=x),
    l1fit   = l1fit(y=mz, x=poly(tof, 2)),
    rreg    = rreg(y=mz, x=poly(tof, 2)))

  # recover the coefficient from the orthogonal to simple polynomial form
  # mz = coefficients[1] + coefficients[2]*tof + coefficients[3]*tof^2
  coefficients <- poly.transform(poly(tof, 2), coef(fit))

  # compute Ciphergen calibration parameters
  # mz = u*(a(tof-t0)^2 + b)
  t0 <- -coefficients[2] / (2*coefficients[3])
  a	 <- coefficients[3] / u
  b	 <- coefficients[1] / u - a*t0^2
  parameters <- c(u, t0, a, b)
  names(parameters) <- coeffnames

  z <- list(coefficients=parameters, tof=tof, formula=mzformula, FUN=FUN)
  oldClass(z) <- "msCalibrate"

  # add predicted mz values
  if (predict.mz)
    z$mz <- predict(z, digits=digits)

  eventHistory(z, msCalibrate=list(FUN=FUN, coefficients=parameters))
}

################################################
# Class Methods
################################################

###
# coef.msCalibrate
###

"coef.msCalibrate" <- function(object, ...) object$coefficients

###
# formula.msCalibrate
###

"formula.msCalibrate" <- function(x, ...) x$formula

###
# plot.msCalibrate
###

"plot.msCalibrate" <- function(x, type="b",
  xlab="tof", ylab="m/z", add=FALSE, ...)
{
  if (!add && !all(par("mfrow")==c(1,1))){
    old.par <- par()
    on.exit(par(old.par))
    par(mfrow=c(1,1))
  }

  if (is.null(x$mz))
    stop("Predicted m/z values are not available")

  plot(x$tof, x$mz, type=type, xlab=xlab, ylab=ylab, ...)

  invisible(NULL)
}

###
# predict.msCalibrate
###

"predict.msCalibrate" <- function(object, newtof = object$tof, digits=4, ...)
{

  if (!isVectorAtomic(newtof) || !is.numeric(newtof))
    stop("newtof must be a vector of numeric values")

  data <- c(as.list(coef(object)), list(t=newtof))
  Formula <- formula(object)

  round(eval(Formula[[3]], data),digits)
}

###
# print.msCalibrate
###

"print.msCalibrate" <- function(x, justify="left", sep=":", ...)
{
  main <- "Mass Calibration Object"
  cat(main,"\n")
  cat(rep("-",nchar(main)),"\n",sep="")
  cat("Formula:\n");print(formula(x))
  cat("\nCoefficients:\n");print(coef(x));cat("\n")

  categories <- c(
   "Regression function",
   "TOF range",
   "m/z range")

  values <- list(
    x$FUN,
    range(x$tof),
    range(x$mz))

  categories <- format(categories, justify=justify)

  for (i in seq(along=categories))
    cat(categories[i], sep, values[[i]], "\n", sep=" ")

  invisible(x)
}
