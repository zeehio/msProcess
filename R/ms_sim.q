################################################
## S+Proteome mass spectrometry simulator
##
##  old (S3) classes:
##    lm
##
##  new (S4) classes:
##    proteins
##    calibrants
##    setting
##    calibrator
##    spectrometer
##    spectrum
##
##  generating functions:
##    proteins
##    calibrants
##    spectrometer
##
##  ordinary functions:
##    ion.focus.delay
##
##  generic functions:
##    run
##
##  methods and group generics:
##    show("proteins")
##    "["("proteins")
##    "[<-"("proteins")
##    "[<-"("proteins", "proteins")
##    Math("proteins")*
##    Math2("proteins")*
##    Summary("proteins")
##    Arith("proteins", "proteins")
##    Arith("proteins", "integer")
##    Arith("numeric", "proteins")
##    Arith("proteins")*
##    Compare("proteins", "proteins")
##    Compare("proteins", "integer")
##    Compare("numeric", "proteins")
##    Logic(e1="proteins")*
##    Logic(e2="proteins")*
##
##    show("setting")
##    run("spectrometer", "calibrants")
##    run("spectrometer", "proteins")
##    xyCall("spectrum", "missing")
##    xyCall("proteins", "missing")
##
################################################

################################################
##  old (S3) classes
################################################

###
# lm
###

setOldClass("lm")

################################################
##  new (S4) classes
################################################

###
# proteins -- protein mixtures or samples
###

setClass("proteins",
  representation(
    masses = "numeric",
    counts = "integer"),
  validity = function(object)
  {
    masses <- object@masses
    counts <- object@counts
    length.masses <- length(masses)
    length.counts <- length(counts)

    if (length.masses != length.counts)
      stop("length of masses != length of counts")
    if (!is.numeric(masses) || any(masses <= 0))
      stop("masses should be a vector of positive numerics")
    if (!is.integer(counts) || any(counts <= 0))
      stop("counts should be a vector of positive integers")  
    if (any(duplicated(masses)))
      stop("masses should be unique")
      
    return(TRUE)
  })

###
# calibrants -- protein calibrants
###

setClass("calibrants", "proteins")

###
# setting -- mass spectrometer settings
###

# TODO: only the first 6 parameters affect calibration,
# so changing the last 3 parameters won't affect the calibrator.

setClass("setting",
  representation(
    # unchanging parameters
    dist.drift = "numeric",  # length of drift tube in meters
    dist.focus = "numeric",  # distance between charged grids in millimeters
    dist.accel = "numeric",  # distance from sample plate to first grid in millimeters
    # parameters with direct control
    volt.accel = "numeric",  # voltage between charged grids in volts
    volt.focus = "numeric",  # voltage used in ion focusing phase in volts
    time.delay = "numeric",  # delay time before focus voltage is applied in nanoseconds
    time.resol = "numeric",  # time between detector records in seconds
    # parameters with indirect control, attribute this to the laser
    vel0.mean  = "numeric",  # mean initial velocity in meters/second
    vel0.std   = "numeric")) # standard deviation of initial velocity
  
###
# calibrator -- mass calibrator
###

setClass("calibrator",
  representation(
    time.mean = "numeric",
    model     = "lm",
    error.rel = "numeric"))

###
# spectrometer -- mass spectrometer
###

setClass("spectrometer",
  representation(
    setting    = "setting",
    calibrator = "calibrator"))

###
# spectrum -- mass spectrum
###

setClass("spectrum",
  representation(
    tof       = "numeric",
    mz        = "numeric",
    intensity = "integer"))

################################################
##  generating/generator functions
################################################

###
# proteins
###

"proteins" <- function(masses, counts)
{
  index <- order(masses)
  
  object <- new("proteins",
    masses = masses[index],
    counts = counts[index])

  validObject(object)

  return(object)
}


###
# calibrants
###

"calibrants" <- function(masses, counts)
{
  index <- order(masses)
  
  object <- new("calibrants",
    masses = masses[index],
    counts = counts[index])

  validObject(object)

  return(object)
}

###
# spectrometer
###

"spectrometer" <- function(
  dist.drift = 1,     # length of drift tube in meters
  dist.focus = 17,    # distance between charged grids in millimeters
  dist.accel = 8,     # distance from sample plate to first grid in millimeters
  volt.accel = 20000, # voltage between charged grids in volts
  volt.focus = 2000,  # voltage used in ion focusing phase in volts
  time.delay = 600,   # delay time before focus voltage is applied in nanoseconds
  time.resol = 4e-9,  # time between detector records in seconds
  vel0.mean  = 350,   # mean initial velocity in meters/second
  vel0.std   = 50,    # standard deviation of initial velocity
  time.mean  = numeric(0), # calibrator time.mean
  model      = structure(NULL, class="lm"),      # calibrator lm model
  error.rel  = numeric(0)) # calibrator error.rel
{
  new("spectrometer",
    setting = new("setting",
      dist.drift = dist.drift,
      dist.focus = dist.focus,
      dist.accel = dist.accel,
      volt.accel = volt.accel,
      volt.focus = volt.focus,
      time.delay = time.delay,
      time.resol = time.resol,
      vel0.mean  = vel0.mean,
      vel0.std   = vel0.std),
    calibrator = new("calibrator",
      time.mean  = time.mean,
      model      = model,
      error.rel  = error.rel))
}

################################################
##  ordinary functions
################################################

###
# ion.focus.delay
###

"ion.focus.delay" <- function(mass, v0, setting)
{  
  # mass    = vectors of masses in daltons
  # v0      = matching vector of initial velocities in meters/second
  # setting = object containing the machine setting

  # first we define the constants we need to convert units
  millimeters <- 1/1000            # convert from mm to meters
  nanoseconds <- 1e-09             # convert from ns to seconds
  z           <- 1.602e-019        # charge in coulombs on one electron or proton
  avogadro    <- 6.023e+023        # the number of molecules in one mole
  daltons     <- 1/(1000*avogadro) # amount of kilograms per one dalton

  # next, we extract the parameters and convert them to standard units
  L     <- setting@dist.drift
  D2    <- setting@dist.accel * millimeters
  V     <- setting@volt.accel
  D1    <- setting@dist.focus * millimeters
  V1    <- setting@volt.focus
  delta <- setting@time.delay * nanoseconds
  m     <- mass * daltons

  # now we perform the actual computations
  x0         <- delta*v0 # position at end of delay period
  v1         <- sqrt(v0^2 + 2*z*V1*(D1 - x0)/(m*D1)) # eqn (1.6)
  time.drift <- sqrt(L^2/(2*z*V/m + v1^2))           # eqn (1.4 or 1.7)
  time.accel <- m*D2/(z*V)*(L/time.drift - v1)       # eqn (1.8)
  time.focus <- m*D1*(v1 - v0)/(z*V1)                # eqn (1.9)
  time.total <- delta + time.focus + time.accel + time.drift

  return(time.total)
}

################################################
##  generic functions
################################################

###
# run
###

setGeneric("run",
  function(simObj=spectrometer(), proObj, isotope=TRUE)
    standardGeneric("run"))

################################################
##  methods
################################################

###
# show("proteins")
###

setMethod("show", "proteins",
  function(object)
  {
    vals <- list()
    for (name in slotNames(object)) 
      vals[[name]] <- slot(object, name)
      
    cat("An object of class \"", class(object), "\"\n\n", sep="")
    show(data.frame(vals))
  }) 

###
# "["("proteins")
###

# note: the prototype for "[" is different for S-PLUS and R

if (is.R()) {
setMethod("[", "proteins",
  function(x, i, j, ..., drop=FALSE)
  {
    x@masses <- x@masses[i, ..., drop=drop]
    x@counts <- x@counts[i, ..., drop=drop]
    return(x)
  }) 	
} else {
setMethod("[", "proteins",
  function(x, ..., drop=FALSE)
  {
    x@masses <- x@masses[..., drop=drop]
    x@counts <- x@counts[..., drop=drop]
    return(x)
  }) 	
}

###
# "[<-"("proteins")
###

# note: the prototype for "[<-" is different for S-PLUS and R

if (is.R()) {
setReplaceMethod("[", "proteins",
  function(x, i, j, ..., value)
  {
    # work around bug that x@counts[...] <- as(value, "integer") 
    #   is considered invalid
    tmp <- x@counts
    tmp[i, ...] <- as(value, "integer")
    x@counts <- tmp
    return(x)
  })
} else {
setReplaceMethod("[", "proteins",
  function(x, ..., value)
  {
    # work around bug that x@counts[...] <- as(value, "integer") 
    #   is considered invalid
    tmp <- x@counts
    tmp[...] <- as(value, "integer")
    x@counts <- tmp
    return(x)
  })
}

###
# "[<-"("proteins", "proteins")
###

if (is.R()) {
setReplaceMethod("[", c(x="proteins", value="proteins"),
  function(x, i, j, ..., value)
  {
    masses.old <- x@masses[i, ...]
    repl <- match(masses.old, value@masses)
    if (any(is.na(repl)))
      stop("some of the proteins to be replaced are not in the replacement: ",
        paste(masses.old[is.na(repl)], ", "))

    # work around bug that x@counts[...] <- value@counts[repl] 
    #   is considered invalid   
    tmp <- x@counts
    tmp[i, ...] <- value@counts[repl]
    x@counts <- tmp
    
    return(x)
  }) 
} else {
setReplaceMethod("[", c(x="proteins", value="proteins"),
  function(x, ..., value)
  {
    masses.old <- x@masses[...]
    repl <- match(masses.old, value@masses)
    if (any(is.na(repl)))
      stop("some of the proteins to be replaced are not in the replacement: ",
        paste(masses.old[is.na(repl)], ", "))

    # work around bug that x@counts[...] <- value@counts[repl] 
    #   is considered invalid   
    tmp <- x@counts
    tmp[...] <- value@counts[repl]
    x@counts <- tmp
    
    return(x)
  }) 
}

###
# Math -- group generic
###

setMethod("Math", "proteins",
  function(x) 
    stop("Math group generics are meaningless for proteins"))

###
# Math2 -- group generic
###

setMethod("Math2", "proteins",
  function(x, digits) 
    stop("Math2 group generics are meaningless for proteins"))

###
# Summary -- group generic
###

setMethod("Summary", "proteins",
  function(x, ..., na.rm=FALSE)
  {
    masses.summ <- callGeneric(x@masses, na.rm=na.rm)
    counts.summ <- callGeneric(x@counts, na.rm=na.rm)
    vals <- c(masses=masses.summ, counts=counts.summ)
    return(vals)
  })
  
###
# Arith("proteins", "proteins") -- group generic
###

setMethod("Arith", c("proteins", "proteins"),
  function(e1, e2)
  {   
    if (.Generic == "+") {
      index <- match(e2@masses, e1@masses, nomatch=0)
      
      e1@counts[index] <- e1@counts[index] + e2@counts[index!=0]
      
      e1@masses <- c(e1@masses, e2@masses[index==0])
      e1@counts <- c(e1@counts, e2@counts[index==0])

      tmpargs <- list(masses=e1@masses, counts=e1@counts)
      object <- do.call(class(e1), tmpargs)
      return(object)
    }
    else if (.Generic == "-") {
      index <- match(e2@masses, e1@masses, nomatch=0)
      
      if (any(index==0))
        stop(deparse(substitute(e2)), " has proteins that are not in ",
        deparse(substitute(e1)))
          
      e1@counts[index] <- e1@counts[index] - e2@counts
      
      return(e1[e1@counts!=0])
    }
    else stop("Arith (\"", .Generic, "\") not meaningful for proteins data")
  })

###
# Arith("proteins", "numeric") -- group generic
###

setMethod("Arith", c("proteins", "integer"),
  function(e1, e2)
  {
    e1@counts <- callGeneric(e1@counts, e2)  
    return(e1[e1@counts!=0])
  })

###
# Arith("numeric", "proteins") -- group generic
###

setMethod("Arith", c("numeric", "proteins"),
  function(e1, e2)
  {
    e2@masses <- callGeneric(e1, e2@masses)
    index <- e2@masses!=0
    tmpargs <- list(masses=e2@masses[index], counts=e2@counts[index])
    object <- do.call(class(e2), tmpargs)
    return(object)
  })

###
# Arith("proteins") -- group generic
###

setMethod("Arith", c("proteins", "missing"),
  function(e1, e2) 
    stop("unary arithmetic operators are meaningless for proteins"))

###
# Compare("proteins", "proteins") -- group generic
###

setMethod("Compare", c("proteins", "proteins"),
  function(e1, e2)
  {
    switch (.Generic,
      "==" = return(identical(e1, e2)),
      "!=" = return(!identical(e1, e2)))
    
    stop("comparison (\"", .Generic, "\") not meaningful for proteins")    
  })

###
# Compare("proteins", "integer") -- group generic
###

setMethod("Compare", c("proteins", "integer"),
  function(e1, e2)
  {
    callGeneric(e1@counts, e2)   
  })
  
###
# Compare("numeric", "proteins") -- group generic
###

setMethod("Compare", c("numeric", "proteins"),
  function(e1, e2)
  {
    callGeneric(e1, e2@masses)
  })  

###
# Logic(e1="proteins") -- group generic
###

setMethod("Logic", c(e1="proteins"),
  function(e1, e2) 
    stop("Logical operators are meaningless for proteins data"))

###
# Logic(e2="proteins") -- group generic
###

setMethod("Logic", c(e2="proteins"),
  function(e1, e2) 
    stop("Logical operators are meaningless for proteins data"))

###
# show("setting")
###

setMethod("show", "setting",
  function(object)
  {
    len <- length(object)
    nam <- slotNames(object)
    
    vals <- vector("numeric", len)

    for (i in seq(len)) 
      vals[i] <- slot(object, nam[i])
      
    nam.vals <- format(c(nam, vals))
    cat("An object of class \"", class(object), "\"\n\n", sep="")
    
    cat(nam.vals[1:len], "\n", nam.vals[-(1:len)])
  }) 

###
# run("spectrometer", "calibrants")
###

setMethod("run", c("spectrometer", "calibrants"),
  function(simObj, proObj, isotope)
  {
    # get the times, for this instrument, for the true masses
    masses <- proObj@masses
    counts <- proObj@counts
    
    time.flight <- unlist(lapply(seq(length(masses)), 
      function(i, masses, counts, setting) 
      {
        v0 <- rnorm(counts[i], setting@vel0.mean, setting@vel0.std)
        median(ion.focus.delay(rep(masses[i], counts[i]), v0, setting))
      }, 
      masses=masses, counts=counts, setting=simObj@setting))
            
    # center the times to stabilize the coeficients of the model
    time.mean     <- mean(time.flight)
    time.centered <- time.flight - time.mean
    
    # model mass as a quadrtatic function of time
    model <- lm(masses ~ time.centered + I(time.centered^2))
    
    # absolute relative error at calibration positions
    error.rel <- abs(model$residuals/masses)
    
    simObj@calibrator <- new("calibrator", time.mean=time.mean, model=model,
      error.rel=error.rel)  
    
    return(simObj)
  })

###
# run("spectrometer", "proteins")
###
  
setMethod("run", c("spectrometer", "proteins"),
  function(simObj, proObj, isotope)
  {
    masses <- proObj@masses
    counts <- proObj@counts

    if (isotope) {
      # most atoms in a protein are carbon (weight 12-13) or nitrogen (14-15)
      # with a few heavier atoms. So, we approximate the number of atoms.
      n.atoms <- 1 + trunc(masses/15)
      
      # we model the isotope distribution as a binomial distribution.
      # The parameter 0.985 is a crude approximation based on experimental
      # data from NIST on the relative abundance of C13/C12 and N15/N14.
      isotopes <- unlist(lapply(seq(length(counts)),
        function(i, np, na) rbinom(np[i], na[i], 1-0.985),
        np=counts, na=n.atoms))
      masses <- isotopes - median(isotopes) + rep(masses, counts)
    }
    
    # we also simulate a normal distribution of initial velocities,
    # which is assumed to be independent of the mass of the particle.
    setting <- simObj@setting
    v0 <- rnorm(sum(counts), setting@vel0.mean, setting@vel0.std)
    time.resol <- setting@time.resol

    # now we compute the times, given the isotope-spread masses and
    # the random initial velocities
    time.list <- ion.focus.delay(masses, v0, setting)
    
    # finally, we use the time resolution of the detector to
    # bin the observations
    start <- floor(min(time.list)/time.resol) - 1
    finish <- ceiling(max(time.list)/time.resol) + 1
    breaks <- time.resol*(start:finish)
    times <- (breaks[1:(length(breaks) - 1)] + breaks[2:length(breaks)])/2
    intensity <- hist(time.list, breaks=breaks, plot=FALSE)$counts

    object <- new("spectrum", intensity=intensity, tof=times)   
    calibrator <- simObj@calibrator

#    if (!is.null(calibrator@model))
    if (length(calibrator@model)!=0)
      object@mz <- predict(calibrator@model, 
        data.frame(time.centered=times-calibrator@time.mean))

    return(object)
  })

###
# xyCall("spectrum", "missing")
###

# note: xyCall make all functions that naturally relate to points in the plane 
# work with objects of defined classes, including plot, lines, points, and etc. 
# It does not exist in R.

if (is.R()) {
setMethod("plot", c("spectrum", "missing"),
  function(x, y, ...)
  {
  	plot(if (length(x@mz)!=0) x@mz else x@tof, x@intensity, ...)
  })	
} else {
setMethod("xyCall", c("spectrum", "missing"),
  function(x, y, FUN, ..., xexpr, yexpr)
  {  
    dots <- list(...)
    type <- ifelse1(hasArg(type), dots$type, "l")
    
    if (missing(xexpr)) {
      FUN(ifelse1(length(x@mz)!=0, x@mz, x@tof), x@intensity, type=type, ...)
    }
    else {
      xlab <- ifelse1(hasArg(xlab), dots$xlab, length(x@mz)!=0, "m/z", "tof")
      ylab <- ifelse1(hasArg(ylab), dots$ylab, "intensity")
      main <- ifelse1(hasArg(main), dots$main, deparseText(xexpr))
      FUN(ifelse1(length(x@mz)!=0, x@mz, x@tof), x@intensity, 
        xlab=xlab, ylab=ylab, main=main, type=type, ...)
    }
  })
}

###
# xyCall("proteins", "missing")
###

if (is.R()) {
setMethod("plot", c("proteins", "missing"),
  function(x, y, ...)
  {
  	plot(x@masses, x@counts, ...)
  })
} else {  
setMethod("xyCall", c("proteins", "missing"),
  function(x, y, FUN, ..., xexpr, yexpr)
  {
    dots <- list(...)
    type <- ifelse1(hasArg(type), dots$type, "l")    
    
    if (missing(xexpr)) FUN(x@masses, x@counts, type=type, ...)
    else {
      xlab <- ifelse1(hasArg(xlab), dots$xlab, "mass")
      ylab <- ifelse1(hasArg(ylab), dots$ylab, "abundance")
      main <- ifelse1(hasArg(main), dots$main, deparseText(xexpr))
      FUN(x@masses, x@counts, xlab=xlab, ylab=ylab, main=main, type=type, ...)
    }
  })
}
