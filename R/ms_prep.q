################################################
## S+Proteome data preparation functions
##
##  msPrepare
##
################################################

###
# msPrepare
###

# TODO: deal with spectra whose mz values are not distinct
# TODO: decide to interpolate to constant time step or constant mass step

"msPrepare" <- function(x, mass.min=1500, transform=NULL, data.name=NULL)
{
  # obtain data name
  if (is.null(data.name))
    data.name <- deparseText(substitute(x))

  # truncate the spectra to the mass range of interest
  n.spectra <- length(x)
  for (i in seq(n.spectra)) {
    mz.index <- x[[i]][, "mz"] >= mass.min
    x[[i]] <- x[[i]][mz.index, ]
  }

  # use the mz's from the first spectrum as a common set and
  # interpolate the rest to this common set if necessary
  mz        <- x[[1]][, "mz"]
  intensity <- x[[1]][, "intensity"]

  if (n.spectra > 1){

    for (i in seq(2, n.spectra)){

      my.mz  <- x[[i]][, "mz"]
      my.int <- x[[i]][, "intensity"]

      if (length(my.mz)!=length(mz) || any(my.mz!=mz)){
        my.int <- approx(x=my.mz, y=my.int, xout=mz,
        method="linear", rule=2, f=0.5)$y
      }

      intensity <- cbind(intensity, my.int)
    }
  }

  # apply transform to intensity if specified, which may stablize noise
  # The transformation could be logarithmic, square root, cube root, and etc.
  if (!is.null(transform)){
    if (!is.function(transform))
      stop("transform is not a function.")
    intensity <- transform(intensity)
  }
  dimnames(intensity) <- list(mz, names(x))

  # compose the output as a msSet object
  z <- msSet(as.matrix(intensity), mz=as.vector(mz),
    type=as.factor(attr(x, "type")), data.name=data.name)
  attr(z, "transform") <- transform

  z
}
