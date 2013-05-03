%%  WARNING: This file was automatically generated from the associated
%%  ms_noise.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_noise.mid file in the project MID directory. Once the
%%  ms_noise.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msDenoise function

\name{msDenoise}
\alias{msDenoise}
\title{Mother Function for Mass Spectra Denoising}
\concept{noise reduction}
\usage{msDenoise(x, FUN="wavelet",
    attach.noise=TRUE, event="Denoising", ...)}
\description{Denoise spectra with various functions.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{...}{Additional arguments to \code{FUN}.
They are passed unchanged to each call of \code{FUN}
and include their names.
See the help documentation of the specified FUN for details.}

\item{FUN}{Either an object of class \code{"character"}
or of class \code{"function"}.

character: A character string denoting the
method to use in denoising the data.
Supported choices are \code{"wavelet"} for wavelet shrinkage,
\code{"mrd"} for partial summation of a wavelet-based
multiresolution decomposition,
and \code{"smooth"} for robust running medians.
Default: \code{"wavelet"}.

function: A user-defined function with an argument list
of the form \code{(x, ...)} where \code{x} is a required argument
corresponding to a numeric vector (typically these values
will be the intensity values of a mass spectrum).

In either case, the additional arguments \code{...} will be
passed directly to the specified FUN.}

%\item{MARGIN}{The subscripts over which the function is to be applied
%to the data defined by \code{type}.
%For example, if \code{type="intensity"}, the  data is
%defined by the intensity matrix of \code{x} and \code{MARGIN=1}
%indicates rows while \code{MARGIN=2} indicates columns.
%Default: 2 (\code{FUN} will operate over each column
%of the data).}

\item{attach.noise}{A logical indicating if the noise removed
should be attached or not. Default: \code{TRUE}.}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Denoising"}.}

%\item{type}{A character string defining the type
%of data matrix to extract from \code{x} and operate over.
%The operative data will be \code{x[[type]]} if it exists or, if it does
%not, \code{attr(x, type)} will be used. If neither exists,
%an error is returned. Default: \code{"intensity"}.}
}

\value{
An object of class \code{msSet},
optionally, with the estimated noise attached as element \code{"noise"}.
}
\note{If \code{FUN="mrd"}, an \code{mrd} object containing meta information regarding
the multiresolution decomposition is attached to the \code{msSet} output object for subsequent
use by other MRD-based function calls such as \code{msPeak(x, FUN="mrd", ...}.}

\seealso{
\code{\link{msDenoiseSmooth}}, \code{\link{msDenoiseWavelet}}, \code{\link{msDenoiseMRD}}, \code{\link{matchObject}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## denoise a spectrum portion via waveshrink and a
## smoothing function
mz   <- (qcset$mz > 3000 & qcset$mz < 5000)
data <- qcset[mz, 1, drop=FALSE]

## add a little Gaussian noise for illustration
noise  <- rnorm(length(data$intensity), sd=stdev(data$intensity)/3)
xnoise <- data
xnoise$intensity <- data$intensity +
    matrix(noise, ncol=1)

## denoise using the supported routines
z1 <- msDenoise(xnoise, FUN="wavelet")
z2 <- msDenoise(xnoise, FUN="smooth")
z3 <- msDenoise(xnoise, FUN="mrd", levels=4:6)

## create a user-defined (albeit naive) denoising
## function
my.fun <- function(x, wavelet="d4"){
    filt <- wavDaubechies(wavelet=wavelet, norm=FALSE)$scaling
    return(filter(x, filt))
}
z4 <- msDenoise(xnoise, FUN=my.fun, wavelet="s12")

## create a stackplot of the results
z <- list(original=data$intensity[,1],
    noisy=xnoise$intensity[,1],
    waveshrink=z1$intensity[,1],
    smooth=z2$intensity[,1],
    mrd=z3$intensity[,1],
    "my function"=z4$intensity[,1])
wavStackPlot(z, col=seq(along=z), same.scale=TRUE)
}
\keyword{manip}

