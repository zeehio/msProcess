%%  WARNING: This file was automatically generated from the associated
%%  ms_noise.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_noise.mid file in the project MID directory. Once the
%%  ms_noise.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msNoise function

\name{msNoise}
\alias{msNoise}
\title{Local Noise Estimation}
\concept{noise estimation}
\usage{msNoise(x, FUN="spline",
    pre=abs, detach.noise=FALSE,
    event="Local Noise Estimation", ...)}
\description{Estimates the local noise level by applying
a specified smoother function to the the noise data attached
to the primary input variable.}

\arguments{
\item{x}{An object of class \code{msSet} with an existing element \code{"noise"}.
The noise element should be a matrix with the same dimensions
as the intensity data, such as that returned by the \code{msDenoise}
function.}
\item{...}{Additional arguments to \code{FUN}.
They are passed unchanged to each call of \code{FUN}
and include their names.
See the help documentation of the specified routine for details.}

\item{FUN}{Either an object of class \code{"character"}
or of class \code{"function"}.

character: A character string denoting the
method to use in smoothing the noise data.
Supported choices are \code{"spline"}, \code{"supsmu"}, \code{"ksmooth"},
\code{"loess"}, and \code{"mean"}. Default: \code{"spline"}.

function: A user-defined function with an argument list
of the form \code{(x, ...)} where \code{x} is a required argument
corresponding to a numeric vector (typically these values
will be the noise estimates of a mass spectrum).

In either case, the additional arguments \code{...} will be
passed directly to the specified routine.}

%\item{MARGIN}{The subscripts over which the function is to be applied
%to the data defined by \code{type}.
%For example, if \code{type="intensity"}, the  data is
%defined by the intensity matrix of \code{x} and \code{MARGIN=1}
%indicates rows while \code{MARGIN=2} indicates columns.
%Default: 2 (\code{FUN} will operate over each column
%of the data).}

\item{detach.noise}{A logical indicating if the noise removed previously
should be detached or not. Default: \code{FALSE}.}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Local Noise Estimation"}.}

\item{pre}{A function that is applied to the data prior to
processing it with the function defined by \code{FUN}. Default: \code{abs}.}

%\item{type}{A character string defining the type
%of data matrix to extract from \code{x} and operate over.
%The operative data will be \code{x[[type]]} if it exists.
%Default: \code{"noise"}.}
}

\value{
An object of class \code{msSet} with local noise estimate attached
as element \code{"noise.local"}
and optionally with the element \code{"noise"} detached.
}
\seealso{
\code{\link{msDenoise}}, \code{\link{msSet}}, \code{\link[ifultools]{properCase}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## denoise a noise contaminated spectrum portion
## via waveshrink
mz     <- (qcset$mz > 3000 & qcset$mz < 5000)
data   <- qcset[mz, 1, drop=FALSE]
noise  <- rnorm(length(data$intensity), sd=stdev(data$intensity)/3)
xnoise <- data

xnoise$intensity <- data$intensity + matrix(noise, ncol=1)
z <- vector("list", length=4)
z[[1]] <- msDenoise(xnoise, FUN="wavelet")

## smooth the resulting noise estimates to form a
## localized estimate of the noise using
## various supported methods

z[[2]] <- msNoise(z[[1]], FUN="spline")
z[[3]] <- msNoise(z[[1]], FUN="loess")

## create a user-defined smoothing function
my.fun <- function(x, wavelet="d4"){
    filt <- wavDaubechies(wavelet=wavelet, norm=FALSE)$scaling
    return(filter(x, filt))
}
z[[4]] <- msNoise(z[[1]], FUN=my.fun, wavelet="s12")

## create a stackplot of the results
type <- c("noise", rep("noise.local", 3))
for (i in 1:4){
    z[[i]] <- as.vector(z[[i]][[type[i]]])
}
names(z) <- c("noise","spline", "loess", "my function")
wavStackPlot(z, col=seq(along=z), same.scale=TRUE)
}
\keyword{manip}

