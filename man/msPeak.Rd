%%  WARNING: This file was automatically generated from the associated 
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_peak.mid file in the project MID directory. Once the 
%%  ms_peak.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msPeak function

\name{msPeak}
\alias{msPeak}
\title{Mother Function for Peak Detection}
\concept{peak detection}
\usage{msPeak(x, FUN="simple",
    use.mean=FALSE, event="Peak Detection", ...)}
\description{This function detects peaks in a set of mass spectra.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{...}{Additional arguments for the FUN specified.
See the specific routine for details.}

\item{FUN}{A character string specifying the method for peak detection.
Possible choices are \code{"simple"}, \code{"search"}, \code{"cwt"} and \code{"mrd"}. In the
MRD case, the input \code{msSet} object is expected to contain
an attached \code{mrd} object containing meta information regarding
the wavelet-based multiresolution decomposition as output by
\code{msDenoise(x, FUN="mrd", ...)} as an example. See the
\code{msPeakMRD} function for more details.
Default: \code{"simple"}.}

%\item{MARGIN}{The subscripts over which the function is to be applied
%to the data defined by \code{type}.
%Default: 2 (FUN will operate over each column of the data).}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Peak Detection"}.}

%\item{type}{A character string defining the type
%of data matrix to extract from \code{x} and operate over.
%The operative data will be \code{x[[type]]} if it exists.
%Default: \code{"intensity"}.}

\item{use.mean}{A logical value specifying if to detect peaks in the mean spectum.
Default: \code{FALSE}.}
}

\value{
An object of class \code{msSet} with elements depending on the value of use.mean:

\item{\code{use.mean==TRUE}}{the mean spectrum is attached as element
\code{"intensity.mean"}
(along with \code{"noise.mean"} and \code{"noise.local.mean"})
the peak info is attached as element \code{"peak.class"},
and the argument \code{"use.mean"} is attached as element \code{"use.mean"}.
The element \code{"peak.class"} is
a matrix with peak classes as rows and some summary statistics as columns.
These statistics include the location, left bound, right bound, and
span of each peak class in both clock tick
(\code{"tick.loc"}, \code{"tick.left"},
\code{"tick.right"}, \code{"tick.span"}) and mass measure
(\code{"mass.loc"}, \code{"mass.left"},
\code{"mass.right"}, \code{"mass.span"}).}

\item{\code{use.mean==FALSE}}{the peak info is attached as element
\code{"peak.list"}. The element \code{"peak.list"} is a list
with one element for each spectrum. Each element is a data.frame
with 10 columns: the location, left bound, right bound, and
span of each peak in both clock tick
(\code{"tick.loc"}, \code{"tick.left"},
\code{"tick.right"}, \code{"tick.span"}) and mass measure
(\code{"mass.loc"}, \code{"mass.left"},
\code{"mass.right"}, \code{"mass.span"}),
and also peak signal-to-noise ratio and intensity
(\code{"snr"}, \code{"intensity"}).}
}
\seealso{
\code{\link{msPeakSimple}}, \code{\link{msPeakSearch}}, \code{\link{msPeakMRD}}, \code{\link{msNormalize}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## extract several spectra from the build-in 
## dataset 
z <- qcset[, 1:8]

## denoising 
z <- msDenoise(z, FUN="wavelet", n.level=10, thresh.scale=2)

## local noise estimation 
z <- msNoise(z, FUN="mean")

## baseline subtraction 
z <- msDetrend(z, FUN="monotone", attach=TRUE)

## intensity normalization based on total ion 
## current 
z <- msNormalize(z, FUN="tic")

## peak detection 
z <- msPeak(z, FUN="simple", use.mean=FALSE, snr=2)

## visualize the detected peaks 
plot(z, process="msPeak", subset=1:8, offset=100,
    xlim=c(13000, 17000))

## perform a similar analysis using a 
## multiresolution decomposition approach 
z <- qcset[, 1:8]
z <- msDenoise(z, FUN="mrd", levels=6, keep.smooth=FALSE)
z <- msPeak(z, FUN="mrd")
plot(z, process="msPeak", subset=1:8, offset=100,
    xlim=c(13000, 17000))

## perform a similar analysis using a CWT approach 
# z <- qcset[, 1:8]
# z <- msPeak(z, FUN="cwt", scale.min=8)
# plot(z, process="msPeak", subset=1:8, offset=100,
# 	    xlim=c(13000, 17000))
}
\keyword{manip}

