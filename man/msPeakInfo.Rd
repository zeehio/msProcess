%%  WARNING: This file was automatically generated from the associated 
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_peak.mid file in the project MID directory. Once the 
%%  ms_peak.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msPeakInfo function

\name{msPeakInfo}
\alias{msPeakInfo}
\title{Peak Detection Constructor Function}
\concept{peak detection}
\usage{msPeakInfo(x, y, index.min, index.max, noise.local = NULL,
 snr.thresh = 2)}
\description{Provides a common interface for packing detected mass spectrometry
peaks into an appropriate output object.}

\arguments{
\item{x}{A numeric vector representing the \code{m/z} values of a spectrum.}

\item{y}{A numeric vector representing the intensity values of the spectrum.}

\item{index.max}{A logical vector the size of the original mass spectrum. If the
kth element is \code{TRUE}, it indicates that the corresonding kth element of the
original mass spectrum is a local maxima.}

\item{index.min}{A logical vector the size of the original mass spectrum. If the
kth element is \code{TRUE}, it indicates that the corresonding kth element of the
original mass spectrum is a local minima. It is assumed that for each peak (identified
as a \code{TRUE} element in \code{index.max}) there exists two minima which encompass
the peak in time. Hence, it is expected that \code{length(which(index.min))} will be one
greater than \code{length(which(index.max))}.}

\item{noise.local}{A numeric vector representing the estimated instantaneous noise level,
i.e., one noise element for each \eqn{m/z} value. This argument is used to form signal-to-noise
ratio (SNR) estimates that are subsequently compared to the \code{snr.thresh} argument in the
pruning process. Default: \code{NULL} (no SNR pruning is performed).}

\item{snr.thresh}{A numeric value representing the signal intensity threshold.
Only the local maxima whose signal intensity is above this value
will be recorded as peaks. Default: \code{2}.}
}

\value{
A data.frame with 10 columns: peak class location, left bound, right bound and
peak span in both clock tick
(\code{"tick.loc"}, \code{"tick.left"}, \code{"tick.right"}, \code{"tick.span"})
and mass measure
(\code{"mass.loc"}, \code{"mass.left"}, \code{"mass.right"}, \code{"mass.span"}),
and	peak signal-to-noise ratio and intensity
(\code{"snr"}, \code{"intensity"}).
Since \code{noise.local} is \code{NULL}, \code{"snr"} is the same as (\code{"intensity"}).
}
\seealso{
\code{\link{msPeak}}, \code{\link{msPeakSimple}}, \code{\link{msPeakMRD}}, \code{\link{msPeakCWT}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## create faux MS peak data 
z <- qcset[seq(500), 1]
x <- z$mz
y <- z$intensity
noise <- as.vector(wavCWT(y, n.scale=1))
index.min <- peaks(-y, span=31)
index.max <- peaks(y, span=31)

## pack data using the constructor 
msPeakInfo(x, y, index.min, index.max, noise.local=noise, snr.thresh=1.3)
}
\keyword{manip}

