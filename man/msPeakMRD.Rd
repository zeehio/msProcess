%%  WARNING: This file was automatically generated from the associated
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_peak.mid file in the project MID directory. Once the
%%  ms_peak.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msPeakMRD function

\name{msPeakMRD}
\alias{msPeakMRD}
\title{Peak Detection via Multiresolution Decomposition}
\concept{peak detection}
\usage{msPeakMRD(x, y, n.level=floor(log2(length(y))),
    concavity.threshold=0, snr.thresh=0, process="msPeakMRD")}
\description{A multiresolution decomposition (MRD) of the input time
is formed using the maximal overlap discrete wavelet transform (MODWT).
The sum of the MRD details (\eqn{D}) over the user-specified decomposition level(s)
is formed.

If the number of specified decomposition levels is unity, first and second derivative approximations
of \eqn{D} are approximated via (approximate) zero phase shifted versions of the MODWT(\eqn{D})
using the Haar and D4 (Daubechies extremal phase 4-tap) wavelet filter, respectively.
The index locations of the original series are "marked" where the
the first derivative is approximately zero and the second derivative
exceeds a user defined threshold, providing an estimate of the local extrema
locations in \eqn{D}. Here, we note that
a \emph{positive} convavity threshold is used (as opposed to a negative value)
due to a natural negation of the second derivative approximation using the
wavelet scheme described above.

If the number of decomposition levels is greater than one,
a simple peak detection scheme (\code{msExtrema}) is used to
return the locations of the local maxima in \eqn{D}.}

\arguments{
\item{x}{A numeric vector representing the \code{m/z} values of a spectrum.}

\item{y}{A numeric vector representing the intensity values of the spectrum.}
\item{concavity.threshold}{A non-negative concavity threshold. All points in the second derivative approximation
to the calculated detail series that do no exceed this threshold are removed a potential
local extrema candidates. Only used if \code{length(levels) == 1}. Default: 0.}

\item{n.level}{The decomposition level in which the analysis is to be carried out.
limited to \code{floor(log2(length(y)))}. Default: \code{floor(log2(length(y)))}.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msPeakMRD"}.}

\item{snr.thresh}{A numeric value representing the signal intensity threshold.
Only the local maxima whose signal intensity is above this value
will be recorded as peaks. Default: 0.}
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
\references{
T. W. Randolph and Y. Yasui,
Multiscale Processing of Mass Spectrometry Data,
\emph{Biometrics}, 62, pp. 589--597, 2006.

D. B. Percival and A. T. Walden, \emph{Wavelet Methods for
Time Series Analysis}, Cambridge University Press, 2000.
}

\seealso{
\code{\link[wmtsa]{wavMODWT}}, \code{\link[wmtsa]{wavDaubechies}}, \code{\link[wmtsa]{wavIndex}}, \code{\link{msPeak}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## extract a subset of a single spectrum
mz  <- qcset$mz
imz <- mz > 3000 & mz < 5000
z   <- as.vector(qcset[imz, 1]$intensity)
mz  <- mz[imz]

## specify the decomposition levels of interest
level <- 6

## calculate the MRD detail
D <- wavMRDSum(z, wavelet="haar",
    levels=level, xform="modwt",
    keep.smooth=FALSE, keep.details=TRUE,
    reflect=TRUE)

## locate MODWT MRD detail features via
## wavelet-based first and second derivative
## approximations
ipeak1 <- msPeakMRD(mz, D, n.level=level)[["tick.loc"]]

## plot the results
plot(D, cex=0.5, ylab=paste("D",level,sep=""), type="b")
abline(v=ipeak1, col="blue", lty="dashed")
}
\keyword{manip}

