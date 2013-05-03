%%  WARNING: This file was automatically generated from the associated 
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_peak.mid file in the project MID directory. Once the 
%%  ms_peak.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msPeakCWT function

\name{msPeakCWT}
\alias{msPeakCWT}
\title{Peak Detection via the Continuous Wavelet Transform}
\concept{peak detection}
\usage{msPeakCWT(x, y, n.scale = 100, snr.min = 3, scale.min = 4,
          length.min = 10, noise.span = NULL, noise.fun =
          "quantile", noise.min = NULL, n.octave.min = 1,
          tolerance = 0, holder = TRUE, process = "msPeakCWT")}

\description{This function isolates peaks of interest in the input mass spectrum
by way of a continuous wavelet transform (CWT). The method is scale-selective,
i.e., the user can specify the scale range of interest in defining peaks.
The basic algorithm works as follows:
\describe{
\item{CWT}{a continuous wavelet transform of the input spectrum \code{y} is calculated
over a broad range of scales.}

\item{CWT Tree}{a CWT tree is formed by dividing the set of local maxima in the CWT time-scale plane
into \emph{branches}. Each branch contains a list of maxima whose neighbors are close in time as scales are
traversed from coarse to fine. The \code{m/z} value at the smallest scale identifies the peak location
in the original input spectrum.}

\item{Pruning}{The collection of branches (and corresponding peak locations) is pruned in numerous
ways (see the \code{\link[wmtsa]{wavCWTPeaks}} function for details.)}}


In addition to peak identification, this function also calculates an estimate of the Holder exponent
associated with each peak. Qualitatively speaking, the magnitude (of the modulus) of the Holder exponent is proportional
to the the sharpness of the corresponding peak. See the \code{\link[wmtsa]{holderSpectrum}} function for more
information. The Holder exponents are packed into the output \code{data.frame} along with other peak-related information.}

\arguments{
\item{x}{A numeric vector representing the \code{m/z} values of a spectrum.}

\item{y}{A numeric vector representing the intensity values of the spectrum.}
\item{length.min}{The minimum number of points along a CWT tree branch and within the specified \code{scale.range}
needed in order for that branches peak to be considered a peak candidate. See the \code{\link[wmtsa]{wavCWTPeaks}} function for more details.
Default: \code{10}.}

\item{n.octave.min}{A pruning factor for excluding non-persistent branches. If
a branch of connected extrema does not span this number of octaves, it is
excluded from the tree. See the \code{\link[wmtsa]{wavCWTTree}} function for more details. Default: 1.}

\item{n.scale}{The (maximum) number of logarithmically distributed scales over which to evaluate the CWT.
For a uniformly sampled time series (\code{x}), the supported range of CWT scales is \code{deltat(x) * c(1, length(x))}.
where \code{deltat(x)} is the sampling interval.
However, a mass spectrum is generally viewed as a non-uniformly sampled time series because the
difference in successive \code{m/z} values is non-constant due to the quadratic mapping of the TOF values to the \eqn{m/z} domain.
For the purpose of peak detection, however, the sampling interval can be set to unity. Thus, the possible range of CWT scales
is \code{1} to \code{length(x)} and the scale values are integers. See the \code{\link[wmtsa]{wavCWT}} function for more details on CWT scales.
Default: \code{100}.}

\item{holder}{a logical value. If TRUE, the holder exponents corresponding to the peaks are also calculated.}

\item{noise.fun}{A character string defining the function to apply to the local noise estimates in order to sumarize
and quantify the local noise level into a scalar value. See the DETAILS section for more information. Supported values
are
\describe{
\item{"quantile"}{quantile(x, probs=0.95)}

\item{"sd"}{sd(x)}

\item{"mad"}{mad(x, center=0)}}


where \code{x} is a vector of smallest-scale CWT coefficients whose time indices are near that of the branch termination time.
See the \code{\link[wmtsa]{wavCWTPeaks}} function for more details. Default: \code{"quantile"}.}

\item{noise.min}{The minimum allowed estimated local noise level. 
Values must be between 0 and 1 inclusive. Default \code{NULL} corresponds to 0.05.
It is converted to raw scale before passed to \code{\link[wmtsa]{wavCWTPeaks}}:
\code{quantile(attr(x,"noise"), prob=0.05)}, 
where \code{x} is the input \code{wavCWTTree} object.
}

\item{noise.span}{The span in time surrounding each branche's temrination point to use in forming
local noise estimates and (ultimately) peak SNR estimates. See the \code{\link[wmtsa]{wavCWTPeaks}} function for more details.
Default: \code{NULL,max(0.01 * diff(range(times)), 5*sampling.interval)},
where \code{times} and \code{sampling.interval} are attributes of the input \code{wavCWTTree} object.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msPeakCWT"}.}

\item{scale.min}{The minimum allowable value of the scale associated with a CWT peak.
See the \code{scale.range} argument of the \code{\link[wmtsa]{wavCWTPeaks}} function for more details.
Default: \code{4}.}

\item{snr.min}{The minimum allowed peak signal-to-noise ratio. See the \code{\link[wmtsa]{wavCWTPeaks}} function for more details. Default: \code{3}.}

\item{tolerance}{A tolerance vector used to find CWT extrema.
This vector must be as long as there are scales in the CWT such that
the \eqn{j^{th}}{jth} element defines the
tolerance to use in finding modulus maxima at
the \eqn{j^{th}}{jth} scale of the CWT. If not, the last value
is replicated appropriately.
See the \code{\link[wmtsa]{wavCWTTree}} function for more details. Default: 0.}
}

\value{
A data.frame with 11 columns: peak class location, left bound, right bound and
peak span in both clock tick
(\code{"tick.loc"}, \code{"tick.left"}, \code{"tick.right"}, \code{"tick.span"})
and mass measure
(\code{"mass.loc"}, \code{"mass.left"}, \code{"mass.right"}, \code{"mass.span"}),
and	peak signal-to-noise ratio and intensity
(\code{"snr"}, \code{"intensity"}). The final column is \code{"holder"}, representing
the estimated Holder exponent asscoiated with each peak.
Since \code{noise.local} is \code{NULL}, \code{"snr"} is the same as (\code{"intensity"}).
}
\references{
Pan Du, Warren A. Kibbe, and Simon M. Lin,
``Improved peak detection in mass spectrum by incorporating continuous wavelet transform-based pattern matching",
\emph{Bioinformatics}, 22, 2059--2065 (2006).

D. B. Percival and A. T. Walden, \emph{Wavelet Methods for
Time Series Analysis}, Cambridge University Press, 2000.
}

\seealso{
\code{\link[wmtsa]{wavCWT}}, \code{\link[wmtsa]{wavCWTTree}}, \code{\link{msPeak}}, \code{\link{msPeakInfo}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## extract a subset of a single spectrum 
mz  <- qcset$mz
imz <- mz > 3000 & mz < 5000
z   <- as.vector(qcset[imz, 1]$intensity)
mz  <- mz[imz]
plot(mz, z, cex=0.5, ylab="Intensity", xlab="m/z", type="l")

## estimate the peak locations using various 
## minimum CWT scales and overlay the plot 
## locations 
scale.min <- c(2,4,8)
col <- c("green","blue","red")

for (i in seq(along=scale.min)){
    p <- msPeakCWT(mz, z, scale.min=scale.min[i])
    ipeak <- p[["tick.loc"]]
    points(mz[ipeak], z[ipeak], cex=i, col=col[i], pch=1)
}

## add a legend 
if (is.R()){
  legend(3000,10000,pch=1,col=col,legend=paste("scale.min =", scale.min))
} else {
  legend(3000,10000,marks=1,col=col,legend=paste("scale.min =", scale.min))
}

## plot the Holder exponents corresponding to each 
## peak 
hx <- p[["mass.loc"]]
hy <- abs(p[["holder"]])
plot(hx, hy, type="h", xlim=range(mz), lty="dashed", col="blue", xlab="m/z", ylab="|Holder exponent|")
points(hx, hy, col="red", cex=1.2)
lines(mz, rescale(z,c(0,0.5)), lwd=3)
abline(h=0, lty="dotted")
legend(3000,1,col=c("blue","black"), lwd=c(1,3), lty=c("dashed","solid"),
    legend=c("|Holder exponent|", "Scaled spectrum"))
}
\keyword{manip}

