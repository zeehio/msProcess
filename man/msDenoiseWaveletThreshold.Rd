%%  WARNING: This file was automatically generated from the associated
%%  ms_noise.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_noise.mid file in the project MID directory. Once the
%%  ms_noise.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msDenoiseWaveletThreshold function

\name{msDenoiseWaveletThreshold}
\alias{msDenoiseWaveletThreshold}
\alias{plot.msDenoiseWaveletThreshold}
\alias{print.msDenoiseWaveletThreshold}
\title{Wavelet Shrinkage Threshold Test}
\concept{noise reductionwavelet}
\usage{msDenoiseWaveletThreshold(x, wavelet="s8",
    n.level=as.integer(floor(logb(length(x), 2))),
    shrink.fun="hard", thresh.scale=NULL,
    xform="modwt", reflect=TRUE, n.threshold=500,
    thresh.fun="universal", noise.variance=NULL,
    min.thresh=NULL, max.thresh=NULL)}
\description{Performs waveshrink on a given spectrum over a (wide) range
of thresholds. For each threshold, a waveshrunk version of the
input spectrum is calculated and a separation statistic is formed
based on the absolute difference between the waveshrunk output and an
\emph{infinitely smooth} reference series. The reference series is
a the result of waveshrinking the input spectrum with a very large
threshold, where (nearly) all the wavelet coefficients are
(shrunk toward) zero in the shrinkage process.
The \code{plot} method can be used to display an image of the separation
statistics over the specified range of thresholds and corresponding
\eqn{m/z} values. This technique eliminates adaptive thresholding, where
a unique threshold is used to shrink the wavelet coefficients at different
scales, since only a single threshold is supplied.}

\arguments{
\item{x}{A vector containing a uniformly-sampled real-valued time series, typically a mass spectrum.}
\item{min.thresh, max.thresh}{Numeric scalars defining the threshold range.
These arguments are only used	if the default value of the \code{thresh.scale}
argument is used, i.e.,	\code{thresh.scale=NULL}.
Default: range(abs(wavelet coefficients)) using the specified  transform.}

\item{n.level}{The number of decomposition levels, limited to
\code{floor(logb(length(x),2))}.
Default: \code{as.integer(floor(logb(length(x), 2)))}.}

\item{n.threshold}{The number of thresholds. This argument is only used
if the default value of the \code{thresh.scale} argument is used, i.e.,
\code{thresh.scale=NULL}. This argument must be a positive integer. Default: 500.}

\item{noise.variance}{A numeric scalar representing (an estimate of)
the additive Gaussian white noise variance. If unknown, setting
this value to 0.0 (or less) will prompt the function to automatically
estimate the noise variance based on the median absolute deviation (MAD)
of the scale one wavelet coefficients. Default: NA (MAD estimate will be used).}

\item{reflect}{A logical value. If \code{TRUE}, the
last \eqn{L_J = (2^{\mbox{n.level}} - 1)(L - 1) + 1}{Lj = (2^n.level - 1)(L - 1) + 1}
coefficients of the series are reflected (reversed and appended to the end
of the series) in order to attenuate the adverse effect of circular
filter operations on wavelet transform coefficients for
series whose endpoint levels are (highly) mismatched.
The variable \eqn{L_J}{Lj} represents the effective filter length at
decomposition level \code{n.level}, where \eqn{L}
is the length of the wavelet (or scaling) filter.
After waveshrinking and reconstructing,
the first \eqn{N} points of the result are returned, where \eqn{N} is the length
of the original time series.
Default: \code{TRUE}.}

\item{shrink.fun}{A character string denoting the shrinkage function.
Choices are \code{"hard"}, \code{"soft"}, and \code{"mid"}. Default: \code{"hard"}.}

\item{thresh.fun}{A character string denoting the threshold function to
use in calculating the waveshrink thresholds.
\describe{
\item{character string}{Choices are \code{"universal"},
\code{"minimax"}, and \code{"adaptive"}.}

\item{numeric values}{Either a single threshold value or a
vector of values containing \code{n.levels}
thresholds (one threshold per decomposition level).}}

Note: if \code{xform == "modwt"}, then only the \code{"universal"}
threshold function is (currently) supported.
Default: \code{"universal"}.}

\item{thresh.scale}{A numeric vector containing the threshold values
to use in denoising the wavelet coefficients. This vector must contain
at least two numeric values.
Default: \code{seq(min(abs(wavelet coefficients)),} \code{max(abs(wavelet coefficients)),}
\code{length=n.threshold)}
where \code{wavelet coefficients} are defined by the specified transform. This range
of thresholds allows the user to explore values over all the effective threshold levels.}

\item{wavelet}{A character string denoting the filter type.
See \code{wavDaubechies} for details. Default: "s8".}

\item{xform}{A character string denoting the wavelet transform type.
Choices are \code{"dwt"} and \code{"modwt"} for the discrete wavelet transform (DWT)
and maximal overlap DWT (MODWT), respectively. The DWT is a decimated transform
where (at each level) the number of transform coefficients is halved. Given
\eqn{N} is the length of the original time series, the total
number of DWT transform coefficients is \eqn{N}.
The MODWT is a non-decimated transform where the number of
coefficients at each level is \eqn{N} and the total number of
transform coefficients is \eqn{N}*\code{n.level}. Unlike the DWT, the MODWT
is shift-invariant and is seen as a weighted average of all possible
non-redundant shifts of the DWT. See the references for details.
Default: \code{"modwt"}.}
}

\value{
An object of class \code{msDenoiseWaveletThreshold}.
}

\section{S3 METHODS}{
\describe{

\item{plot}{Plots an image of the separation statistics as a function
of threshold (ordinate) and \eqn{m/z} value (abscissa). For reference,
a line plot of the original and \emph{infinitely smoothed} spectra are overlaid
on the image.  Available options are:
\describe{
\item{xlab}{character string defining x-axis label. Default: \code{"m/z"}.}

\item{ylab}{character string defining y-axis label. Default: \code{"Waveshrink Threshold"}.}

\item{lty}{an integer denoting the line type ala the \code{par} function. Default: 1.}

\item{lwd}{an integer denoting the line width ala the \code{par} function. Default: 2.}

\item{\code{...}}{additional argument sent directly to the \code{lines} function used to overlay the
image with the original and infiniteluyu smooth spectra.}}}

\item{print}{Prints a summary of the returned object. Available options are:
\describe{
\item{justify}{text justification ala the \code{format} function. Default: \code{"left"}.}

\item{sep}{header separator. Default: \code{":"}.}}}

}
}

\references{
Donoho, D. and Johnstone, I. \emph{Ideal Spatial Adaptation
by Wavelet Shrinkage}. Technical report, Department of Statistics,
Stanford University, 1992.

Donoho, D. and Johnstone, I. \emph{Adapting to Unknown Smoothness
via Wavelet Shrinkage}. Technical report, Department of Statistics,
Stanford University, 1992.

D. B. Percival and A. T. Walden, \emph{Wavelet Methods for
Time Series Analysis}, Cambridge University Press, 2000.

http://bioinformatics.mdanderson.org/sizer.html.
}

\seealso{
\code{\link{msDenoiseWavelet}}, \code{\link{msDenoise}}, \code{\link{rescale}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## grab portion of a mass spectrum and plot
x <- qcset$intensity[5000:7000,1]

## create noise and add it to the spectrum portion
sd.noise <- 2
set.seed(100)
xnoise <- x + rnorm(length(x), sd=sd.noise)

## calculate the waveshrink separation statistics
z <- msDenoiseWaveletThreshold(xnoise)
print(z)
plot(z)
}
\keyword{manip}

