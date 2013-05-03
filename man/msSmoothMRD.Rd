%%  WARNING: This file was automatically generated from the associated
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_smooth.mid file in the project MID directory. Once the
%%  ms_smooth.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msSmoothMRD function

\name{msSmoothMRD}
\alias{msSmoothMRD}
\title{Baseline Estimation via Partial Summation of an MRD}
\concept{baseline estimationwavelet}
\usage{msSmoothMRD(x, wavelet="s8", levels=1,
    xform="modwt", reflect=TRUE,
    keep.smooth=TRUE, keep.details=FALSE,
    process="msSmoothMRD")}
\description{Forms a multiresolution decomposition (MRD) by taking a specified discrete
wavelet transform of the input spectrum and subsequently inverting each level of the transform
back to the "time" domain. The resulting components of the MRD form an octave-band
decomposition of the original spectrum, and can be summed together to reconstruct the original
spectrum. In many real-world observations, the trend of the data is caught up in
the last decomposition level's so-called \emph{smooth}, which corresponds to residual
low frequency content. This function allows the user to approximate the baseline trend
in a mass spectrum by calculating the MRD smooth. Additionally, the user has the option
to include relatively higher frequency content in the approximation by including various
MRD details (see the DETAILS section for a definition of MRD details and MRD smooth).
As this function primarily calls the \code{wavMRDSum} function with appropriate arguments,
see the corresponding help documentation for more details.}

\arguments{
\item{x}{A vector containing a uniformly-sampled real-valued time series.}
\item{keep.details}{A logical value. If \code{TRUE}, the details
corresponding to the specified levels are included in the partial summation
over the MRD components. The user also has the choice to
include the smooth in the summation via the \code{keep.smooth} option,
but one of \code{keep.details} and \code{keep.smooth} must be \code{TRUE}.
Default: \code{FALSE}.}

\item{keep.smooth}{A logical value. If \code{TRUE}, the smooth
at the last decomposition level is added to the partial summation
over specified details. The smooth typically contains low-frequency trends present in
a spectrum. The user also has the choice to
include the details in the summation via the \code{keep.details} option,
but one of \code{keep.details} and \code{keep.smooth} must be \code{TRUE}.
Default: \code{TRUE}.}

\item{levels}{An integer vector of integers denoting the MRD detail(s) to sum over in forming
a denoised approximation to the orginal spectrum (the summation is performed across scale and nto across time).
All values must be positive integers,
and cannot exceed \code{floor(logb(length(x),2))} if \code{reflect=FALSE} and, if \code{reflect=TRUE}, cannot exceed
\code{floor(logb((length(x)-1)/(L-1) + 1, b=2))} where \eqn{L} is the length of the wavelet filter. Use
the \code{keep.smooth} option to also include the last level's smooth in the summation.
Default: 1.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data. This process is not updated if it
already exists in the event history. Default: \code{"msSmoothMRD"}.}

\item{reflect}{A logical value. If \code{TRUE}, the
last \eqn{L_J = (2^{\mbox{n.level}} - 1)(L - 1) + 1}{Lj = (2^n.level - 1)(L - 1) + 1}
coefficients of the series are reflected (reversed and appended to the end
of the series) in order to attenuate the adverse effect of circular
filter operations on wavelet transform coefficients for
series whose endpoint levels are (highly) mismatched.
The variable \eqn{L_J}{Lj} represents the effective filter length at
decomposition level \code{n.level}, where \eqn{L}
is the length of the wavelet (or scaling) filter.
A similar operation is performed at the beginning of the series.
After synthesis and (partial) summation of the resulting details
and smooth, the middle \eqn{N} points of the result are returned, where \eqn{N} is the length
of the original time series.
Default: \code{TRUE}.}

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
A vector containing the baseline trend of the input spectrum.
}
\references{
D. B. Percival and A. T. Walden, \emph{Wavelet Methods for
Time Series Analysis}, Cambridge University Press, 2000.

T.W. Randolph and Y. Yasui, \emph{Multiscale Processing of Mass Spectrometry Data},
\emph{Biometrics}, 62:589--97, 2006.
}

\seealso{
\code{\link[wmtsa]{wavMRDSum}}, \code{\link{msDetrend}}, \code{\link[wmtsa]{wavDaubechies}}, \code{\link[wmtsa]{wavDWT}},
\code{\link[wmtsa]{wavMODWT}}, \code{\link[wmtsa]{wavMRD}}, \code{\link{eventHistory}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## obtain a subset of a mass spectrum and add some
## noise
x <- qcset$intensity[5000:10000,1]
sd.noise <- 2
set.seed(100)
xnoise <- x + rnorm(length(x), sd=sd.noise)
mz <- as.matrix(as.numeric(names(x)))

## calculate the smooth at decomposition level 9
## and use that as an approximation of the
## spectrum baseline
z <- msSmoothMRD(xnoise, levels=9)

## plot the results
plot(range(mz), range(xnoise), type="n",
    xlab="m/z", ylab="Spectrum and Baseline Estimation")
lines(mz, xnoise, type="l", lty=2, col=1)
lines(mz, z, lty=1, lwd=3, col=2)
}
\keyword{manip}

