%%  WARNING: This file was automatically generated from the associated
%%  ms_noise.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_noise.mid file in the project MID directory. Once the
%%  ms_noise.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msDenoiseMRD function

\name{msDenoiseMRD}
\alias{msDenoiseMRD}
\title{Denoising a Mass Spectrum via Partial Summation of an MRD}
\concept{noise reductionwavelet}
\usage{msDenoiseMRD(x, wavelet="s8",
    levels=1, xform="modwt", reflect=TRUE,
    keep.smooth=TRUE, keep.details=TRUE,
    process="msDenoiseMRD")}
\description{Forms a multiresolution decomposition (MRD) by taking a specified discrete
wavelet transform of the input spectrum and subsequently inverting each level of the transform
back to the "time" domain. The resulting components of the MRD form an octave-band
decomposition of the original spectrum, and can be summed together to reconstruct the original
spectrum. Summing only a subset of these components can be viewed as a denoising
operation if the "noisy" components are excluded from the summation.}

\arguments{
\item{x}{A vector containing a uniformly-sampled real-valued time series.}
\item{keep.details}{A logical value. If \code{TRUE}, the details
corresponding to the specified levels are included in the partial summation
over the MRD components. The user also has the choice to
exclude the smooth in the summation via the \code{keep.smooth} option,
but one of \code{keep.details} and \code{keep.smooth} must be \code{TRUE}.
Default: \code{TRUE}.}

\item{keep.smooth}{A logical value. If \code{TRUE}, the smooth
at the last decomposition level is added to the partial summation
over specified details. The smooth typically contains low-frequency trends present in
a spectrum, so removing the smooth (\code{keep.smooth=FALSE}) will result
in removing the trend in such cases. The user also has the choice to
exclude the details in the summation via the \code{keep.details} option,
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
already exists in the event history. Default: \code{"msDenoiseMRD"}.}

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
A vector containing the denoised series.
}
\details{
%
Performs a level \eqn{J} decimated or undecimated discrete wavelet transform
on the input series and inverts the transform at each level
separately to produce details \eqn{D_1,\ldots,D_J}{D1,...,DJ} and smooth \eqn{S_J}{SJ}.
The decomposition is additive such that the original series \eqn{X} may
be reconstructed ala \eqn{X = S_J + \sum_{j=1}^J D_j}{X=D1 + D2 + ... DJ + SJ}.
As the effective wavelet filters at level \eqn{j} are nominally associated with approximate band pass filters,
the details \eqn{D_j}{Dj} correspond approximately to normalized frequencies
on the interval \eqn{[1/2^{j+1}, 1/2^j]}{[1/2^(j+1), 1/2^j]}, while the
content of the smooth \eqn{S_J}{SJ} corresponds approximately to normalized frequencies
\eqn{[0, 1/2^{J+1}]}{[0, 1/2^(J+1)]}. The collection of details and smooth
form a multiresolution decomposition (MRD).

With the intent of removing unwanted noise events,
a summation over a subset of MRD components may be
calculated yielding a smooth approximation to the
original spectrum. For example, summing all MRD components
beyond \eqn{D_1}{D1} is tantamount to a low-pass filtering
of the original spectrum (whether or not this is a relevant
and sufficient noise removal technique is left to the discretion
of the practitioner). This function allows the user to specify
the decomposition levels they wish to sum over in order to form
a multiresolution approximation. The inclusion of the last level's smooth
in the summation is controlled by the optional \code{keep.smooth}
argument.

The user may also select either a decimated wavelet
transform (DWT) or an undecimated wavelet transform (MODWT).
However, we recommend that the user stick with the MODWT for the
following reasons:
\describe{
\item{Translation invariance}{Unlike the DWT, the MODWT is translation invariant, meaning
that a (circular) shift of the input spectrum will result in a corresponding (circular) shift of
the transform coefficients.}

\item{Smoothness}{The MODWT coefficients are a result of \emph{cycle-spinning}, where averages
are taken over all unique DWTs resulting from various circular shifts of the original spectrum. The resulting
MODWT MRD is relatively more smooth than the corresponding DWT MRD.}

\item{Zero phase aligment}{Unlike the DWT MRD, the MODWT MRD produces components that are
associated with exactly zero phase filter operations such that events (such as peaks)
in the details and smooth line up exactly with those of the original spectrum in TOF (or \eqn{m/z}).}

\item{Computational speed}{The DWT is faster than the MODWT, but the MODWT is still quite
fast, requiring multiplication and summation operations on the same order as the popular
Fast Fourier Transform.}}
}
\references{
D. B. Percival and A. T. Walden, \emph{Wavelet Methods for
Time Series Analysis}, Cambridge University Press, 2000.

T.W. Randolph and Y. Yasui, \emph{Multiscale Processing of Mass Spectrometry Data},
\emph{Biometrics}, 62:589--97, 2006.

T.W. Randolph, \emph{Scale-based normalization of spectral data},
\emph{Disease Biomarkers}, 2:135--144, 2006.
}

\seealso{
\code{\link{msDenoise}}, \code{\link{msDenoiseWaveletThreshold}}, \code{\link{msNoise}},
\code{\link[wmtsa]{wavDaubechies}}, \code{\link[wmtsa]{wavDWT}}, \code{\link[wmtsa]{wavMODWT}},
\code{\link[wmtsa]{wavMRD}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothSpline}},
\code{\link{msSmoothKsmooth}}, \code{\link{msSmoothSupsmu}}, \code{\link{msSmoothApprox}},
\code{\link{msDenoiseSmooth}}, \code{\link{eventHistory}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## obtain a subset of a mass spectrum and add some
## noise
x <- qcset$intensity[5000:7000,1]
sd.noise <- 2
set.seed(100)
xnoise <- x + rnorm(length(x), sd=sd.noise)
mz <- as.matrix(as.numeric(names(x)))

## define two different ranges of summation levels
lev1 <- 6:8
lev2 <- 4:8

## calculate MODWT MRDs over these levels
z1 <- msDenoiseMRD(xnoise, levels=lev1)
z2 <- msDenoiseMRD(xnoise, levels=lev2)

## plot the results
Slab <- "S8"
lab1 <- paste(paste("D", lev1, sep="", collapse="+"), Slab, sep="+")
lab2 <- paste(paste("D", lev2, sep="", collapse="+"), Slab, sep="+")

msPlot(matlines=list(
    list(x=mz, y=cbind(z1, z2), lty=1, lwd=3),
    list(x=mz, y=cbind(xnoise, xnoise), type="p", pch="o", cex=0.15)),
    yref=FALSE, xlab="m/z", ylab="MODWT MRD",
    text=list(x=rep(8300,2), y=c(2300,1650),
    labels=c(lab2,lab1), adj=0, col=2:1))
}
\keyword{manip}

