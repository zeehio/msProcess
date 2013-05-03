%%  WARNING: This file was automatically generated from the associated
%%  ms_norm.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_norm.mid file in the project MID directory. Once the
%%  ms_norm.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msNormalizeSNV function

\name{msNormalizeSNV}
\alias{msNormalizeSNV}
\title{Standard Normal Variate Intensity Normalization}
\concept{intensity normalizationwavelet}
\usage{msNormalizeSNV(x, process="msNormalizeSNV")}
\description{Normalizes the input spectrum via the Standard Normal Variate (SNV) transformation
defined as \eqn{X_t \equiv (X_t - \bar{X})/\sqrt{\mbox{var}(X)}}{X[t] = (X[t] - mean(X))/stdev(X)},
where \eqn{X} is the spectrum.}

\arguments{
\item{x}{A vector containing a uniformly-sampled real-valued time series.}
\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data. This process is not updated if it
already exists in the event history. Default: \code{"msNormalizeSNV"}.}
}

\value{
A vector containing the scale-based normalization of the input spectrum.
}
\details{
%
This function can also produce scale-based normalization transformations
if the input is a (partial) sum over multiresolution decomposition (MRD) components formed
by taking a discrete wavelet transform of the input spectrum and subsequently inverting each level of the transform
back to the "time" domain. The resulting components of the MRD form an octave-band
decomposition of the original spectrum, and can be summed together to reconstruct the original
spectrum. Summing only a subset of these components can be viewed as a denoising
operation if the "noisy" components are excluded from the summation. The result is then
normalized by the standard deviation of the sum of the details. As this function merely
calls the \code{msDenoiseMRD} function with the argument \code{normalize=TRUE}, see that function
for more details.
}
\references{
I.S. Helland, T. Naes and T. Isaksson, Related versions of
the multiplicative scatter correction method for preprocessing
spectroscopic data, \emph{Chemometrics and Intelligent Laboratory
Systems}, 29:233--241, 1995.

T.W. Randolph, Scale-based normalization of spectral data,
\emph{Cancer Biomarkers}, 2:135--144, 2006.

T.W. Randolph and Y. Yasui, \emph{Multiscale Processing of Mass Spectrometry Data},
\emph{Biometrics}, 62:589--97, 2006.
}

\seealso{
\code{\link{msNormalize}}, \code{\link{msNormalizeTIC}}, \code{\link{msDenoiseWavelet}},
\code{\link[wmtsa]{wavDaubechies}}, \code{\link[wmtsa]{wavDWT}}, \code{\link[wmtsa]{wavMODWT}}, \code{\link[wmtsa]{wavMRD}},
\code{\link{eventHistory}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## obtain a subset of a mass spectrum and add some
## noise
x <- qcset[5000:7000,1]
sd.noise <- 2
set.seed(100)
x$intensity <- x$intensity + rnorm(length(x), sd=sd.noise)
mz <- x$mz

## sum over specified MODWT MRD details and
## normalize
y <- msDenoise(x, FUN="mrd", levels=6:8, keep.smooth=FALSE)
z <- msNormalizeSNV(y)

## plot the results
old.plt <- par("plt")
par(plt=c(0.08,1,0.5,0.95))
plot(mz, x$intensity, type="l", xaxt="n", xlab="", ylab="xnoise")
par(plt=c(0.08,1,0.12,0.5), new=TRUE)
plot(mz, z$intensity, type="l", xlab="m/z", ylab="Normalized (D6+D7+D8)")
par(plt=old.plt)
}
\keyword{manip}

