%%  WARNING: This file was automatically generated from the associated
%%  ms_align.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_align.mid file in the project MID directory. Once the
%%  ms_align.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msAlign function

\name{msAlign}
\alias{msAlign}
\alias{msAlignCluster}
\alias{msAlignGap}
\alias{msAlignMRD}
\alias{msAlignVote}
\title{Peak Alignment}
\concept{peak alignment}
\usage{msAlign(x, FUN="cluster", mz.precision=0.003, snr.thresh=10,...)}
\description{Performs cross-spectral alignment of detected peaks.}

\arguments{
\item{x}{An object of class \code{msSet} containing a \code{"peak.list"} element.}
\item{FUN}{A character string specifying the method to use for alignment.
Choices are
\describe{
\item{\code{"cluster"}:}{clusters peaks using one-dimensional
hierarchical clustering and uses distances between peak locations
as the similarity measure.}

\item{\code{"gap"}:}{analyzes peaks sequentially from low mass to
high mass. Two adjacent peaks are classified into the same class
if the distances between their locations is smaller
than the specified threshold.}

\item{\code{"vote"}:}{clusters peaks iteratively.
Each peak is associated with a window and the number of peaks
that fall within the window across all samples is counted.
The peak corresponding to the highest count forms a new peak cluster
and all the peaks that have contributed to this peak are removed.
The procedure is repeated until all peaks are exhausted
from every sample.}

\item{\code{"mrd"}:}{clusters peaks by smoothing a histogram of scale-based feature locations
for all spectra as identified by a call to \code{msPeak(x,FUN="mrd", ...)}.
The midpoints of the valleys in the smoothed
histogram identifies the common peak locations across corresponding spectra.}}

Default: \code{"cluster"}.}

\item{mz.precision}{A numeric value, used to construct the threshold
when performing clustering.
The default value is 0.003 because SELDI data is often assumed to have
\eqn{\pm 0.3\%}{+/- 0.3\%} mass drift, i.e., a peak at mass \code{w} could represent a protein
with a mass within the interval	\eqn{[w(1-0.003), w(1+0.003)]}.}

\item{snr.thresh}{A non-negative numeric value.
The peaks with signal-to-noise ratio larger than this value
will be used to construct the common set of peak classes.
Default: 10.}

\item{...}{Additional arguments passed to the \code{msAlignMRD} function.}
}

\value{
An object of class \code{msSet}, which is the input \code{x}
with the added element \code{"peak.class"}:  a matrix with peak classes as rows
and some summary statistics of the peak clusters as columns.
These statistics include the location, left bound, right bound
and peak span of the peak classes in both clock tick
(\code{"tick.loc"}, \code{"tick.left"}, \code{"tick.right"}, \code{"tick.span"})
and mass measure
(\code{"mass.loc"}, \code{"mass.left"}, \code{"mass.right"}, \code{"mass.span"}).
}
\details{
%
Currently, the mass accuracy of a mass spectrometer is
proportional to	the mass-to-charge (\eqn{m/z}) values.
Thus, for a given set of spectra, the locations of the detected peaks
will vary from spectrum to spectrum.
In order to perform a comparative analysis of an ensemble of spectra,
it is then a prerequisite to perform inter-sample alignment of the detected peaks.
This process is normally called peak alignment or clustering.

The basic idea for peak alignment is to group peaks of similar molecular weight
across all spectra into peak clusters or classes to form a superset,
allowing for slight variations in mass.
Each cluster is representative of a particular protein.
Various methods have been proposed to align the peaks,
which differ in how the superset is constructed.
}
\references{
Coombes, K.R., Tsavachidis, S., Morris, J.S.,
Baggerly, K.A., and Kuerer, H.M.,
``Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, 5:4107--17, 2005.

Tibshirani, R., Hastie, T., Narasimhan, B., Soltys, S., Shi, G.,
Koong, A., and Le, Q.T.,
``Sample classification from protein mass spectrometry,
by 'peak probability contrasts',"
\emph{Bioinformatics}, 20(17):3034--44, 2004.

Yasui, Y., McLerran, D., Adam, B.L., Winget, M., Thornquist, M., and
Feng, Z.,
``An automated peak identification/calibration procedure for
high-dimensional protein measures from mass spectrometers,"
\emph{Journal of Biomedicine and Biotechnology}, 2003(4):242--8, 2003.

Yasui, Y., Pepe, M., Thompson, M.L., Adam, B.L., Wright, Jr., G.L.,
Qu, Y., Potter, J.D., Winget, M., Thornquist, M., and Feng, Z.,
``A data-analytic strategy for protein biomarker discovery:
Profiling of high-dimensional proteomic data for cancer detection,"
\emph{Biostatistics}, 4(3):449--63, 2003.

T.W. Randolph and Y. Yasui, \emph{Multiscale Processing of Mass Spectrometry Data},
\emph{Biometrics}, 62:589--97, 2006.
}

\seealso{
\code{\link{msPeak}}, \code{\link{msQuantify}}.}

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

## intensity normalization
z <- msNormalize(z)

## peak detection
z <- msPeak(z, FUN="simple", use.mean=FALSE, snr=2)

## peak alignment
z <- msAlign(z, FUN="cluster", snr.thresh=10,
    mz.precision=0.004)

## extract the peak.class
z[["peak.class"]]

## visualize the alignment
plot(z, process="msAlign", subset=1:8, offset=100,
    xlim=c(13000, 17000), lty=c(1,4))
}
\keyword{manip}

