%%  WARNING: This file was automatically generated from the associated
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_peak.mid file in the project MID directory. Once the
%%  ms_peak.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msPeakSearch function

\name{msPeakSearch}
\alias{msPeakSearch}
\title{Peak Detection via Elevated Intensity}
\concept{peak detection}
\usage{msPeakSearch(x, y, noise.local=NULL, span=41, span.supsmu=0.05,
    snr.thresh=2, process="msPeakSearch")}
\description{This method seeks intensities that
are higher than those in a local area and are
higher than an estimated average background at the sites.}

\arguments{
\item{x}{A numeric vector representing the \code{m/z} values of a spectrum.}

\item{y}{A numeric vector representing the intensity values of the spectrum.}
\item{noise.local}{A numeric vector representing the estimated local noise level. Default: NULL.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msPeakSearch"}.}

\item{snr.thresh}{A numeric value representing the signal to noise threshold.
Only the local maxima whose signal to noise level is above this value
will be recorded as peaks. Default: 2.}

\item{span}{A peak is defined as an element in a sequence which is greater
than all other elements within a window of width span centered at that element.
Default: 41.}

\item{span.supsmu}{The fraction of observations in the smoothing window. If \code{span="cv"},
then automatic (variable) span selection is done by means of cross validation.
Default: 0.05.}
}

\value{
A data.frame with 10 columns: peak class location, left bound, right bound and
peak span in both clock tick
(\code{"tick.loc"}, \code{"tick.left"}, \code{"tick.right"}, \code{"tick.span"})
and mass measure
(\code{"mass.loc"}, \code{"mass.left"}, \code{"mass.right"}, \code{"mass.span"}),
and	peak signal-to-noise ratio and intensity
(\code{"snr"}, \code{"intensity"}).
If \code{noise.local} is \code{NULL}, \code{"snr"} is the same as (\code{"intensity"}).
}
\references{
Tibshirani, R., Hastie, T., Narasimhan, B., Soltys, S., Shi, G.,
Koong, A., and Le, Q.T.,
``Sample classification from protein mass spectrometry,
by peak probability contrasts,"
\emph{Bioinformatics}, 20(17):3034--44, 2004.

Yasui, Y., McLerran, D., Adam, B.L., Winget, M., Thornquist, M., Feng, Z.,
``An automated peak identification/calibration procedure for
high-dimensional protein measures from mass spectrometers,"
\emph{Journal of Biomedicine and Biotechnology}, 2003(4):242--8, 2003.

Yasui, Y., Pepe, M., Thompson, M.L., Adam, B.L., Wright, Jr., G.L.,
Qu, Y., Potter, J.D., Winget, M., Thornquist, M., and Feng, Z.,
``A data-analytic strategy for protein biomarker discovery:
Profiling of high-dimensional proteomic data for cancer detection,"
\emph{Biostatistics}, 4(3):449--63, 2003.
}

\seealso{
\code{\link{msPeak}}, \code{\link{msPeakSimple}}, \code{\link{msExtrema}}, \code{\link[splus2R]{peaks}}.}
\keyword{manip}


