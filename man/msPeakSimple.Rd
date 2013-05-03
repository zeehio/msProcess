%%  WARNING: This file was automatically generated from the associated
%%  ms_peak.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_peak.mid file in the project MID directory. Once the
%%  ms_peak.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msPeakSimple function

\name{msPeakSimple}
\alias{msPeakSimple}
\title{Peak Detection via Local Maxima}
\concept{peak detection}
\usage{msPeakSimple(x, y, noise.local=NULL, span=3,
    snr.thresh=2, process="msPeakSimple")}
\description{Performs peak detection via a simple local maxima search.}

\arguments{
\item{x}{A vector representing the \code{m/z} values of a spectrum.}

\item{y}{A vector representing the intensity values of the spectrum.}
\item{noise.local}{A vector representing the estimated local noise level. Default: NULL.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msPeakSimple"}.}

\item{snr.thresh}{A value representing the signal to noise threshold.
Only the local maxima whose signal to noise level is above this value
will be recorded as peaks. Default: 2.}

\item{span}{A peak is defined as an element in a sequence which is greater
than all other elements within a window of width span centered at that element.
The default value is \code{span=3}, meaning that a peak is bigger than both of its neighbors.}
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
Coombes, K.R., Tsavachidis, S., Morris, J.S., Baggerly, K.A., Kuerer, H.M.,
``Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, 5:4107--17, 2005.
}

\seealso{
\code{\link{msPeak}}, \code{\link{msPeakSearch}}, \code{\link{msExtrema}}, \code{\link[splus2R]{peaks}}.}
\keyword{manip}


