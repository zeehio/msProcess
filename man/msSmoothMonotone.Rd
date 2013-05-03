%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothMonotone function

\name{msSmoothMonotone}
\alias{msSmoothMonotone}
\title{Monotonic Baseline Estimation}
\concept{baseline correction}
\usage{msSmoothMonotone(x, process="msSmoothMonotone")}
\description{Estimates the baseline of a spectrum as a decreasing function
that always takes on a monotonic minimum value.}

\arguments{
\item{x}{A numeric vector representing the intensity values of a spectrum.}
\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothMonotone"}.}
}

\value{
A vector representing the estimated monotonically decreasing baseline.
}
\details{
%
Since this function estimates the baseline of a spectrum as a monotone minimum,
it works only if the portion before the maximum	of the saturation period
has been removed from the spectrum.
}
\references{
Coombes, K.R., Tsavachidis, S., Morris, J.S., Baggerly, K.A., Kuerer, H.M.,
``Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, 5:4107--17, 2005.
}

\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothKsmooth}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


