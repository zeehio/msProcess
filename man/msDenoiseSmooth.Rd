%%  WARNING: This file was automatically generated from the associated 
%%  ms_noise.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_noise.mid file in the project MID directory. Once the 
%%  ms_noise.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msDenoiseSmooth function

\name{msDenoiseSmooth}
\alias{msDenoiseSmooth}
\title{Denoising Mass Spectra via Smoothing}
\concept{noise reduction}
\usage{msDenoiseSmooth(x, twiceit=TRUE, process="msDenoiseSmooth")}
\description{Mass spectra are denoised via running medians.}

\arguments{
\item{x}{A numeric vector.}
\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data. This process is not updated if it
already exists in the event history. Default: \code{"msDenoiseSmooth"}.}

\item{twiceit}{A logical flag. If \code{TRUE}, smooth performs twicing.
Twicing is the process of smoothing, computing the residuals
from the smooth, smoothing these,
and then adding the two smoothed series together.
Default: \code{TRUE}.}
}

\value{
A vector of the same length of \code{x} with noise removed.
}
\seealso{
\code{\link[stats]{smooth}}.}
\keyword{manip}


