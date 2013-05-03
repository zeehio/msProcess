%% R documentation for the msLogic function

\name{msLogic}
\alias{msLogic}
\title{Processing Logic}
\concept{processing logic}
\usage{msLogic(x, process="msDenoise")}
\description{For a specific \code{msSet} object \code{x}, it determines 
which methods are applicable for the processing step \code{process}.
This function is mainly used to build the GUI for the msProcess package.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{process}{a character string specifying the process to be applied.
The options are \code{"msDenoise"},
\code{"msNoise"}, \code{"msDetrend"},
\code{"msNormalize"}, \code{"msPeak"},
\code{"msAlign"}, and \code{"msQuantify"}. 
Default: \code{"msDenoise"}.}
}

\value{
A vector of character string denoting the methods of \code{process} that are 
applicable to the \code{msSet} object \code{x}, which are the valid options for
the \code{FUN} argument of \code{process}. 
\code{NULL} is returned if none is applicable.
}
\seealso{
\code{\link{msSet}}.
}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## determine which methods of msDenoise are applicable to qcset
msLogic(qcset, "msDenoise")

## determine which methods of msNoise are applicable to qcset
msLogic(qcset, "msNoise")

## apply wavelet denoise
denoised <- msDenoise(qcset, FUN="wavelet")

## determine which methods of msNoise are applicable to the denoised spectra
msLogic(denoised, "msNoise")

## determine which methods of msPeak are applicable to the denoised spectra
msLogic(denoised, "msPeak")
}
\keyword{manip}

