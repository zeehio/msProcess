%% R documentation for the msLogic function

\name{msObjects}
\alias{msObjects}
\title{Finding msSet Object Names}
\concept{gui logic}
\usage{msObjects(process="msDenoise")}
\description{Returns a vector of character strings (possibly of zero length)
which are the names of the \code{msSet} objects in the working database 
to which \code{process} can be applied.
This function is mainly used to build the GUI for the msProcess package.}

\arguments{
\item{process}{a character string specifying the process to be applied.
The options are \code{"msDenoise"},
\code{"msNoise"}, \code{"msDetrend"},
\code{"msNormalize"}, \code{"msPeak"},
\code{"msAlign"}, and \code{"msQuantify"}. 
Default: \code{"msDenoise"}.}
}

\value{
a character vector of names of the \code{msSet} objects in the working database
to which \code{process} can be applied or \code{character(0)} if none applicable.
}
\seealso{
\code{\link{msSet}}.
}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## find the names of msSet objects to which msDenoise can be applied
msObjects("msDenoise")

## find the names of msSet objects to which msDenoise can be applied
my.qcset <- qcset
msObjects("msDenoise")

## apply denoise, peak detection, and alignment
denoised <- msDenoise(qcset)
detected <- msPeak(denoised)
aligned <- msPeak(detected)

## find the names of msSet objects to which msAlign can be applied
msObjects("msAlign")

## find the names of msSet objects to which msQuantify can be applied
msObjects("msQuantify")
}
\keyword{manip}

