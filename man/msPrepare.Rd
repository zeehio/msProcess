%%  WARNING: This file was automatically generated from the associated 
%%  ms_prep.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_prep.mid file in the project MID directory. Once the 
%%  ms_prep.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msPrepare function

\name{msPrepare}
\alias{msPrepare}
\title{Convert an msList Object to an msSet Object}
\concept{data preparation}
\usage{msPrepare(x, mass.min=1500, transform=NULL, data.name = NULL)}
\description{Converts an \code{msList} object to an \code{msSet} object by
truncating the spectra to the mass range of interest,
interpolating the spectra to a common set of \code{m/z} values, and
transforming the intensity values if specified.}

\arguments{
\item{x}{An object of class \code{msList}.}
\item{mass.min}{A numeric value denoting the lowest mass (in Dalton) of interest.
The portion of the spectra below this value will be removed.
The low mass region is generally considered suspicious
as matrix contamination may be a problem. Default: 1500.}

\item{transform}{A function to be used to transform the intensity values.
The main purpose of the transformation is to reduce the variance of the
intensity values and to stabilize the noise component of the spectra.
Some possible choices are logarithm (\code{log}), square root (\code{sqrt})
and cube root. The default is not to perform any tranformation.
Default: \code{NULL} (no transform).}

\item{data.name}{name for the data. Default: \code{deparseText(substitute(x))}.}
}

\value{
An object of class \code{msSet} that has the same number of spectra
as in the input \code{msList} object \code{x}.
}
\seealso{
\code{\link{msList}}, \code{\link{msSet}}, \code{\link{msNormalize}}.}

\examples{
if (!exists("qclist")) data("qclist", package="msProcess")

## extract several spectra from the build-in 
## dataset qclist 
zList <- qclist[1:8]

## convert the subset to an msSet object 
cbrt <- function(x) x^(1/3)
zSet <- msPrepare(zList, mass.min=950, transform=cbrt)

## visualize a portion of the spectra 
plot(zSet, subset=NULL, xlim=c(13000, 17000), offset=0.5)
}
\keyword{manip}

