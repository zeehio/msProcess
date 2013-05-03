%%  WARNING: This file was automatically generated from the associated
%%  ms_norm.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_norm.mid file in the project MID directory. Once the
%%  ms_norm.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msNormalize function

\name{msNormalize}
\alias{msNormalize}
\title{Mother Function for Intensity Normalization}
\concept{intensity normalization}
\usage{msNormalize(x, FUN="tic",
    event="Intensity Normalization", ...)}
\description{Normalizes the intensity matrix of an \code{msSet} object.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{...}{Additional arguments to \code{FUN}.
They are passed unchanged to each call of \code{FUN}
and include their names.
See the help documentation of the specified FUN for details.}

\item{FUN}{Either an object of class \code{"character"}
or of class \code{"function"}.

character: A character string denoting the
method to use in denoising the data.
Supported choices are \code{"snv"} for standard normal variate
transformation or \code{"tic"} for normalization based on the median
total ion current estimate for all spectra.
Default: \code{"tic"}.

function: A user-defined function with an argument list
of the form \code{(x, ...)} where \code{x} is a required argument
corresponding to a numeric vector (typically these values
will be the intensity values of a mass spectrum).

In either case, the additional arguments \code{...} will be
passed directly to the specified FUN.}

%\item{MARGIN}{The subscripts over which the function is to be applied
%to the intensity matrix.
%Default: 2 (\code{FUN} will operate over each column
%of the intensity matrix).}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Intensity Normalization"}.}
}

\value{
An \code{msSet} object with the intensity matrix object
replaced by its normalized form.
}
\seealso{
\code{\link{msNormalizeTIC}}, \code{\link{msNormalizeSNV}}, \code{\link{msSet}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## normalize a subset of spectra in the
## qcset object using total ion current.
data <- qcset[,1:8]
zion <- msNormalize(data, FUN="tic")
plot(zion, process="msNormalize", subset=1:8,
xlim=c(13000, 17000), lty=c(1,4), lwd=1:2)

## normalize a subset of spectra in the
## qcset object using a standard normal
## variate transformation.
zsnv <- msNormalize(data, FUN="snv")
plot(zsnv, process="msNormalize", subset=1:8,
xlim=c(13000, 17000), lty=c(1,4), lwd=1:2)

## perform a multiresolution decomposition of each
## spectrum in the intensity matrix, sum over
## levels 6-8, then normalize using the SNV
## transformation (this process is equivalent
## to scale-based normalization (SBN)).
data <- msDenoise(data, FUN="mrd", levels=6:8, keep.smooth=FALSE)
zsnv <- msNormalize(data, FUN="snv")
plot(zsnv, process="msNormalize", subset=1:8,
xlim=c(13000, 17000), lty=c(1,4), lwd=1:2)
}
\keyword{manip}

