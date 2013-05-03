%%  WARNING: This file was automatically generated from the associated
%%  ms_base.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_base.mid file in the project MID directory. Once the
%%  ms_base.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msDetrend function

\name{msDetrend}
\alias{msDetrend}
\title{Baseline Correction}
\concept{baseline correction}
\usage{msDetrend(x, FUN="loess",
    attach.base=TRUE,    
    event="Baseline Correction", ...)}
\description{Estimate and subsequently subtract the baselines from mass spectra.
The basic technique for baseline estimation is to fit a curve locally
to the intensity minima.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{...}{Additional arguments for the \code{FUN} specified.
See the specific underlying function for details.}

\item{FUN}{Either an object of class \code{"character"}
or of class \code{"function"}.

character: A character string denoting the
method to use in smoothing the data to estimate the baseline.
Supported choices are
\describe{
\item{\code{"loess"}}{uses the function \code{loess.smooth}
to fit a local regression model to the local minima.}

\item{\code{"spline"}}{uses the function \code{spline}
to interpolate through the local minima by means of a cubic spline.}

\item{\code{"supsmu"}}{uses the function \code{supsmu}
to fit a smooth curve to the local minima.}

\item{\code{"approx"}}{uses the function \code{approx}
to linearly interpolate the local minima.}

\item{\code{"monotone"}}{uses the function \code{cummin}
to fit a non-increasing curve to all the intensity values.}

\item{\code{"mrd"}}{uses the function \code{msSmoothMRD} to
extract wavelet-based multiresolution decomposition components
relevant to baseline trends.}}

Default: \code{"loess"}.

function: A user-defined function with an argument list
of the form \code{(x, ...)} where \code{x} is a required argument
corresponding to a numeric vector (typically these values
will be the noise estimates of a mass spectrum).

In either case, the additional arguments \code{...} will be
passed directly to the specified routine.}

%\item{MARGIN}{The subscripts over which the function is to be applied
%to the data defined by \code{type}.
%For example, if \code{type="intensity"}, the  data is
%defined by the intensity matrix of \code{x} and \code{MARGIN=1}
%indicates rows while \code{MARGIN=2} indicates columns.
%Default: 2 (FUN will operate over each column
%of the data).}

\item{attach.base}{A logical value specifying if the estimated baseline needs to be attached
as an element to the output, and the default is \code{T}.
It has to be \code{T} if you want to visualize the baseline.}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Baseline Correction"}.}

%\item{pre}{A function that is applied to the data prior to
%processing it with the function defined by \code{FUN}.
%Default: \code{NULL} (no function is applied a priori).}

%\item{type}{A character string defining the type
%of data matrix to extract from \code{x} and operate over.
%The operative data will be \code{x[[type]]} if it exists.
%Default: \code{"intensity"}.}
}

\value{
An object of class \code{msSet},
optionally, with the estimated baseline attached as element
\code{"baseline"}.
}
\note{If \code{FUN="mrd"}, an \code{mrd} object containing meta information regarding
the multiresolution decomposition is attached to the \code{msSet} output object for subsequent
use by other MRD-based function calls such as \code{msPeak(x, FUN="mrd", ...)}.}

\seealso{
\code{\link{msSmoothLoess}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}, \code{\link{msSmoothApprox}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothMRD}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## extract several spectra from the build-in
## dataset
z <- qcset[, 1:8]

## denoising
z <- msDenoise(z, FUN="wavelet", n.level=10, thresh.scale=2)

## baseline subtraction
z <- msDetrend(z, FUN="monotone", attach=TRUE)

## visualize the baseline
plot(z, process="msDetrend", subset=1:3,
    xlim=c(5000, 8500), lty=1, lwd=c(1,3))
}
\keyword{manip}

