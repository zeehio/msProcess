%%  WARNING: This file was automatically generated from the associated
%%  ms_charge.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_charge.mid file in the project MID directory. Once the
%%  ms_charge.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msCharge function

\name{msCharge}
\alias{msCharge}
\title{Charge Detection}
\concept{charge detection}
\usage{msCharge(x, ncharge=2:3, mz.precision=0.003,
    event="Charge Detection", ...)}
\description{Find proteins that possibly have multiple charges.
This is achieved by detecing proteins whose mz values are nearly exact
multiples of others and hence potentially represent the same protein.}

\arguments{
\item{x}{An object of class \code{msSet} with an existing element \code{"peak.class"}.}
\item{...}{Not used.}

\item{event}{A character string denoting the name of the
event to register with the (embedded) event history object of the input
after processing the input data. Default: \code{"Charge Detection"}.}

\item{mz.precision}{A numeric value, used to construct the threshold
when comparing the multiples of mz values.
The default value is 0.003 because SELDI data is often assumed to have
\eqn{\pm 0.3\%}{+/- 0.3\%} mass drift, i.e., a peak at mass \code{w} could represent a protein
with a mass within the interval	\eqn{[w(1-0.003), w(1+0.003)]}.}

\item{ncharge}{A numeric integer vector denoting the multiple charges
of interest. All of its elements must be larger than 1.}
}

\value{
An object of class \code{msSet} with charge estimate attached
as element \code{"peak.charge"}.
}
\seealso{
\code{\link{msSet}}.}

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

## charge detection
z <- msCharge(z, ncharge=2:5, mz.precision=0.003)

## extract the peak.charge
z[["peak.charge"]]
}
\keyword{manip}

