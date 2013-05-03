%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the spectrometer function

\name{spectrometer}
\alias{spectrometer}
\alias{spectrometer-class}
\alias{run}
\alias{run,spectrometer,calibrants-method}
\alias{run,spectrometer,proteins-method}
\title{Class Representing a Mass Spectrometer}
\concept{data structure}
\usage{spectrometer(dist.drift=1, dist.focus=17, dist.accel=8,
    volt.accel=20000, volt.focus=2000,
    time.delay=600, time.resol=4e-9,
    vel0.mean=350, vel0.std=50,
    time.mean=numeric(0), model=structure(NULL, class="lm"), error.rel=numeric(0))}
\description{Class slots:
\describe{
\item{setting}{an object of class \code{setting}.}

\item{calibrator}{an object of class \code{calibrator}.}}}

\arguments{

\item{dist.accel}{A numeric scalar denoting the distance from sample plate to first grid in millimeters.}

\item{dist.drift}{A numeric scalar denoting the length of drift tube in meters.}

\item{dist.focus}{A numeric scalar denoting the distance between charged grids in millimeters.}

\item{error.rel}{A numeric vector denoting the relative calibration error for the calibrants.}

\item{model}{An object of class \code{lm}.}

\item{time.delay}{A numeric scalar denoting the delay time before focus voltage is applied in nanoseconds.}

\item{time.mean}{A numeric scalar denoting the mean time-of-flight of the calibrants.}

\item{time.resol}{A numeric scalar denoting the time between detector records in seconds.}

\item{vel0.mean}{A numeric scalar denoting the mean initial velocity in meters/second.}

\item{vel0.std}{A numeric scalar denoting the standard deviation of initial velocity.}

\item{volt.accel}{A numeric scalar denoting the voltage between charged grids in volts.}

\item{volt.focus}{A numeric scalar denoting the voltage used in ion focusing phase in volts.}
}

\section{S3 METHODS}{
\describe{

\item{run}{Run a mass spectrometer.

Usage: run(simObj, proObj, isotope)
\describe{
\item{simObj}{a \code{spectrometer} object.}

\item{proObj}{a \code{proteins} object or a \code{calibrants} object.
If \code{proObj} is a \code{calibrants} object, then a calibration run is performed
and a calibrated \code{spectrometer} object is returned.
If \code{proObj} is a \code{proteins} object, then a real run is performed
and a \code{spectrum} object is returned.
If the \code{spectrometer} object has been calibrated,
the returned \code{spectrum} will have values for both \code{mz} slot and \code{tof} slot.
Otherwise, the returned \code{spectrum} will have values only for \code{tof} slot.}

\item{isotope}{a logical value indicating if isotope distribution should be simulated.}}}

}
}

\references{
Coombes, K.R., Koomen, J.M., Baggerly, K.A., Morris, J.S., Kobayashi, R.,
``Understanding the characteristics of mass spectrometry data
through the use of simulation,"
\emph{Cancer Informatics}, 2005(1):41--52, 2005.
}

\seealso{
\code{\link{setting}}, \code{\link{calibrator}}, \code{\link{spectrum}}, \code{\link{ion.focus.delay}}.}

\examples{
## run a uncalibrated mass spectrometer 
sam <- proteins(masses=c(1, 95, 190), counts=as.integer(c(500, 3000, 10000)))
sim <- spectrometer(vel0.mean=350, vel0.std=75, time.resol=4e-9)
x <- run(sim, sam)
plot(x)

## run a calibrated mass spectrometer 
cal <- calibrants(masses=c(1000, 2000, 5000, 10000, 20000), counts=as.integer(rep(1000, 5)))
sim.cal <- run(sim, cal)
y <- run(sim.cal, sam)
plot(y)
}
\keyword{manip}

