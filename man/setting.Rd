%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the setting function

\name{setting}
\alias{setting}
\alias{setting-class}
\alias{show,setting-method}
\title{Class Representing the Setting of a Mass Spectrometer}
\concept{data structure}
\description{Class slots:
\describe{
\item{dist.drift}{a numeric scalar denoting the length of drift tube in meters.}

\item{dist.focus}{a numeric scalar denoting the distance between charged grids in millimeters.}

\item{dist.accel}{a numeric scalar denoting the distance from sample plate to first grid in millimeters.}

\item{volt.accel}{a numeric scalar denoting the voltage between charged grids in volts.}

\item{volt.focus}{a numeric scalar denoting the voltage used in ion focusing phase in volts.}

\item{time.delay}{a numeric scalar denoting the delay time before focus voltage is applied in nanoseconds.}

\item{time.resol}{a numeric scalar denoting the time between detector records in seconds.}

\item{vel0.mean}{a numeric scalar denoting the mean initial velocity in meters/second.}

\item{vel0.std}{a numeric scalar denoting the standard deviation of initial velocity.}}


The model of a linear MALDI-TOF instrument with time-lag focusing
depends on the nine parameters. In a real instrument,
the three distance parameters
(\code{dist.drift}, \code{dist.focus}, and \code{dist.accel})
are unchanging characteristics of the design.
The user has direct control over
\code{volt.accel}, \code{volt.focus}, \code{time.delay}, and \code{time.resol}.
The parameters that determine the normal distribution of initial velocities,
i.e., \code{vel0.mean} and \code{vel0.std},
are controlled indirectly by the choice of EAM and by the laser intensity.}


\section{S3 METHODS}{
\describe{

\item{show}{Display a \code{setting} object.

Usage: show(object) or object
\describe{
\item{object}{a \code{setting} object.}}}

}
}

\references{
Coombes, K.R., Koomen, J.M., Baggerly, K.A., Morris, J.S., Kobayashi, R.,
``Understanding the characteristics of mass spectrometry data
through the use of simulation,"
\emph{Cancer Informatics}, 2005(1):41--52, 2005.
}

\seealso{
\code{\link{spectrometer}}.}
\keyword{manip}


