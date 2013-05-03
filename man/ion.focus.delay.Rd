%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the ion.focus.delay function

\name{ion.focus.delay}
\alias{ion.focus.delay}
\title{Simulating Linear MALDI-TOF with Ion Focus Delay}
\concept{simulation}
\usage{ion.focus.delay(mass, v0, setting)}
\description{Given the mass, initial velocity, and the instrument parameters,
it produces the time-of-flight for the protein particles.}

\arguments{
\item{mass}{A vector of masses in daltons.}

\item{v0}{A matching vector of initial velocities in meters/second.}

\item{setting}{An object of class \code{setting} containing the machine setting.}

}

\value{
The time-of-flight for the protein particles.
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


