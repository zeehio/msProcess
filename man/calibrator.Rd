%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the calibrator function

\name{calibrator}
\alias{calibrator}
\alias{calibrator-class}
\title{Class Representing the Calibrator of a Mass Spectrometer}
\concept{data structure}
\description{Class slots:
\describe{
\item{time.mean}{a numeric scalar denoting the mean time-of-flight of the calibrants.}

\item{model}{an object of class \code{lm}.}

\item{error.rel}{a numeric vector denoting the relative calibration error for the calibrants.}}}

\references{
Coombes, K.R., Koomen, J.M., Baggerly, K.A., Morris, J.S., Kobayashi, R.,
``Understanding the characteristics of mass spectrometry data
through the use of simulation,"
\emph{Cancer Informatics}, 2005(1):41--52, 2005.
}

\seealso{
\code{\link{spectrometer}}.}
\keyword{manip}


