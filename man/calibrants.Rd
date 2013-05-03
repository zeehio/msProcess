%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the calibrants function

\name{calibrants}
\alias{calibrants}
\alias{calibrants-class}
\title{Class Representing a Set of Calibrants}
\concept{data structure}
\usage{calibrants(masses, counts)}
\description{A set of calibrants only contains a small number (typically 5 to 7)
of proteins of known mass.

Class slots:
\describe{
\item{masses}{A numeric vector of protein masses.}

\item{counts}{An integer vector of protein counts/abundance.}}


Class extends:
\describe{
\item{proteins}{by direct inclusion.}}


The class \code{calibrants} inherits all the methods of the class \code{proteins}.}

\arguments{
\item{masses}{A positive numeric vector of protein masses,
whose elements should be unique.}

\item{counts}{A positive integer vector of protein counts/abundance,
which should have the same length as \code{masses}.}

}
\references{
Coombes, K.R., Koomen, J.M., Baggerly, K.A., Morris, J.S., Kobayashi, R.,
``Understanding the characteristics of mass spectrometry data
through the use of simulation,"
\emph{Cancer Informatics}, 2005(1):41--52, 2005.
}

\seealso{
\code{\link{proteins}}, \code{\link{spectrometer}}.}

\examples{
## generate two protein samples 
cal1 <- calibrants(masses=c(1, 95, 190), counts=as.integer(c(500, 3000, 10000)))
cal2 <- calibrants(masses=10000+200*(0:3), counts=as.integer(c(12000, 4000, 2000, 1000)))

## print the synopsis of the protein samples 
cal1
cal2

## mix the protein samples 
cal <- cal1 + cal2

## visualize the calibrants 
plot(cal, type="h")
}
\keyword{manip}

