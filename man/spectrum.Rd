%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the spectrum function

\name{spectrum}
\alias{spectrum}
\alias{spectrum-class}
\alias{plot,spectrum,missing-method}
\title{Class Representing a Spectrum}
\concept{data structure}
\description{Class slots:
\describe{
\item{tof}{a numeric vector denoting the time-of-flight value of the proteins.}

\item{mz}{a numeric vector denoting the mass-to-charge ratio of the proteins.}

\item{intensity}{a integer vector denoting the aboundance/counts of the proteins.}}}


\section{S3 METHODS}{
\describe{

\item{xyCall}{Make all functions that naturally relate to points in the plane
work with objects of class \code{spectrum},
including \code{plot}, \code{lines}, \code{points}, and etc.
This generic function is not meant to be called directly.

Usage: xyCall(x, y, FUN, ..., xexpr, yexpr)
\describe{
\item{x}{a \code{spectrum} object.}

\item{y}{missing.}

\item{FUN}{a function to be called that have arguments \code{x}, \code{y}, ....}

\item{xexpr}{the S object representing the \code{x} argument to \code{FUN} unevaluated.}

\item{yexpr}{the S object representing the \code{y} argument to \code{FUN} unevaluated.}}}

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


