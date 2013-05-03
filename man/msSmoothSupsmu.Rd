%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothSupsmu function

\name{msSmoothSupsmu}
\alias{msSmoothSupsmu}
\title{Fit a Smooth Curve Using Super Smoother}
\concept{smoothingnoise estimationbaseline correction}
\usage{msSmoothSupsmu(x, y, span="cv", periodic=FALSE, bass=0,
    index=rep(TRUE, length(x)), process="msSmoothSupsmu")}
\description{Fits a smooth curve to a subset or set of data points using a super smoother
and returns a vector of fitted smooth curve values evaluated
at the original locations.}

\arguments{
\item{x}{A numeric vector of abscissa values.}

\item{y}{A numeric vector of ordinate values,
which must be of the same length of \code{x}.}
\item{index}{A logical vector of the same length of \code{x}
indicating the elements to be used in the fitting.
Deafult: \code{rep(TRUE, length(x))}.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothSupsmu"}.}

\item{span, periodic, bass}{See function \code{supsmu} for descriptions.}
}

\value{
A vector of fitted smooth curve values evaluated at the original locations.
}
\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothKsmooth}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSpline}}.}
\keyword{manip}


