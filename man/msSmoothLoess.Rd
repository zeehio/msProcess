%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothLoess function

\name{msSmoothLoess}
\alias{msSmoothLoess}
\title{Fit a Smooth Curve Using Loess}
\concept{smoothingnoise estimationbaseline correction}
\usage{msSmoothLoess(x, y, span=0.1, degree=1, family="symmetric",
    index=rep(TRUE, length(x)), process="msSmoothLoess")}
\description{Fits a local regression model to a subset or set of data points and
returns a vector of fitted smooth curve values evaluated
at the original locations.}

\arguments{
\item{x}{A numeric vector of abscissa coordinates.}

\item{y}{A numeric vector of ordinate coordinates,
which must be of the same length as \code{x}.}
\item{index}{A logical vector of the same length as \code{x},
indicating the indices of \code{x} and \code{y} to use in fitting the data.
Default: \code{rep(TRUE, length(x))}.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothLoess"}.}

\item{span, degree, family}{See function \code{loess.smooth} for descriptions.}
}

\value{
A numeric vector of fitted smooth curve values evaluated at the original locations.
}
\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothKsmooth}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


