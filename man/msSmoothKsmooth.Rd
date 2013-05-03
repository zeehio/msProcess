%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothKsmooth function

\name{msSmoothKsmooth}
\alias{msSmoothKsmooth}
\title{Fit a Smooth Curve Using Kernel Smoothers}
\concept{smoothingnoise estimation}
\usage{msSmoothKsmooth(x, y, kernel="box", bandwidth=100, process="msSmoothKsmooth")}
\description{Fits a smooth curve to a set of data points using kernel smoothers and
returns a vector of fitted smooth curve values evaluated
at the original locations.}

\arguments{
\item{x}{A numeric vector of abscissa values.}

\item{y}{A numeric vector of ordinate values,
which must be of the same length as \code{x}.}
\item{kernel, bandwidth}{See fucntion \code{ksmooth} for descriptions.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothKsmooth"}.}
}

\value{
A numeric vector of fitted smooth curve values evaluated at the original locations.
}
\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


