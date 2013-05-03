%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothMean function

\name{msSmoothMean}
\alias{msSmoothMean}
\title{Fit a Smooth Curve Using Moving Average}
\concept{smoothingnoise estimation}
\usage{msSmoothMean(y, half.span=250, process="msSmoothMean")}
\description{Fits a smooth curve to a vector using moving average and
returns a vector of fitted smooth curve values evaluated
at the original locations.}

\arguments{
\item{y}{A numeric vector.}
\item{half.span}{A numeric value denoting half of the window width when performing average.
Default: 250.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothMean"}.}
}

\value{
A vector of fitted smooth curve values evaluated at the original locations.
}
\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothKsmooth}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


