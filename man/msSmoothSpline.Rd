%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothSpline function

\name{msSmoothSpline}
\alias{msSmoothSpline}
\title{Fit a Smooth Curve Using Cubic Spline}
\concept{smoothingnoise estimationbaseline correction}
\usage{msSmoothSpline(x, y, df=30, spar=0, cv=FALSE, all.knots=FALSE,
    df.offset=0, penalty=1, index=rep(TRUE, length(x)),
    process="msSmoothSpline")}
\description{Fits a cubic spline curve to a subset or set of data points and
returns a vector of fitted smooth curve values evaluated
at the original locations.}

\arguments{
\item{x}{A numeric vector of abscissa values.}

\item{y}{A numeric vector of ordinate values,
which must be of the same length of \code{x}.}
\item{df, spar, cv, all.knots, df.offset, penalty}{See function \code{smooth.spline} for descriptions.}

\item{index}{A logical vector of the same length of \code{x}
indicating the elements to be used in the fitting.
Deafult: \code{rep(TRUE, length(x))}.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothSpline"}.}
}

\value{
A numeric vector of fitted smooth curve values evaluated at the original locations.
}
\seealso{
\code{\link{msSmoothApprox}}, \code{\link{msSmoothKsmooth}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


