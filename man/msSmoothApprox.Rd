%%  WARNING: This file was automatically generated from the associated 
%%  ms_smooth.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_smooth.mid file in the project MID directory. Once the 
%%  ms_smooth.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msSmoothApprox function

\name{msSmoothApprox}
\alias{msSmoothApprox}
\title{Piecewise Linear Baseline Estimation}
\concept{baseline correction}
\usage{msSmoothApprox(x, y, method="linear", rule=2, f=0.5,
    index=rep(TRUE, length(x)), process="msSmoothApprox")}
\description{Estimates the baseline of a spectrum as a linear or
constant interpolation of the local minima of a spectrum.}

\arguments{
\item{x}{A numeric vector representing the \code{m/z} values of a spectrum.}

\item{y}{A numeric vector representing the intensity values of the spectrum corresponding to the
specified \code{m/z} values.}
\item{f}{A numeric scalar used when \code{method="constant"},
which determines a blend of the left and right side \code{y} values.
e.g., suppose we want an interpolated value between x1 and x2
(with corresponding y values y1 and y2).
Then the interpolated value is \eqn{(1-f)*y1+f*y2}.
Thus, if \eqn{f=0}, the left y-value is used, if \eqn{f=1}, the right y-value,
and if f is between 0 and 1, an intermediate value is used.
Default: 0.5.}

\item{index}{A logical vector indicating the local minima to be used
to approximate the baseline. Default: \code{rep(TRUE, length(x))}.}

\item{method}{A character string describing the method to be used in approximating the baseline.
This must be either \code{"linear"} or \code{"constant"}. Default: \code{"linear"}.}

\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data.
Default: \code{"msSmoothApprox"}.}

\item{rule}{An integer (either 2 or 3) describing the rule to be used for values
that are outside the range of the minima of \code{x}.
%If \code{rule=1}, NAs will be supplied for any such points.
If \code{rule=2}, the \code{y} values corresponding to the extreme \code{x} values will be used.
If \code{rule=3}, linear extrapolation is used.
Default: 2.}
}

\value{
A numeric vector representing the estimated piece-wise linear baseline.
}
\seealso{
\code{\link{msSmoothKsmooth}}, \code{\link{msSmoothLoess}}, \code{\link{msSmoothMean}}, \code{\link{msSmoothMonotone}}, \code{\link{msSmoothSpline}}, \code{\link{msSmoothSupsmu}}.}
\keyword{manip}


