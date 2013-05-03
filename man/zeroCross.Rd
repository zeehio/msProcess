%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the zeroCross function

\name{zeroCross}
\alias{zeroCross}
\title{Find the Zero Crossing of a Time Series}
\concept{utility}
\usage{zeroCross(x, slope="positive")}
\description{Finds positive-negative of negative-positive zero crossings of the input time series.}

\arguments{
\item{x}{a numeric vector representing a time series.}
\item{slope}{a character string defining the typeo of zero-crossings to find:
specifying a \code{"negative"} slope identifies all positive- to negative-valued zero-crossings,
while a \code{"positive"} slope identifies all negative- to positive-valued zero crossings.
Default: \code{"positive"}.}
}
\seealso{
\code{\link{msExtrema}}.}

\examples{
x <- sin(seq(0,10,length=1000))
pos <- zeroCross(x, "positive")
neg <- zeroCross(x, "negative")
plot(x, cex=0.5, type="b")
abline(v=pos, col="red", lty="solid")
abline(v=neg, col="blue", lty="dashed")
}
\keyword{manip}

