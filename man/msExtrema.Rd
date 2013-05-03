%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msExtrema function

\name{msExtrema}
\alias{msExtrema}
\title{Find both Local Maxima and Local Minima}
\concept{utility}
\usage{msExtrema(x, span=3)}
\description{Finds both local extrema in a vector, time series,
or in each column of a matrix.}

\arguments{
\item{x}{a vector, time series, or matrix. If \code{x} is a matrix,
\code{msExtrema} finds both local maxima and local minima
in each column of \code{x}.}
\item{span}{a numeric value indicating the span of the local extreme.
A local maximum (minimum) is defined as an element in a sequence
which is greater (smaller) than all other elements
within a window of width span centered at that element.
The default value is 3, meaning that a maximum (minimum) is bigger (smaller)
than both of its neighbors.}
}

\value{
a list with two elements:

\item{index.max}{an object like x of logical values.
Values that are TRUE correspond to local maxima in the data.}

\item{index.min}{an object like x of logical values.
Values that are TRUE correspond to local minima in the data.}
}
\seealso{
\code{\link{zeroCross}}.}

\examples{
## create a synthetic sequence 
a<-c(3,3,2,1,1,2,1,2,3,3,1,1,1,3,3,3,2,2,2,1,1,1,1,2,3)

## detect local maxima and minima 
maxmin <- msExtrema(a)

## visualize the result 
par(mfrow=c(1,1))
plot(a, type="l")
points((1:length(a))[maxmin$index.max],
    a[maxmin$index.max], col=2, pch=1)
points((1:length(a))[maxmin$index.min],
    a[maxmin$index.min], col=3, pch=2)
if (!is.R()){
    legend(x=18, y=3, col=2:3, marks=1:2, legend=c("maxima", "minima"))
} else {
    legend(x=18, y=3, col=2:3, pch=1:2, legend=c("maxima", "minima"))
}
}
\keyword{manip}

