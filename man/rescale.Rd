%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the rescale function

\name{rescale}
\alias{rescale}
\title{Rescale a Vector or a Matrix to the Specified Limits}
\concept{utility}
\usage{rescale(x, range.=c(0,1), ...)}
\description{Linear scales a vector or matrix to the specified range.}

\arguments{
\item{x}{a vector as defined by \code{isVectorAtomic}, a \code{signalSeries} object,
or a \code{matrix}.}
\item{...}{additional data to be rescaled. Each additional input is scaled
in the exact same way as the \code{x} input. This provides relative scaling functionality
to the user.}

\item{range.}{a two-element numeric vector containing the upper and lower bounds
for the scaled data: Default: \code{c(0,1)}.}
}
\seealso{
\code{\link[base]{scale}}.}

\examples{
rescale(1:10,c(-3,-5))
rescale(-1:4, c(0,1), 1:3, 0:1)
}
\keyword{manip}

