%%  WARNING: This file was automatically generated from the associated
%%  msb_set.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the msb_set.mid file in the project MID directory. Once the
%%  msb_set.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msSet, msSet-class, [.msSet, apply.msSet, print.msSet, summary.msSet, print.summary.msSet, plot.msSet, image.msSet functions

\name{msSet}
\alias{msSet}
\alias{msSet-class}
\alias{[.msSet}
\alias{print.msSet}
\alias{summary.msSet}
\alias{print.summary.msSet}
\alias{plot.msSet}
\alias{image.msSet}
\docType{methods}
\alias{[,msSet-method}
\title{S3 Class Representing a Set of Spectra with Common m/z Values}
\concept{data structure}
\usage{msSet(x, mz=NULL, type=NULL, data.name=NULL, ...)}
\description{An object of class \code{msSet} is a list containing three required components:
\describe{
\item{mz}{A vector of mass/charge values.}

\item{type}{A factor denoting the disease label for each spectrum.}

\item{intensity}{A matrix of intensity values.
Each column corresponds to a spectrum and
each row corresponds to an \eqn{m/z} value.
The number of rows should be the same as the length of \code{mz}.
The number of columns should be the same as the length of \code{type}.}}

It can also have additional elements such as
\code{"noise"}, \code{"noise.local"}, \code{"baseline"}, \code{"tic"},
\code{"peak.list"}, \code{"peak.class"}, \code{"peak.matrix"},
\code{"intensity.mean"}, \code{"noise.mean"}, and \code{"noise.local.mean"}.}

\arguments{
\item{x}{a \code{vector}, a \code{matrix}, a \code{data.frame}, or an object of class \code{msSet}.}
\item{...}{additional elements to be added.}

\item{data.name}{a character string denoting the name for the dataset.
Default: NULL, i.e., using the variable name of the \code{x} argument.}

\item{mz}{a vector of \eqn{m/z} values. It must be of the same length as \code{x} if \code{x} is a vector,
or its length must be the same as the number of rows of \code{x}
if \code{x} is a \code{matrix} or \code{data.frame}.}

\item{type}{a factor denoting the disease label for each spectrum.
This argument must be a scalar (length=1) if \code{x} is a \code{vector},
or a vector whose length is the same as the number of rows of \code{x}
in the case that \code{x} is a \code{matrix} or \code{data.frame}.}
}

\value{
an object of class \code{msSet}.
}

\section{S3 METHODS}{
\describe{

\item{[}{spectra extraction from an \code{msSet} object.

Usage: x[i, j]
\describe{
\item{x}{an \code{msSet} object.}

\item{i}{a subscript expression used to identify the \eqn{m/z} elements to extract or replace.}

\item{j}{a subscript expression used to identify the spectra to extract or replace.}}}

\item{image}{displays a set of spectra as an image.

Usage: image(x, what="spectra", subset=NULL, xaxis="mass", xlim=NULL,
    add=FALSE, xaxs="i", yaxs="i", ...)
\describe{
\item{x}{an \code{msSet} object.}

\item{what}{a character string specifying what to be imaged.
The options are \code{"spectra"}, \code{"noise"}, \code{"noise.local"},
\code{"baseline"}, \code{"peak.list"}, and \code{"peak.matrix"}.
Default: \code{"spectra"}.
If \code{what=="peak.matrix"},
the arguments \code{subset}, \code{xaxis}, and \code{xlim} are ignored.}

\item{subset}{a numeric vector indicating the spectra numbers to be included.
All spectra are included by default.}

\item{xaxis}{a character string specifying what to be used as the x-axis.
The options are \code{"mass"} and \code{"time"}.
Default: \code{"time"}.}

\item{xlim}{a numeric vector with two elements specifying the range for the x axis.
Its first element should be less than its second element.
If it is \code{NULL} (by default), the entire range of the data is plotted.
If one of its element is \code{NA}, it is replaced with the corresponding limit.}

\item{add}{a logical value. If \code{TRUE}, the plot is added using the current
\code{par()} layout. Otherwise a new plot is produced. Default: \code{FALSE}.}

\item{xaxs}{a character specifying the style of x axis interval calculation.
Default: "i". See \code{par} for details.}

\item{yaxs}{a character specifying the style of y axis interval calculation.
Default: "i". See \code{par} for details.}

\item{...}{Other graphical parameters passed to the \code{image} function.}}}

\item{plot}{visualizes a particular processing step.

Usage: plot(x, process="msPrepare", subset=1, offset=0,
xaxis="mass", xlim=NULL, pch=1, lty=1:2, col=1:8, lwd=1, add=FALSE, ...)
\describe{
\item{x}{an \code{msSet} object.}

\item{process}{a character string specifying the process to be visualized.
The options are \code{"msPrepare"}, \code{"msDenoise"},
\code{"msNoise"}, \code{"msDetrend"},
\code{"msNormalize"}, \code{"msPeak"},
\code{"msAlign"}. Default: \code{"msPrepare"}.}

\item{subset}{a numeric vector or \code{NULL} indicating the index of the
spectra to be plotted. Default: 1, i.e., the first spectrum.
If \code{NULL}, the entire set of spectra are plotted.}

\item{offset}{a numeric scalar representing the vertical offset
to apply between each spectrum.
If \code{NULL}, the offset is automatically calculated.
Default: \code{NULL}.}

\item{xaxis}{a character string specifying what to be used as the x-axis.
The options are \code{"mass"} and \code{"time"}.
Default: \code{"time"}.}

\item{xlim}{a numeric vector with two elements specifying the range for the x axis.
Its first element should be less than its second element.
If it is \code{NULL} (by default), the entire range of the data is plotted.
If one of its element is \code{NA}, it is replaced with the corresponding limit.}

\item{pch}{a single character or integer denoting the plotting character for the peaks.
Default: 1, i.e., a circle.}

\item{lty}{an integer vector of length 2 denoting the line type
for the two lines associated with each spectrum to be plotted.
If it is a single integer, it will be used cyclically.
If its length is more than 2, only the first two elements will be used.}

\item{col}{a vector of integers denoting the colors for the spectra to be plotted.
Colors are used cyclically.}

\item{lwd}{a integer vector of length 2 denoting the line width
for the two lines associated with each spectrum to be plotted, device dependent.
If it is a single integer, it will be used cyclically.
If its length is more than 2, only the first two elements will be used.
Width 1 is the standard width for the device.
Many devices cannot change line width. Default: 1.}

\item{add}{a logical value. If \code{TRUE}, the plot is added using the current
\code{par()} layout. Otherwise a new plot is produced. Default: \code{FALSE}.}

\item{...}{Other graphical parameters passed to the underlying plotting functions.}}}

\item{print}{prints an \code{msSet} object.

Usage: print(x, justify="left", sep=":", ...) or x
\describe{
\item{x}{an \code{msSet} object.}

\item{justify}{a character string giving the justification
of the numbers relative to each other.
The choices are \code{"none"}, \code{"left"}, \code{"right"} and \code{"decimal"}.
Only the first letter needs to be given.}

\item{sep}{a character string to be inserted between text and values.
The default is a colon.}}}

\item{summary}{provides a synopsis of an \code{msSet} object.

Usage: summary(x)
\describe{
\item{x}{an \code{msSet} object.}}}

}
}

\seealso{
\code{\link{msList}}, \code{\link{msPlot}}, \code{\link{apply.msSet}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## plot a few spectra from qcset
plot(qcset, subset=1:5)

## image of all spectra in qcset
image(qcset)
}
\keyword{classes}

