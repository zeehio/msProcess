%%  WARNING: This file was automatically generated from the associated 
%%  msb_set.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_set.mid file in the project MID directory. Once the 
%%  msb_set.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msList, msList-class, [.msList, merge.msList, 
%% print.msList, summary.msList, print.summary.msList, plot.msList functions

\name{msList}
\alias{msList}
\alias{msList-class}
\alias{[.msList}
\alias{merge.msList}
\alias{print.msList}
\alias{summary.msList}
\alias{summary.msList-class}
\alias{print.summary.msList}
\alias{plot.msList}
\docType{methods}
\alias{[,msList-method}
\title{S3 Class Representing a List of Spectra with Possibly Different m/z Values}
\concept{data structure}
\description{An \code{msList} object is a list of matrices with each matrix representing a spectrum.
Each matrix in the list has two columns.
The first column is named \code{"mz"} and contains the \eqn{m/z} values.
The second column is named \code{"intensity"} and contains the intensity values.
The list has also an attribute named \code{"type"}, which is of type \code{factor}
and contains the classification labels for the spectra.
An object of this class is usually generated from the function \code{msImport()}
in package \code{proteome}.}


\section{S3 METHODS}{
\describe{

\item{[}{extract or replace parts of an \code{msList} object.

Usage: x[i]
\describe{
\item{x}{an \code{msList} object.}

\item{i}{a subscript expression used to identify the spectra to extract or replace.}}}

\item{merge}{merges multiple \code{msList} objects.

Usage: merge(...)
\describe{
\item{...}{\code{msList} objects.}}}

\item{plot}{plot a single spectrum from an \code{msList} object.

Usage: plot(x, index=1, type="l", add=FALSE, ...)
\describe{
\item{x}{an \code{msList} object.}

\item{index}{a single numeric value or character string
specifying the spectrum to be plotted. The default is 1.}

\item{type}{a single character specifying the type of plot.
see fucntion \code{par} for details. The default is \code{"l"}}

\item{add}{A logical value. If \code{TRUE},
the plot is added using the current \code{par()} layout.
Otherwise a new plot is produced. Default: \code{FALSE}.}

\item{...}{other graphical parameters passed to the \code{plot} function.}}}

\item{print}{prints an \code{msList} object.

Usage: print(x, justify="left", sep=":", ...) or x
\describe{
\item{x}{an \code{msList} object.}

\item{justify}{a character string giving the justification
of the numbers relative to each other.
The choices are "none", "left", "right" and "decimal".
Only the first letter needs to be given.}

\item{sep}{a character string to be inserted between text and values.
The default is a colon.}}}

\item{summary}{provides a synopsis of an \code{msList} object.

Usage: summary(x)
\describe{
\item{x}{an \code{msList} object.}}}

}
}

\seealso{
\code{\link{msSet}}.}

\examples{
if (!exists("qclist")) data("qclist", package="msProcess")

## print an msList object 
qclist

## print the synopsis of an msList object 
summary(qclist)

## plot the first spectrum from an msList object 
plot(qclist, index=1)
}
\keyword{classes}

