%%  WARNING: This file was automatically generated from the associated 
%%  ms_sim.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_sim.mid file in the project MID directory. Once the 
%%  ms_sim.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the proteins function

\name{proteins}
\alias{proteins}
\alias{proteins-class}
\alias{[,proteins-method}
\alias{[<-,proteins,ANY,ANY,ANY-method}
\alias{[<-,proteins,ANY,ANY,proteins-method}
\alias{Arith,proteins,proteins-method}
\alias{Arith,proteins,integer-method}
\alias{Arith,proteins,missing-method}
\alias{Arith,numeric,proteins-method}
\alias{Compare,proteins,proteins-method}
\alias{Compare,proteins,integer-method}
\alias{Compare,numeric,proteins-method}
\alias{Logic,proteins,ANY-method}
\alias{Logic,ANY,proteins-method}
\alias{Math,proteins-method}
\alias{Math2,proteins-method}
\alias{plot,proteins,missing-method}
\alias{show,proteins-method}
\alias{Summary,proteins-method}
\title{Class Representing a Protein Mixture or Sample}
\concept{data structure}
\usage{proteins(masses, counts)}
\description{Class slots:
\describe{
\item{masses}{A numeric vector of protein masses in daltons.}

\item{counts}{An integer vector of protein counts/abundance.}}}

\arguments{
\item{masses}{A positive numeric vector of protein masses in daltons,
whose elements should be unique.}

\item{counts}{A positive integer vector of protein counts/abundance,
which should have the same length as \code{masses}.}

}

\section{S3 METHODS}{
\describe{

\item{Arith}{Arith group generic functions.

Usage: FUN(x, y) or x op y
\describe{
\item{FUN}{a member of the \code{Arith} group generic functions,
which include \code{+}, \code{-}, \code{*},\eqn{\mbox{\textasciicircum}}{^},
\%\%, \%/\%, and \code{/}.}

\item{x}{a \code{proteins} object or a numeric vector.}

\item{y}{a \code{proteins} object or an integer vector.}}


If both \code{x} and \code{y} are objects of \code{proteins},
only \code{+} and \code{-} apply as mixing two protein samples and
taking part of a protein sample away, respectively.
If \code{x} is a numeric and \code{y} is an objects of \code{proteins},
the \code{masses} of \code{y} will be modified according to the operation.
If \code{x} is an objects of \code{proteins} and \code{y} is an integer,
the \code{counts} of \code{x} will be modified according to the operation.}

\item{Compare}{Compare group generic functions.

Usage: FUN(x, y)
\describe{
\item{FUN}{a member of the \code{Compare} group generic functions,
which include \eqn{==}, \eqn{>}, \eqn{<}, \eqn{!=}, \eqn{<=},
\eqn{>=}, and \eqn{compare}.}

\item{x}{a \code{proteins} object or a numeric vector.}

\item{y}{a \code{proteins} object or a numeric vector.}}


If both \code{x} and \code{y} are objects of \code{proteins},
only \eqn{==} and \eqn{!=} apply .
If \code{x} is a numeric and \code{y} is an objects of \code{proteins},
the \code{masses} of \code{y} will be compared according to the operation.
If \code{x} is an objects of \code{proteins} and \code{y} is a numeric,
the \code{counts} of \code{x} will be compared according to the operation.}

\item{Math, Math2, Logic}{Math, Math2, Logic group generic functions
are not defined for class \code{proteins}.}

\item{Summary}{Summary group generic functions.

Usage: FUN(x)
\describe{
\item{FUN}{a member of the \code{Summary} group generic functions,
which include \code{max}, \code{min}, \code{range}, \code{prod}, \code{sum},
\code{any}, and \code{all}.}

\item{x}{a \code{proteins} object.}}}

\item{[}{Extract parts of a \code{proteins} object.

Usage: x[i]
\describe{
\item{x}{a \code{proteins} object.}

\item{i}{a subscript expression identifying the proteins to extract.}}}

\item{[@mt{<-}}{Replace the counts of parts of a \code{proteins} object.

Usage: x[i]\eqn{<-}value
\describe{
\item{x}{a \code{proteins} object.}

\item{i}{a subscript expression identifying the protein counts to replace.}

\item{value}{a \code{proteins} object or an integer vector.}}}

\item{show}{Display a \code{proteins} object.

Usage: show(object) or object
\describe{
\item{object}{a \code{proteins} object.}}}

\item{xyCall}{Make all functions that naturally relate to points in the plane
work with objects of class \code{proteins},
including \code{plot}, \code{lines}, \code{points}, and etc.
This generic function is not meant to be called directly.

Usage: xyCall(x, y, FUN, ..., xexpr, yexpr)
\describe{
\item{x}{a \code{proteins} object.}

\item{y}{missing.}

\item{FUN}{a function to be called that have arguments \code{x}, \code{y}, ....}

\item{xexpr}{the S object representing the \code{x} argument to \code{FUN} unevaluated.}

\item{yexpr}{the S object representing the \code{y} argument to \code{FUN} unevaluated.}}}

}
}

\references{
Coombes, K.R., Koomen, J.M., Baggerly, K.A., Morris, J.S., Kobayashi, R.,
``Understanding the characteristics of mass spectrometry data
through the use of simulation,"
\emph{Cancer Informatics}, 2005(1):41--52, 2005.
}

\seealso{
\code{\link{calibrants}}, \code{\link{spectrometer}}.}

\examples{
## generate two protein samples 
sam1 <- proteins(masses=c(1, 95, 190), counts=as.integer(c(500, 3000, 10000)))
sam2 <- proteins(masses=10000+200*(0:3), counts=as.integer(c(12000, 4000, 2000, 1000)))

## print the synopsis of the protein samples 
sam1
sam2

## mix the protein samples 
sam <- sam1 + sam2

## visualize the protein mixture 
plot(sam, type="h")
}
\keyword{manip}

