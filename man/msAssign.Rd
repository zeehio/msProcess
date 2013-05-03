%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msAssign, msExists, msGet, msGlobalEnv, msNewEnv, msRemove functions

\name{msAssign}
\alias{msAssign}
\alias{msExists}
\alias{msGet}
\alias{msGlobalEnv}
\alias{msNewEnv}
\alias{msRemove}
\title{Utility functions for maintaining pipeline processing histories}
\concept{history}
\usage{msAssign(x, value, envir)
msExists(x, envir)
msGet(x, envir)
msGlobalEnv()
msNewEnv()
msRemove(x, envir)}
\description{For mutual compatibility in S-PLUS and R,
these functions switch on \code{\link[base:is.R]{is.R}} as an interface to
the \code{\link[base]{assign}}, \code{\link[base:get]{exists}}, \code{\link[base]{get}}, and \code{\link[base:rm]{remove}}
functions. In addition, the \code{msGlobalEnv} function outputs
the environment in R and frame in S-PLUS associated with
the global work environment and session database, respectively.
The \code{msNewEnv} returns the integer 1 in S-PLUS (for frame 1, the expression frame)
or a new environment in R (ala \code{new.env}).}

\arguments{
\item{x}{a character string denoting the name of the object.}

\item{value}{any S-PLUS or R object; the value to be assigned the name in x.
Only used in the \code{msAssign} function.}

\item{envir}{the \code{frame} in S-PLUS (or \code{environment} in R) designated for the processing
and storage of pipeline history data. Default: \code{msProcessEnv},
a global environment impicitly set by a previous
call to \code{\link{throwEvent}}. In general, the user should rely
on the specified default value.}

}

\value{
the standard output of the related functions as described in the
\emph{Description} section.
}
\seealso{
\code{\link[base]{assign}}, \code{\link[base:get]{exists}}, \code{\link[base]{get}}, \code{\link[base:rm]{remove}}.}

\examples{
envir <- msGlobalEnv()
x <- "myval"

msAssign(x, 1:10, envir)
msExists(x, envir)
msGet(x, envir)
msRemove(x, envir)
msExists(x, envir)
}
\keyword{utilities}

