%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the throwEvent function

\name{throwEvent}
\alias{throwEvent}
\title{Throw a History Event to a Specified Frame}
\concept{history}
\usage{throwEvent(x, histname="event.history", envir=msNewEnv())}
\description{Throws a history event with a specified name to a specified frame in S-PLUS
(or environment in R).
Typically, this function will be issued within a caller with
the expectation that the thrown history will be added to
by the callee(s). The history can then be caught by the caller
using the \code{catchHistory} function.}

\arguments{
\item{x}{a character string defining the event.}
\item{envir}{the \code{frame} in S-PLUS (or \code{environment} in R) designated for the processing
and storage of pipeline history data. Default: \code{msNewEnv()}.}

\item{histname}{a character string defining the name of the history
variable stored in the specified frame. Default: \code{"event.history"}.}
}

\value{
a list named \code{histname} containing the following objects
is written to the specified frame.

\item{name}{A character string defining the name of the event.}

\item{history}{The actual history object.}

\item{recorded}{A vector of chacter strings maintaining a record
of all processes that have been recorded for the event.}

Typically, these list objects will not be accessed directly by the user.
Rather, this information is updated and retrieved via the \code{throwEvent}
and \code{catchEvent} function.
}
\seealso{
\code{\link{catchEvent}}, \code{\link{assignEvent}}, \code{\link{isProcessRecorded}}, \code{\link{eventHistory}}.}

\examples{
## throw an event 
envir <- msGlobalEnv()
throwEvent("Wimbledon 2005", envir=envir)

## assign data to the thrown event 
record  <- list(Men="Roger Federer", Women="Venus Williams")
assignEvent(record)

## catch event 
catchEvent(NULL)
}
\keyword{utilities}

