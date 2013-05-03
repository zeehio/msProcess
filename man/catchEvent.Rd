%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the catchEvent function

\name{catchEvent}
\alias{catchEvent}
\title{Catch a History Event that has been Thrown}
\concept{history}
\usage{catchEvent(x, histname="event.history", envir=NULL)}
\description{Catches a history event that has been thrown, extracts
the history, and attaches/updates the history in the primary input
object.}

\arguments{
\item{x}{an object of arbitrary class. Optionally, this input
may already contain an event history in which case
the history is updated after being caught. Otherwise
the new history is attached.}
\item{envir}{the \code{frame} in S-PLUS (or \code{environment} in R) designated for the processing
and storage of pipeline history data. Default: \code{msProcessEnv},
a global environment impicitly set by a previous
call to \code{\link{throwEvent}}. In general, the user should rely
on the specified default value.}

\item{histname}{a character string defining the name of the history
variable stored in the specified frame. Default: \code{"event.history"}.}
}

\value{
a replication of \code{x} with the event history updated/attached.
}
\note{If available, the last entry of the specified \code{histname} object
(a list located in the specified frame) is extracted and written to the
input \code{x} via the \code{msSet} constructor function. If no other entries
exist after extraction, the \code{histname} list is deleted from the specified
frame.}

\seealso{
\code{\link{throwEvent}}, \code{\link{assignEvent}}, \code{\link{isProcessRecorded}}, \code{\link{eventHistory}}.}

\examples{
## throw an event 
envir <- msGlobalEnv()
throwEvent("Superbowl XL", envir=envir)

## assign data to the thrown event 
record  <- list(NFC="Seattle Seahawks", AFC="Pittsburgh Steelers")
assignEvent(record)

## catch event 
catchEvent(NULL)
}
\keyword{utilities}

