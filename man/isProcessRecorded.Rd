%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the isProcessRecorded function

\name{isProcessRecorded}
\alias{isProcessRecorded}
\title{Verifies the Existence of a Recorded Process in a Thrown Event}
\concept{history}
\usage{isProcessRecorded(process, histname="event.history", envir=NULL)}
\description{Checks for the existence of a previously recorded process
in a thrown event history.}

\arguments{
\item{process}{a character string defining the name of the process.}
\item{envir}{the \code{frame} in S-PLUS (or \code{environment} in R) designated for the processing
and storage of pipeline history data. Default: \code{msProcessEnv},
a global environment impicitly set by a previous
call to \code{\link{throwEvent}}. In general, the user should rely
on the specified default value.}

\item{histname}{a character string defining the name of the history
variable stored in the specified frame. Default: \code{"event.history"}.}
}

\value{
a logical value. If \code{TRUE}, the process has already
been recorded in the thrown event.
}
\seealso{
\code{\link{throwEvent}}, \code{\link{catchEvent}}, \code{\link{assignEvent}}, \code{\link{eventHistory}}.}

\examples{
## throw an event 
envir <- msGlobalEnv()
throwEvent("The 2005 British Open Championship", envir=envir)

## assign data to the thrown event 
record  <- list(Winner="Tiger Woods")
process <- "champion"
assignEvent(record, process)

## verify process has been recorded: TRUE 
isProcessRecorded(process)

## catch event 
catchEvent(NULL)

## once event has been caught, isProcessRecorded returns FALSE 
isProcessRecorded(process)
}
\keyword{utilities}

