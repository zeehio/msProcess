%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the getHistory function

\name{getHistory}
\alias{getHistory}
\title{Extract an Event History from an Arbitrary Object}
\concept{history}
\usage{getHistory(x, event=NULL)}
\description{Extracts a registered event history from an arbitrary object or a specified
event within that history.}

\arguments{
\item{x}{an object of any class.}
\item{event}{a character string or integer defining a specific event to query.
If no such event has been registered in a history object (or the history
object does not exist) then a \code{NA} is returned.
Default: \code{NULL} (do not extract a specific event).}
}

\value{
an object of class \code{msHistory}. If \code{event} is specified, only
that event is returned in the history.
}
\seealso{
\code{\link{assignEvent}}, \code{\link{isProcessRecorded}}, \code{\link{existHistory}}, \code{\link{eventHistory}}.}

\examples{
## create a simple history 
z <- 1:5
z <- eventHistory(z, "Event A"=list(number="first", positive="yes"),
    "Event B"=list(horse="mustang"),
    "Event C"=list(string="violins and cellos", percussion="drums",
       reed="flutes and oboes"))

## extract the entire event history 
getHistory(z)

## extract only Event B (each method is 
## equivalent) 
getHistory(z,"Event B")
getHistory(z,2)
getHistory(z)["Event B"]
getHistory(z)[2]

## return entire history except the second event 
## (each method is equivalent) 
getHistory(z,-2)
getHistory(z)[-2]

## attempt to extract a non-existent event (NA is 
## returned) 
getHistory(z)["dogs"]
}
\keyword{utilities}

