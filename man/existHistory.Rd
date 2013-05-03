%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the existHistory function

\name{existHistory}
\alias{existHistory}
\title{Verify Existence of an Embedded History Object}
\concept{history}
\usage{existHistory(x, event=NULL)}
\description{Checks for the existence of an object of class \code{eventHistory}
attached an as attribute to the input object. Additionally,
the user can seek the existence of a particular event within
that history.}

\arguments{
\item{x}{an object of arbitrary class.}
\item{event}{a character string defining a registered event to query.
If no such event has been registered in a history object (or the history
object does not exist) then a \code{FALSE} is returned. Default: \code{NULL}
(do not search for a specific event).}
}

\value{
a logical value. A \code{TRUE} value is returned if a history object
exists as an attribute of the primary input object \code{x}.
If \code{event} is also specified, the output will be a logical value
defining whether or not that event has been registered in the
history object.
}
\seealso{
\code{\link{assignEvent}}, \code{\link{isProcessRecorded}}, \code{\link{getHistory}}, \code{\link{eventHistory}}.}

\examples{
## check for the existence of a registered history 
## in an object without one (FALSE) 
z <- 1:5
existHistory(z)

## create a simple history 
z <- eventHistory(z, "Event A"=list(number="first", positive="yes"),
    "Event B"=list(horse="mustang"))

## check for a registered history (TRUE) 
existHistory(z)

## check to see if "Event A" has been registered 
## (TRUE) 
existHistory(z,"Event A")

## check to see if "Event D" has been registered 
## (FALSE) 
existHistory(z,"Event D")
}
\keyword{utilities}

