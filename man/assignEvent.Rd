%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the assignEvent function

\name{assignEvent}
\alias{assignEvent}
\title{Update a Previously Thrown History Event
with New Information}
\concept{history}
\usage{assignEvent(record, process=NULL, histname="event.history", envir=NULL)}
\description{Updates a history event previously thrown to a specified frame (or envirnoment in R).
Typically, a caller function will have thrown the event
while the callee(s) update the event information
using the \code{assignEvent} function. That information
can then be retrieved (typically by the caller) using
\code{catchEvent}.}

\arguments{
\item{record}{a list containing named character vectors describing
the event in the form
\code{list(proc2="Description 1", proc2="Description 2")} and so on.
Here, \emph{EventName} is a character string defining the name of the event,
and the named list variables \emph{proc1} and \emph{proc2} are character strings
that define the specific proceedings for that event. Each of these proceedings
is descibed more thoroughly by the assigned character string.
If, for a given event, the user wishes only to register the name
of the event sans extra proceedings information, set this variable
to a blank character string \code{""} and specify only the second \code{event}
argument.}
\item{envir}{the \code{frame} in S-PLUS (or \code{environment} in R) designated for the processing
and storage of pipeline history data. Default: \code{msProcessEnv},
a global environment impicitly set by a previous
call to \code{\link{throwEvent}}. In general, the user should rely
on the specified default value.}

\item{histname}{a character string defining the name of the history
variable stored in the specified frame. Default: \code{"event.history"}.}

\item{process}{a character string defining a title for the current
information being recorded. This typically serves as a flag
for other functions so that the same process is not written
more than once. This can be checked using the \code{isProcessRecorded} function.}
}

\value{
no output is returned directly. Instead, the variable \code{histname}
in frame \code{frame} is updated with the supplied event information.
}
\note{If assignable, the specified \code{histname} object is updated in the specified
frame with the new process information.}

\seealso{
\code{\link{throwEvent}}, \code{\link{catchEvent}}, \code{\link{isProcessRecorded}}.}

\examples{
## throw an event in the global frame 
envir <- msGlobalEnv()
throwEvent("The 2005 British Open Championship", envir=envir)

## assign data to the thrown event 
record  <- list(Winner="Tiger Woods")
process <- "champion"
assignEvent(record, "champion", envir=envir)

## verify process has been recorded: TRUE 
isProcessRecorded(process, envir=envir)

## catch event 
catchEvent(NULL, envir=envir)
}
\keyword{utilities}

