%%  WARNING: This file was automatically generated from the associated 
%%  ms_hist.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_hist.mid file in the project MID directory. Once the 
%%  ms_hist.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the eventHistory, [.eventHistory, print.eventHistory functions

\name{eventHistory}
\alias{eventHistory}
\alias{[.eventHistory}
\alias{print.eventHistory}
\title{Event History Creation}
\concept{history}
\usage{eventHistory(x, ..., sub.label="   ", time.stamp=date(), action="append")}
\description{The \code{S+Proteome} module provides a wealth of preprocessing
functionality, some or all of which may be used as a preface
for subsequent classification investigations. This function
allows the user to document the processing pipeline by adding
text to a particular attribute of an object
that describes the current processing state.
Each process in the pipeline is referred to as an \emph{event}.
This function will typically not be called directly by
the user and will alternatively be called from within
various preprocessing functions.}

\arguments{
\item{x}{an object of any class.}

\item{...}{one or more named lists, each containing named character vectors describing
an event to register. Each list must be in the form
EventName = list(proc2="Description 1", proc2="Description 2") and so on.
Here, \emph{EventName} is a character string defining the name of the event,
and the named list variables \emph{proc1} and \emph{proc2} are character strings
that define the specific proceedings for that event. Each of these proceedings
is descibed more thoroughly by the assigned character string.
If for a given event, the user wishes only to register the name
of the event sans extra proceedings information, it is allowable
to make the call ala \code{eventHistory(x, "Event A", "Event B")} and
so on. In this case, each character string is taken to mean
the name of the corresponding event.}
\item{action}{a character string defining the action to take with the new history
information. Supported values are as follows:
\describe{
\item{prepend}{Prepend new history to the existing event history.}

\item{append}{Append new history to the existing event history.}

\item{replace}{Replace the event history with the new history.}

\item{merge}{Update old events which have a common event
name with new events. The uncommon events are appended.}}

Default: \code{"append"}.}

\item{sub.label}{a character string used to preface the proceedings lines for each event
when a history summary is printed on the command line.
Default: \code{"   " (3 blanks)}.}

\item{time.stamp}{a character string defining a time stamp for the process(es) being documented.
This time stamp will be automatically added to each process in the event.
Default: \code{date()}.}
}

\value{
a replication of the original object, with an attached attribute
of class \code{eventHistory} containing a vector of formatted
character strings defining the processing history of the input object.
}

\section{S3 METHODS}{
\describe{

\item{@lsb}{event data access. Input either a character
string or an integer defining the registered event in
the history.}

\item{print}{pretty-prints the event history. Optional arguments
for this method are
\describe{
\item{pre}{Character string to preface each event header.
Default: \code{paste("[", attr(x, "index"), "]", sep = "")}.}}}

}
}

\seealso{
\code{\link{assignEvent}}, \code{\link{isProcessRecorded}}, \code{\link{existHistory}}, \code{\link{getHistory}}, \code{\link{eventHistory}}.}

\examples{
## create a list of simple objects 
z <- list(dog="chihuahua", vals=1:5, colors=c("red","green","blue"))

## remove mean from vals and document as a history 
## event 
z$vals <- z$vals - mean(z$vals)
z <- eventHistory(z, "Process A"=list(values="mean removed"))

## now sort the colors and change the dog name, 
## and document both actions in the same call 
## to msHistory as separate processes 
z$colors <- sort(z$colors)
z$dog    <- "pomeranian"
z <- eventHistory(z, "Process B"=list(colors="sorted alphabetically"),
    "Process C"=list(dog="name change", result="more hair"))

## add an event without proceedings 
z <- eventHistory(z, "Event A", "Event B")

## print the history 
print(getHistory(z))

## replace some of the events with new information 
z <- eventHistory(z, "Event A"=list(show="pony"), "Process A",
action="merge")
print(getHistory(z))

## prepend some new events 
z <- eventHistory(z, new=list(alpha="first greek letter"),
    action="prepend")
print(getHistory(z))

## replace event history altogether 
z <- eventHistory(z, newest=list(omega="final greek letter"),
    action="replace")
print(getHistory(z))

## return an object of class eventHistory, i.e., 
## not attached to any other object 
eventHistory(NULL,
    symphony=list(string="violins and cellos", percussion="drums",
       reed="flutes and oboes"))
}
\keyword{utilities}

