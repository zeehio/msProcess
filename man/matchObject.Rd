%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the matchObject function

\name{matchObject}
\alias{matchObject}
\title{Search for an Object Name Matching a Character String
on the Data Directory Search List}
\concept{utility}
\usage{matchObject(what, ignore.case=TRUE)}
\description{Returns a character string which is the name of an S-PLUS object
in a position on the search list.}

\arguments{
\item{what}{a character string to be searched as the name of an S-PLUS object.}
\item{ignore.case}{if TRUE upper- and lowercase analogs are considered equivalent when matching.}
}

\value{
a character string which is the name of an S-PLUS object
in a position on the search list is returned.
}
\seealso{
\code{\link[base]{ls}}, \code{\link[base:ls]{objects}}.}

\examples{
matchObject("max")
matchObject("lm")
matchObject("glm")
}
\keyword{utilities}

