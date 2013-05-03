%%  WARNING: This file was automatically generated from the associated
%%  ms_demo.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_demo.mid file in the project MID directory. Once the
%%  ms_demo.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msHelp function

\name{msHelp}
\alias{msHelp}
\title{Open msProcess Help Files in S-PLUS}
\concept{demonstration}
\usage{msHelp(keyword="", section="msProcess")}
\description{Used to quickly open help files related to the msProcess package.}
\arguments{

\item{keyword}{A character string specifying a particular S+Proteome function or object.
Default: \code{""}.}

\item{section}{A character string giving the name of a module or a library
associated with the S+Proteome module. Default: \code{"proteome"}.}
}
\seealso{
\code{\link[utils]{help}.}}

\examples{
## open the S+Proteome help file
msHelp()
}
\keyword{manip}

