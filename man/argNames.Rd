%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the argNames function

\name{argNames}
\alias{argNames}
\title{Display the Argument List of a Function}
\concept{utility}
\usage{argNames(x)}
\description{Displays only the argument names for a function.}

\arguments{
\item{x}{the name of a function. A character string is also accepted, and is useful
when the function name would not be interpreted as a name by the parser.}

}

\value{
the names of the arguments.
}
\seealso{
\code{\link[base]{args}}.}

\examples{
argNames(lm)
argNames("lm")
}
\keyword{utilities}

