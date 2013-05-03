%%  WARNING: This file was automatically generated from the associated
%%  ms_demo.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_demo.mid file in the project MID directory. Once the
%%  ms_demo.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msHelp function

\name{msLaunchExample}
\alias{msLaunchExample}
\title{Open msProcess Example/Demo Files in S-PLUS}
\concept{demonstration}
\usage{msLaunchExample(x, open=TRUE, run=TRUE, type="R-ex")}
\description{Used to quickly open help files related to the msProcess package.}
\arguments{

\item{x}{A character string specifying the name of a particular example or demo script.}

\item{open}{A logical indicating open the script or not. Default: \code{TRUE}.}

\item{run}{A logical indicating run the script or not. Default: \code{TRUE}.}

\item{type}{A character string giving the name of a subdirectory 
where the example or demo script is located. Default: \code{"R-ex"}.}
}
\seealso{
\code{\link[utils]{example}.}}

\examples{
\dontrun{
if (!is.R() && is.ui.app("s+gui")) {
## open a msProcess example file
msLaunchExample("msDenoise", open=TRUE, run=FALSE, type="R-ex")
}
}
}
\keyword{manip}

