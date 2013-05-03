%%  WARNING: This file was automatically generated from the associated 
%%  ms_quan.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_quan.mid file in the project MID directory. Once the 
%%  ms_quan.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msQuantifyCount function

\name{msQuantifyCount}
\alias{msQuantifyCount}
\title{Count of Spectral Peaks Associated with a Peak Class}
\concept{peak quantification}
\usage{msQuantifyCount(x, xnew=NULL)}
\description{Using the \code{peak.list} element of a given \code{msSet} object,
this function returns a count of peaks associated with
a common set of peak classes.}

\arguments{
\item{x}{An object of class \code{msSet} containing a \code{peak.class} element.}
\item{xnew}{An object of class \code{msSet}.
This object may contain a set of spectra that were not used to originally generate
the peak classes. If the user wishes to quantify the original spectra, set
\code{xnew=NULL}. Default: \code{NULL}.}
}

\value{
The same input \code{msSet} object (\code{x} if  \code{xnew=NULL}, \code{xnew} otherwise)
with an updated/new \code{peak.matrix} element. The rows and columns
of the \code{peak.matrix} are the peak class measures
and peak classes, respectively.
If \code{measure="count"}, the element \code{"peak.list"} is also
updated with a class ID for each peak.
}
\references{
Morris, J.S., Coombes, K.R., Koomen, J., Baggerly, K.A., Kobayashi, R.,
``Feature extraction and quantification for mass spectrometry
in biomedical applications using the mean spectrum,"
\emph{Bioinformatics}, 21(9):1764--75, 2005.

Tibshirani, R., Hastie, T., Narasimhan, B., Soltys, S., Shi, G.,
Koong, A., and Le, Q.T.,
``Sample classification from protein mass spectrometry,
by peak probability contrasts,"
\emph{Bioinformatics}, 20(17):3034--44, 2004.

Yasui, Y., McLerran, D., Adam, B.L., Winget, M., Thornquist, M., Feng, Z.,
``An automated peak identification/calibration procedure for
high-dimensional protein measures from mass spectrometers,"
\emph{Journal of Biomedicine and Biotechnology}, 2003(4):242--8, 2003.

Yasui, Y., Pepe, M., Thompson, M.L., Adam, B.L., Wright, Jr., G.L.,
Qu, Y., Potter, J.D., Winget, M., Thornquist, M., and Feng, Z.,
``A data-analytic strategy for protein biomarker discovery:
Profiling of high-dimensional proteomic data for cancer detection,"
\emph{Biostatistics}, 4(3):449--63, 2003.
}

\seealso{
\code{\link{msQuantify}}, \code{\link{msQuantifyIntensity}}.}
\keyword{manip}


