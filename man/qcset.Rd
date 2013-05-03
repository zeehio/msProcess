%%  WARNING: This file was automatically generated from the associated 
%%  msb_data.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_data.mid file in the project MID directory. Once the 
%%  msb_data.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the qcset function

\name{qcset}
\alias{qcset}
\title{Mass Spectra from a Breast Cancer Quality Control Sample}
\concept{breast cancer}
\concept{quality control}
\description{A data object of class \code{msSet}, consisting  of 8 mass spectra
generated from a pooled sample of nipple aspirate fluid (NAF)
from healthy breasts and breasts with cancer.
The data set was derived from \code{qclist} by eliminating the mass region
below 950 Da/charge. Please see the references for more details.}

\references{
Coombes, K.R., Tsavachidis, S., Morris, J.S.,
Baggerly, K.A., and Kuerer, H.M.,
"Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, 5:4107--17, 2005.

Pawlik, T.M., Fritsche, H., Coombes, K.R., Xiao, L., Krishnamurthy, S., Hunt, K.K., 
Pusztai, L., Chen, J.N., Clarke, C.H., Arun, B., Hung, M.C., and Kuerer, H.M.,
"Significant differences in nipple aspirate fluid protein expression between 
healthy women and those with breast cancer demonstrated by time-of-flight mass 
spectrometry,"
\emph{Breast Cancer Research and Treatment}, 89(2):149--57, 2005.

Kuerer, H.M., Coombes, K.R., Chen, J.N., Xiao, L., Clarke, C., Fritsche, H., 
Krishnamurthy, S., Marcy, S., Hung, M.C., and Hunt, K.K.,
"Association between ductal fluid proteomic expression profiles and 
the presence of lymph node metastases in women with breast cancer,"
\emph{Surgery}, 136(5):1061--9, 2004.

Coombes, K.R., Fritsche, Jr., H.A., Clarke, C., Chen, J.N.,
Baggerly, K.A., Morris, J.S., Xiao, L.C., Hung, M.C., and Kuerer, H.M.,
"Quality control and peak finding for proteomics data collected from
nipple aspirate fluid by surface-enhanced laser desorption and ionization,"
\emph{Clinical Chemistry}, 49(10):1615--23, 2003.
}

\seealso{
\code{\link{qclist}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## plot a few spectra 
plot(qcset, subset=1:5)

## image of all spectra 
image(qcset)
}
\keyword{sysdata}

