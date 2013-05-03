\name{msProcess-package}
\alias{msProcess-package}
\alias{msProcess}
\docType{package}
\title{
Protein Mass Spectra Processing
}
\description{
This package provides tools for protein mass spectra processing
including data preparation, denoising, noise estimation, baseline correction,
intensity normalization, peak detection, peak alignment, peak quantification,
and various functionalities for data ingestion/conversion, mass calibration,
data quality assessment, and protein mass spectra simulation.
It also provides auxiliary tools for data representation, data visualization,
and pipeline processing history recording and retrieval.
}
\details{
  Basic class structures:
  \tabular{ll}{
    \code{\link{msList}} \tab Spectra List \cr
    \code{\link{msSet}} \tab Spectra Set
  }  

  Top level functions:
  \tabular{ll}{
    \code{\link{msImport}} \tab Data Import \cr
    \code{\link{msPrepare}} \tab Data Conversion \cr
    \code{\link{msDenoise}} \tab Spectra Denoising \cr
    \code{\link{msDetrend}} \tab Baseline Correction \cr
    \code{\link{msNormalize}} \tab Intensity Normalization \cr
    \code{\link{msPeak}} \tab Peak Detection \cr
    \code{\link{msAlign}} \tab Peak Alignment \cr    
    \code{\link{msQuantify}} \tab Peak Quantification \cr
    \code{\link{msQualify}} \tab Quality Assessment
  }
  
  Run \code{library(help="msProcess")} or \code{packageDescription("msProcess")} 
  from S-PLUS command line to get more general information about this package.
}
%\seealso{
%  \code{\link[dimeR:dimeR-package]{dimeR}}, \code{\link[MLearn:MLearn-package]{MLearn}}
%}
\keyword{package}
