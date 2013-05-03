%%  WARNING: This file was automatically generated from the associated
%%  ms_xml.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_xml.mid file in the project MID directory. Once the
%%  ms_xml.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msImportCiphergenXML function

\name{msImportCiphergenXML}
\alias{msImportCiphergenXML}
\title{Import Mass Spectrometry Data from a Ciphergen XML File}
\concept{data inputxml}
\usage{msImportCiphergenXML(x, tof=FALSE, mz.calc=TRUE, digits=3)}
\description{Imports either time-of-flight (TOF) or intensity values
from a mass spectrum written in Ciphergen's XML format.
The corresponding \eqn{m/z} data are either imported
directly from the file or calculated (to a specified precision)
based on mass calibration factors extracted from the XML file.}

\arguments{
\item{x}{A character string defining the path to the Ciphergen XML file.}
\item{digits}{An integer defining the precision of the \eqn{m/z} if
\code{mz.calc} is \code{TRUE}. Default: 3.}

\item{mz.calc}{A logical value. If \code{TRUE}, the \eqn{m/z} values
are calculated based on the mass calibration parameters given in the
file. If any of the required parameters are missing in the file, then
the pre-calculated \eqn{m/z} values are returned instead (if they
do not exist, then an error is returned). The precision of the \eqn{m/z}
values is controlled through the \code{digits} argument.
Default: \code{TRUE}.}

\item{tof}{A logical value. If \code{TRUE}, the TOF data are returned. Otherwise,
the intensity values (processed TOF data) are returned. In the case that
the \code{tof} is \code{FALSE} but the intensity values do not exist in the file,
then the TOF data are returned instead.
Default: \code{FALSE}.}
}
\seealso{
\code{\link{msImport}}.}

\examples{
## create a faux Ciphergen file with basic MS data
## information
xmlFileName <- file.path(getwd(), "ciphergen_example.xml")
cat(paste(
"<spectrum>",
   "<processingParameters>",
      "<massCalibration>",
         "<massCalibrationA>264659356.3912175</massCalibrationA>",
         "<massCalibrationB>0.0005517310499463604</massCalibrationB>",
         "<massCalibrationT0>2.053450999111159e-007</massCalibrationT0>",
      "</massCalibration>",
   "</processingParameters>",
   "<tofData>",
      "<tofDataNumSamples>5</tofDataNumSamples>",
      "<tofDataTimeZero>0</tofDataTimeZero>",
      "<tofDataSamples>2172 2163 2114 2061 2107</tofDataSamples>",
   "</tofData>",
   "<acquisitionInfo>",
      "<setting>",
         "<ionSourceVoltage>20000</ionSourceVoltage>",
         "<digitizerRate>2.5e+008</digitizerRate>",
      "</setting>",
   "</acquisitionInfo>",
   "<processedData>",
      "<processedDataSamples>",
         "10.811,4.4363 10.820,4.4179 10.828,4.3178",
         "10.837,4.2096 10.845,4.3035",
      "</processedDataSamples>",
   "</processedData>",
"</spectrum>",
sep="\n"), file=xmlFileName)

## read in the data, comparing calculated and
## preset m/z values
msImportCiphergenXML(xmlFileName, mz.calc=TRUE, digits=3)$mz
msImportCiphergenXML(xmlFileName, mz.calc=FALSE)$mz

## read in TOF and then the intensity data
msImportCiphergenXML(xmlFileName, tof=TRUE, digits=3)$tof
msImportCiphergenXML(xmlFileName, tof=FALSE)$intensity
}
\keyword{manip}

