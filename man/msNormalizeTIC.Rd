%%  WARNING: This file was automatically generated from the associated
%%  ms_norm.mid file. Do NOT edit this Rd file to make a change. Instead,
%%  edit the ms_norm.mid file in the project MID directory. Once the
%%  ms_norm.mid file has been updated, this Rd file, and all other
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation)
%%  should be regenerated using the mid.pl Perl script.

%% R documentation for the msNormalizeTIC function

\name{msNormalizeTIC}
\alias{msNormalizeTIC}
\title{Intensity Normalization Using Total Ion Current}
\concept{intensity normalization}
\usage{msNormalizeTIC(x, process="msNormalizeTIC")}
\description{Normalizes a set of spectra using the total ion current (TIC).
Each TIC is calculated as the sum of the intensities.
The intensity of a spectrum is divided by its TIC and
then multiplied by the median TIC of the set of spectra.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{process}{A character string denoting the name of the
process to register with the (embedded) event history object of the input
after processing the input data. This process is not updated if it
already exists in the event history. Default: \code{"msNormalizeTIC"}.}
}

\value{
An object of class \code{msSet} such that the normalized spectra
have the same AUC (the median of the AUCs of spectra).
The TIC's are attached as element \code{"tic"}.
}
\references{
E.T. Fung and C. Enderwick,
ProteinChip clinical proteomics: computational challenges and solutions,
\emph{Biotechniques}, Supplement 32:S34-S41, 2002.
}

\seealso{
\code{\link{msNormalize}}, \code{\link{msNormalizeSNV}}. 
%\code{\link[splus:rowmeans]{colSums}}, 
}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## extract several spectra from the build-in
## dataset
z <- qcset[, 1:8]

## denoising
z <- msDenoise(z, FUN="wavelet", n.level=10, thresh.scale=2)

## baseline subtraction
z <- msDetrend(z, FUN="monotone", attach=TRUE)

## intensity normalization
z <- msNormalizeTIC(z)

## visualize the normalization
plot(z, process="msNormalize", subset=1:8,
    xlim=c(13000, 17000), lty=c(1,4), lwd=1:2)
}
\keyword{manip}

