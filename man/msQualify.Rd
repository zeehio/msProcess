%%  WARNING: This file was automatically generated from the associated ms_qa.mid 
%%  file. Do NOT edit this Rd file to make a change. Instead, edit the 
%%  ms_qa.mid file in the project MID directory. Once the ms_qa.mid file has 
%%  been updated, this Rd file, and all other documentation (such as 
%%  corresponding LaTeX, SGML and HTML documentation) should be regenerated 
%%  using the mid.pl Perl script. 

%% R documentation for the msQualify function

\name{msQualify}
\alias{msQualify}
\alias{predict.msQualify}
\title{Quality Assessment of Mass Spectra Data}
\concept{data quality}
\usage{msQualify(x, FUN="princomp", ...)}
\description{Creates a data quality object using a set of
spectra from a quality control (QC) sample.
The result can be used to assess the quality of
other spectra generated from the same QC sample.}

\arguments{
\item{x}{A matrix of peak intensity values with spectra as rows and
peak classes as columns. The peak intensity matrix can be
estimated via the \code{msAlign} function (with \code{measure="intensity"})
whose output contains (in part) a \code{peak.matrix} object.}
\item{...}{Additional arguments for the specified principal component analysis
\code{FUN}. See the specific function for details.}

\item{FUN}{A character string specifying the method for principal component analysis.
Possible choices are \code{"princomp"}, \code{"princompRob"}.
Default: \code{"princomp"}.}
}

\value{
An object of class \code{msQualify}.
}

\section{S3 METHODS}{
\describe{

\item{predict}{Predict the quality of a set of spectra.
This method supports the following optional arguments.
\describe{
\item{object}{An object of class \code{msQualify}.}

\item{newdata}{A matrix of peak intensities.
It must have the same number of columns as the peak intensity matrix
used to compute the \code{msQualify} object.}

\item{criterion}{A character string indicating the criterion to be use.
Possible choices are "Cattell" and "Kaiser". Default: "Cattell".}

\item{threshold}{A numeric value representing the threshold to be used.
Default: 0.9.}}}

}
}

\details{
%
The user is expected to provide a (training) peak intensity matrix
that has been derived from a set of pooled quality control samples.
The output of \code{msQualify} contains the
projection of this matrix onto its principal
components (PCs) via the \code{princomp} or \code{princompRob} function.
The user can subsequently
assess the quality of another (test) peak intensity matrix generated from the
same QC sample via the \code{predict} method, which compares the training PCs
to the test PCs.
}
\references{
Coombes KR, Fritsche HA Jr., Clarke C, Chen JN, Baggerly KA, Morris JS,
Xiao LC, Hung MC, and Kuerer HM,
``Quality control and peak finding for proteomics data collected from
nipple aspirate fluid by surface-enhanced laser desorption and ionization,"
\emph{Clinical Chemistry}, 49(10), pp. 1615--23, 2003.
}

\seealso{
\code{\link[stats]{princomp}}.
%, \code{\link[splus]{princompRob}}
}

\examples{
## create multiple reference samples with multiple 
## peaks 
set.seed(10)
nrs <- 240
nv <- 35
my.mean <- 10
my.sd <- rnorm(nv)
my.sd <- my.sd - min(my.sd) + 1
rsam <- splus2R::rmvnorm(n=nrs, d=nv, mean=rep(my.mean, nv),
    cov=diag(nv), sd=my.sd)

## run msQualify 
pca <- msQualify(rsam, FUN="princompRob", estim="auto")

## create multiple reference samples with multiple 
## peaks from the same distribution 
nts <- 72
tsam <- splus2R::rmvnorm(n=nts, d=nv, mean=rep(my.mean, nv),
    cov=diag(nv), sd=my.sd)

## predict the quality of the test samples 

quality <- predict(pca, tsam)
quality$pass
if (!is.R()) assign("quality", quality, frame=1)
## check if the distances truly follow 
## chisq(nkeep) distribution 
qqmath(~quality$dist,
    distribution=function(p, df=quality$df) qchisq(p, df),
    panel = function(x, y) {
    panel.grid()
    panel.abline(0, 1)
    panel.qqmath(x, y)
    },
    aspect=1,
    xlab=paste("Chisq(", quality$df, ") Quantile"),
    ylab="mahalanobis distance")
}
\keyword{manip}

