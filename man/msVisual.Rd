%% R documentation for the msVisual function

\name{msVisual}
\alias{msVisual}
\title{Determining What Can Be Visualized}
\concept{visualization}
\usage{msVisual(x, FUN="plot")}
\description{For a specific \code{msSet} object \code{x}, it determines 
which processing steps or object elements can be visualized using \code{FUN}.
This function is mainly used to build the GUI for the msProcess package.}

\arguments{
\item{x}{An object of class \code{msSet}.}
\item{FUN}{a character string specifying the visualization method.
The options are \code{"plot"} and \code{"image"}. Default: \code{"plot"}.}
}

\value{
A vector of character string denoting the processing steps or object elements
that can be visualized for the \code{msSet} object \code{x}.
Depending on \code{FUN}, its elements are the valid options for 
either the \code{process} argument of \code{plot.msSet}
or the \code{what} argument of \code{image.msSet}.
}
\seealso{
\code{\link{plot.msSet}}, \code{\link{image.msSet}}.
}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## determine which processing steps can be visuazlized using plot for qcset
msVisual(qcset, "plot")

## determine which elements can be visuazlized using image for qcset
msVisual(qcset, "image")

## apply wavelet denoise
denoised <- msDenoise(qcset, FUN="wavelet")

## determine which processing steps can be visuazlized using plot for the denoised spectra
msVisual(denoised, "plot")

## determine which elements can be visuazlized using image for the denoised spectra
msVisual(denoised, "image")
}
\keyword{manip}

