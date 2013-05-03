%%  WARNING: This file was automatically generated from the associated 
%%  ms_cal.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the ms_cal.mid file in the project MID directory. Once the 
%%  ms_cal.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msCalibrate function

\name{msCalibrate}
\alias{msCalibrate}
\alias{coef.msCalibrate}
\alias{formula.msCalibrate}
\alias{plot.msCalibrate}
\alias{predict.msCalibrate}
\alias{print.msCalibrate}
\title{Constructor Function for Objects of Class msCalibrate}
\concept{mass calibration}
\usage{msCalibrate(mz, tof, u=20000, FUN="lm", digits=4, predict.mz=TRUE)}
\description{Computes the parameters of the quadratic equation used by Ciphergen mass spectrometers
to convert time-of-flight to \eqn{m/z} values.}

\arguments{
\item{mz}{A vector of mass-to-charge (\code{m/z}) values (in daltons) of the calibrants used.
This input may also be a \code{list} or named vector whose objects have
(at the very least) the names \code{"u"}, \code{"t0"}, \code{"a"}, and \code{"b"},
each containing a numeric scalar coresponding to the conversion coefficients.
In this case, the \code{predict} method is called to return the predicted
\code{m/z} values for the \code{tof} input.}

\item{tof}{A numeric vector of corresponding time-of-flight measures (in nanoseconds)
of the calibrants used.}
\item{FUN}{A character string specifying the method for quadratic fitting.
Possible choices are \code{"lm"}, \code{"lmRobMM"}, \code{"ltsreg"},
\code{"lmsreg"}, \code{"l1fit"}, \code{"rreg"}. Default: \code{"lm"}.}

\item{digits}{The maximum precision to use in calculating the \code{m/z} values. Default: 4.}

\item{predict.mz}{A logical. If \code{TRUE}, predicts the mz from the input tof. Default: \code{TRUE}.}

\item{u}{A numeric value denoting the voltage (in volts) used. Default: 20000.}
}

\value{
An object of class \code{msCalibrate}.
}

\section{S3 METHODS}{
\describe{

\item{coef}{Get regression coefficients.}

\item{plot}{Plot the predicted m/z values versus supplied tof.
Optional plot parameters are as follows:
\describe{
\item{type}{The plot type. Default: \code{"b"}.}

\item{xlab}{A character string defining the abscissa label. Default: \code{"tof"}.}

\item{ylab}{A character string defining the ordinate label. Default: \code{"m/z"}.}

\item{add}{A logical value. If \code{TRUE}, the plot is added using the current
\code{par()} layout. Otherwise a new plot is produced. Default: \code{FALSE}.}

\item{...}{Additional plot arguments, i.e., \code{par()} options.}}}

\item{predict}{Predict \eqn{m/z} values from from \code{tof} input.
The following optional arguments are supported:
\describe{
\item{newtof}{New TOF values to predict over. If missing, the original
TOF values will be used. Default: \code{NULL} (missing).}

\item{digits}{The maximum precision to use in calculating the \code{m/z} values. Default: 4.}}}

\item{print}{Print the results.}

}
}

\details{
%
Assuming that the mass spectrometry data was recorded by a mass spectrometer
using time of flight (TOF) to register the number of ions
at each mass/charge (\code{m/z}) value,
mass calibration means to convert a raw TOF \code{t} to \code{m/z}.
The \code{m/z} ratio is usually a direct measure of mass because
the protein molecules are almost exclusively singly charged,
i.e., \code{z=+1}.

Typically, the calibration process involves acquiring a spectrum
from a standard sample with at least five proteins or peptides
of various molecular weights, spanning the mass	range of interest.
A quadratic equation relating \code{t} to \code{mz} is then
fit to the \code{t} values of the standard peaks in this spectrum:
\eqn{\frac{m/z}{U} = a(t-t_0)^2+b}{mz/U = a(t-t0)^2 + b},
where \code{U} is the preset voltage. The equation (with the fitted \code{a, b},
and \eqn{t_0}{t0} coefficients) is then used to convert \code{t} to \code{m/z}
in mass spectra that are collected under the
same instrument conditions such as laser intensity, approximate
date, and focusing mass or time lag.
}
\seealso{
\code{\link[stats]{lm}}, \code{\link{msCalibrate}}.
%\code{\link[splus]{lmRobMM}}, \code{\link[splus]{ltsreg}}, \code{\link[splus]{lmsreg}}, \code{\link[splus]{l1fit}}, \code{\link[splus]{rreg}}, 
}

\examples{
## set up parameters 
u  <- 20000
t0 <- 0.0038
a  <- 0.0002721697
b  <- 0.0

## simulate m/z and time-of-flight 
tof <- seq(from=20, to=60, length=7)
mz  <- u*(a*(tof - t0)^2 + b)

## perform quadratic fitting 
fit <- msCalibrate(mz=mz, tof=tof, u=u, FUN="lm")

## check the fitted parameters 
print(fit)

## do prediction: convert tof to mass 
tof2 <- seq(from=min(tof), to=max(tof), length=60)
mz.predicted <- predict(fit, tof2)

## visualization 
plot(fit, type="p", col=1, xlim=range(tof2),
    ylim=range(mz.predicted))
lines(tof2, mz.predicted, col=2)
legend(x=20, y=20000, col=1:2, pch="o ", lty=c(0,1),
    legend=c("true", "predicted"))
}
\keyword{manip}

