%%  WARNING: This file was automatically generated from the associated 
%%  msb_util.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the msb_util.mid file in the project MID directory. Once the 
%%  msb_util.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the msPlot function

\name{msPlot}
\alias{msPlot}
\title{General Plotting Utility}
\concept{utility}
\usage{msPlot(matlines.=NULL, matpoints.=NULL, lines.=NULL, points.=NULL,
    image.=NULL, abline.=NULL, text.=NULL, offset=NULL, recenter=FALSE,
    offset.fun=function(x){3 * stdev(x, na.rm=TRUE)},
    xlab="", ylab="", yref=TRUE, main="", add=FALSE, ...)}
\description{This function facilitates a wide array of customized plots,
enabling the user to easily combine \code{matlines}, \code{matpoints},
\code{lines}, \code{points}, \code{image}, \code{abline}, and
\code{text} elements. An advantage of this technique is that
the plot boundaries defined by \code{xlim} and \code{ylim} are
automatically and correctly set using all sources of relevant
input data to avoid "plot out of bounds" warnings. A second
advantage is with the \code{matlines} and \code{matpoints} plots
in that the optional \code{offset} function can be used to
visually separate each series (column) of the supplied data
matrix in the display, creating a waterfall-like plot. There
are restrictions, however, in that the number of columns of
the \code{matlines} and \code{matpoints} matrices must be the same if supplied.
Also, currently a mix of \code{matlines}-\code{matpoints} with \code{lines}-\code{points}
is not supported.}

\arguments{

\item{...}{additional arguments for \code{plot()}.}

\item{abline.}{a list of named objects. This list must contain either
of the objects \code{h} or \code{v}, containing the vector of coordinates
at which to plot at the specifed ablines. Default: \code{NULL} (no ablines).}

\item{add}{a logical value. If \code{TRUE}, the plot is added using the current
\code{par()} layout. Otherwise a new plot is produced. Default: \code{FALSE}.}

\item{image.}{a list of named objects. At the very least, the
objects \code{x}, \code{y}, and \code{z} must exist, containing the x and y vectors
and z matrix, respectively. Default: \code{NULL} (no image).}

\item{lines.}{a list of named objects. At the very least, the
objects \code{x} and \code{y} must exist, containing the x and y matrices,
respectively. Default: \code{NULL} (no lines).}

\item{main}{a character string representing the title label. Default: \code{""} (no label).}

\item{matlines.}{a list of named objects. At the very least, the
objects \code{x} and \code{y} must exist, containing the x and y matrices,
respectively. Default: \code{NULL} (no matlines).}

\item{matpoints.}{a list of named objects. At the very least, the
objects \code{x} and \code{y} must exist, containing the x and y matrices,
respectively. Default: \code{NULL} (no matpoints).}

\item{offset}{a numeric scalar representing the vertical offset
to apply between each line in \code{matlines} and each point in \code{matpoints}.
If \code{NULL}, the offset is automatically calculated.
Default: \code{NULL} (the offset between plots is set to one standard
deviation calculated over the entire set of \code{matlines}-\code{matpoints}
y data).}

\item{offset.fun}{the function to use in calculating the \code{offset}
if not supplied. This function should have a single input argument which operates
over a vector assumed to contain a vectorized collection of all \code{matlines} and
\code{matpoints} \code{y} matrices. Default: \code{function(x){3 * stdev(x)}}.}

\item{points.}{a list of named objects. At the very least, the
objects \code{x} and \code{y} must exist, containing the x and y matrices,
respectively. Default: \code{NULL} (no points).}

\item{recenter}{a logical value specifying whether
or not to recenter the plot. Default: \code{FALSE}.}

\item{text.}{a list of named objects. At the very least, the
objects \code{x} and \code{y} must exist, containing the x and y coordinates,
respectively, used to place the text. Default: \code{NULL} (no text).}

\item{xlab}{a character string representing the x-axis label. Default: \code{""} (no label).}

\item{ylab}{a character string representing the y-axis label. Default: \code{""} (no label).}

\item{yref}{a logical value. If \code{TRUE}, a line connecting each
y-offset value on the ordinate to the starting point of the corresponding series is drawn
for \code{matlines} and \code{lines} plots. Default: \code{TRUE}.}
}

\value{
the offsets applied to each column of existing \code{matlines}-\code{matpoints}
y data. This vector is returned invisibly.
}
\seealso{
\code{\link[graphics:matplot]{matlines}}, \code{\link[graphics:matplot]{matpoints}}, \code{\link[graphics]{lines}}, \code{\link[graphics:lines]{points}}, \code{\link[graphics]{image}}, \code{\link[graphics]{abline}}, \code{\link[graphics]{text}}, \code{\link[graphics]{par}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

## define range variables 
iz  <- seq(7500)
pts <- c(1500,2500,4500)

## plot matrix style objects 
offsets <- msPlot(
    matlines=list(x=qcset$mz[iz], y=qcset$intensity[iz,1:4],
       lty = 1),
    matpoints=list(x=qcset$mz[pts], y=qcset$intensity[pts,1:4],
       pch=1,cex=2),
    text=list(x=c(3,7)*1000, y=c(15,18)*1000,
       labels=c("cow","moon"), cex=2, adj=1, col=1:2),
    abline=list(v=qcset$mz[pts], h=10000),
    xlab="X Label", ylab="Y Label", main="Title",
    yref=TRUE)

## plot non-matrix style objects 
x <- y <- seq(-4*pi, 4*pi, len=27)
r <- sqrt(outer(x^2, y^2, "+"))
z <- cos(r^2)*exp(-r/6)

msPlot(
    lines=list(x=-60:-55, y=40:45, lwd=5, col=4),
    points=list(x=rnorm(30)-65, y=sin(1:30)+35, col=3, pch=2, cex=2),
    text=list(x=-65,y=39.5,"Sample text", cex=4, col=2),
    image=list(x=x/5-70, y=y/5+45, z=z),
    yref=FALSE)

## multiple matlines in a single call 
x <- matrix(seq(0,10,by=0.1))
y <- sin(x \%*\% t(1:4))
msPlot(matlines=list(
    list(x=x, y=y, lty=1, lwd=1),
    list(x=x, y=y + 0.2, lty=1, lwd=2),
    list(x=x, y=y - 0.2, lty=1, lwd=4)),
    yref=TRUE)
}
\keyword{hplot}

