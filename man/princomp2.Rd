\name{princomp2}
\alias{princomp2}
\title{Principal Component Analysis for Wide Data}
\usage{
princomp2(x, ...)
}
\description{This function performs principal component analysis (PCA) 
	for wide data \code{x}, i.e. \code{dim(x)[1] < dim(x)[2]}.
	This kind of data can be handled by \code{princomp} in S-PLUS but not in R.
	The trick is to do PCA for \code{t(x)} first 
	and then convert back to the original space.
	It might be more efficient than \code{princomp} for high dimensional data.
}
\arguments{
\item{x}{a matrix.}
\item{...}{not used.}
}
\value{an object of class "princomp".}
\seealso{\code{\link[stats]{princomp}}.}
\examples{
d <- matrix(rnorm(12), nrow=3)
dimnames(d) <- list(paste("S", 1:3, sep=""), paste("V", 1:4, sep=""))
{d.pc <- princomp2(d)}
summary(d.pc)
screeplot(d.pc) 
biplot(d.pc)
predict(d.pc, d)
plot(loadings(d.pc)) 
}
\keyword{multivariate}

