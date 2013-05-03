\name{apply}
\alias{apply}
\alias{apply.msSet}
\alias{apply.default}
\docType{methods}
\title{S3 generic apply method for msSet Class}
\concept{data structure}
\usage{
apply(X, MARGIN, FUN, \dots, type="intensity", pre=NULL, covar=NULL)
}
\description{The \code{apply} function in S-PLUS is S3 generic,
but it is not so in R. For the msProcess package, the \code{apply}
function is overloaded to be an S3 generic function, relying on
\code{UseMethod("apply")} to distribute the call. The \code{apply.default}
function is defined to be \code{base::apply}, so if the class of
the \code{X} input is not \code{"msSet"} then the standard R definition will
be used.
}

\arguments{

\item{X}{an \code{msSet} object.}

\item{MARGIN}{an integer denoting the dimension over which the
given function is applied. Use \code{MARGIN=1} for rows and
\code{MARGIN=2} for columns.}

\item{FUN}{a function to be applied to the specified array sections,
or a character string giving the name of the function.}

\item{\ldots}{any arguments to \code{FUN}. They are passed unchanged to
each call of \code{FUN} and include their names.}

\item{type}{a character string specifying the name of the array in the \code{msSet}
object list to operate over. A typical value is \code{"intensity"} or \code{"noise"},
but the name of any legitimate matrix attached to the primary
\code{msSet} object list can be used for \code{type}.
Default: \code{"intensity"} (the intensity matrix).}

\item{pre}{a function that is applied to the matrix prior to
processing the data. Typical examples would be \code{pre=t} (matrix
transpose), \code{pre=log} (log of matrix), etc. Default: \code{NULL}
(no function is applied a priori).}

\item{covar}{a named list of additional matrices to be parsed in the
same manner as the primary matrix (specified by \code{type}). The
contents of the \code{covar} matrices are also sent to the \code{FUN}
function as an input argument with the same name as that supplied in
the \code{covar} list. As an example, assuming \code{x} is an object
of class \code{msSet} that contains the matrices \code{x$intensity}
and \code{x$z}, then the call:

\code{apply(x, MARGIN=1, FUN="foo", type="intensity", covar=list(z=z))},

will ultimately result in calls
\code{foo(x$intensity[i,], z=x$z[i,])}, where \code{i=1:numRows(x$intensity)}.
Note that the matrix
\code{x$z} need only have the same number of rows in this case since
\code{MARGIN=1}, but need not necessarily contain the same number
of columns, i.e., restrictions on the dimensions of \code{covar} matrices
not specfied by \code{MARGIN} are controlled by the \code{FUN} function.
Default: \code{NULL} (no covariate matrices).}
}


\value{
a matrix containing the result of the \code{FUN} function applied to the matrix
of type \code{type} found in the original msSet object \code{X}.}

\seealso{\code{\link{msList}}, \code{\link{msSet}}.}

\examples{
if (!exists("qcset")) data("qcset", package="msProcess")

# find the means of each spectrum
# and convert the result to a single-row matrix
means <- apply(qcset, MARGIN=2, mean)
nc <- NCOL(qcset$intensity)
means <- matrix(means, ncol=nc)
print(means)

# add the means (single-row) matrix to the original
# msSet object and verify its existence
z <- msSet(qcset, means=means)
names(z)
is.matrix(z$means)

# to illustrate the use of the 'covar' argument in apply,
# create a faux function that finds tha maximum absolute
# difference between each spectrum and its mean value
foo <- function(x,meanvals) max(abs(x-meanvals))
maxdiff <- as.vector(apply(z, MARGIN=2, FUN=foo, covar=list(meanvals=means)))
print(maxdiff)

# verify the results: should get vector of nc zeros
unlist(lapply(seq(along=maxdiff), function(i,z,maxdiff)
  vecnorm(max(abs(z$intensity[,i]-z$means[,i])) - maxdiff[i]),
  z=z, maxdiff=maxdiff))
}
\keyword{classes}

