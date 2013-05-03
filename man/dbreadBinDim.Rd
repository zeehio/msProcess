\name{readBinMatrix}
\alias{readBinMatrix}
\title{ Read a binary file containing dimension information }
\description{
  \code{readBinMatrix} reads a binary file containing dimension information, 
  and the result is wrapped into a matrix.
}
\usage{
	readBinMatrix(name, what = "double", ncol = 1e+06, ...)
}
\arguments{
  \item{name}{ A \code{character} string. }
  \item{what}{ A \code{character} string. }
  \item{ncol}{ An \code{integer} specifying an upper limit of the column number. }
  \item{\dots}{ Additional optional arguments. }
}

\details{

	\code{readBinMatrix} reads a binary file containing dimension information and wrap 
	the data into a matrix.	The first element in the input file \code{name} 
	must be an integer \code{n}, determining the length of dimensionality. 
	The following \code{n} elements must also be integers, specifying 
	the actual dimensionality, followed by data elements. Only the first dimension will
	used to determine the number of rows. This function needs user-defined ncol 
	(the number of columns). If ncol is unknown, a pre-defined upper limit (1000000) 
	will be used.
}

\value{
	 returns a logical indicating whether the importing was successful or not. 
}
\author{ Y. Alex Chen <ychen@insightful.com> }
\seealso{ \link[base]{readBin}, \link{importBin2Sqlite}}
\keyword{interface}
\keyword{file}
