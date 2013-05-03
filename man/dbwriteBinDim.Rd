\name{writeBinBlocks}
\alias{writeBinBlocks}
\title{ Write a series of binary files containing data blocks }
\description{
  Functions to write a vector \code{x} into a series of binary files.
}
\usage{
writeBinBlocks(x, pattern, maxRows=10000, path=".", what = c("double", "integer", "character"), 
	append = FALSE, verbose = TRUE)
}
\arguments{
  \item{x}{ A \code{character} string to specify the name of the binary file. }
  \item{path}{A \code{character} string to specify the path to input binary file.}
  \item{what}{ A \code{character} string denoting the mode of the data to be read, 
  	one of: '"double", "integer", "character"'. }
  \item{append}{ A \code{logical} value. If \code{FALSE}, 
    then existing files will be overwritten. Otherwise, data will be appended 
    onto the existing file.}
  \item{pattern}{ A \code{character} string to specify 
  	the common prefix for a series of binary files.}
  \item{maxRows}{ An \code{integer} to specify the maximum number of rows allowed
   in a single binary file. See details for more information. }
  \item{verbose}{A \code{logical} value specifying whether or not to print verbose messages.}
}

\details{
	To facilitate handling large data, function \code{writeBinBlocks} is provided,
	in which a long \code{x} is split into several small parts, each containing less than 
	\code{maxRows} rows. A series of binary files, like [\code{pattern}]1,
	[\code{pattern}]2, [\code{pattern}]3, ..., will be thus generated.
}

\value{
  returns an invisible \code{NULL}. 
}
\author{ Y. Alex Chen <ychen@insightful.com> }
\seealso{ \link[base:readBin]{writeBin}, \link{readBinMatrix}, \link{importBin2Sqlite} }
\keyword{ interface }
\keyword{ file }
