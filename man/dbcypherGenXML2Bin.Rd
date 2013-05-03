\name{cypherGenXML2Bin}
\alias{cypherGenXML2Bin}
\alias{cypherGenXMList2BinBlocks}
\title{ Convert Cyphergen XML files into binary files }
\description{
  Convert one or a set of Cyphergen XML files into binary files to be
  imported into SQLite database.
}
\usage{
cypherGenXML2Bin(name, pattern, path, mz, tof = FALSE, maxRows = 10000, 
	append = TRUE, verbose=TRUE, ...)
cypherGenXMList2BinBlocks(x, pattern, tof = FALSE, maxRows = 10000, 
	maxCols = NULL, path = ".", category = NULL, verbose = 1, ...)
}

\arguments{
  \item{name}{ A character string specifying the name of a xml file. }
  \item{pattern}{A character string to specify 
  	the common prefix for a series of binary files. }
  \item{path}{ A character to specify the name of input directory. }
  \item{mz}{ A numeric vector specifying \code{mz} peaks to be used. }
  \item{maxRows}{ An integer defining the maximum number of rows in a binary file. }
  \item{tof}{ A logical indicating whether TOF data or the processed data should be read. }
  \item{append}{ A logical. If \code{FALSE}, 
    then existing files will be overwritten. Otherwise, data will be appended 
    onto the existing file.}
  \item{x}{ A vector of characters specifying a set of XML files to be imported. }
  \item{maxCols}{ An integer defining the maximum number of columns in a binary file, or
  	\code{NULL}, in which case all xml files will be put into one sqliteTable.  }
  \item{category}{ A \code{data.frame} object containing categorical information. }
  \item{verbose}{A logical or a non-negative integer value to control whether or not to print 
  	out extra messages during processing. The larger the value, the more information is printed out.}
  \item{\dots}{ Additional optional arguments. }
}
\details{
  \code{cypherGenXML2Bin} converts a single Cyphergen XML file into 
  a series of binary files via function \code{writeBinBlocks}. 
  Each file contains at most \code{maxRows} mz peaks. 
  Currently we assume calibration has been made by Cyphergen mass spectrometer.
  We might support customized input of \code{a}, \code{b}, \code{t0}, 
  \code{u} in the future.
  
  \code{cypherGenXMList2BinBlocks} is a batch version of \code{cypherGenXML2Bin}.
  It converts a list of xml files into a list of binary files.
  Each binary file contains at most \code{maxRows} rows and 
  \code{maxCols} columns. If provided, \code{category} will be 
  used as group factor to partition XML files into groups, 
  and each binary file contains XML files within the same group.
  
  \code{mz} provides a calibration standard to make all mass spectra sharing the same
  set of mass-charge ratio peaks. 
  
  \code{maxRows} is used as input to \code{mxRow} in function \code{writeBinBlocks}.
  
}
\value{
	\code{cypherGenXML2Bin} returns \code{NULL}.
	
	\code{cypherGenXMList2BinBlocks} returns a list containing the following items:
	\item{sampleTable}{A data frame that contains table topology. }
	\item{categoryTable}{A data frame that contains categorical information.}
}
\author{ Y. Alex Chen <ychen@insightful.com> }
\seealso{ \link{readBinMatrix}, \link{writeBinBlocks}, \link{importBin2Sqlite} }
\keyword{ interface }
\keyword{ database }
