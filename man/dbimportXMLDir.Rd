\name{importXMLDir}
\alias{importXMLDir}
\title{ Import a directory of cyphergenXML files into SQLite }
\description{
	A wrapper function to import a directory of cyphergenXML files into a SQLite database.
}
\usage{
importXMLDir(xmldir, dbname, tablename, tof = FALSE, maxRows = 10000, 
  maxCols = NULL, tmpdir = tempdir(), splitSubdir = TRUE, verbose = 0, 
  ...)
}
\arguments{
  \item{xmldir}{ A character specifying the directory that holds xml files. }
  \item{dbname}{ A character specifying the full name of a SQLite database file,
  	including path. }
  \item{tablename}{ A \code{character} string specifying the SQL table name. } 
  \item{tof}{ A logical determining whether reading in TOF or processed data. }
  \item{maxRows}{ An integer specifying the largest number of rows 
  	that a intermediate binary file can hold. }
  \item{maxCols}{ An integer specifying the largest number of columns 
  	that the resulting SQLite table can hold. Or a \code{NULL} value, indicating 
  	that all xml files to be put into a single table. If the number of 
  	cyphergenXML files are too large, it is recommended to specify a maxCols at 
  	around 100 so that xml files can be partitioned into several SQLite tables. }
  \item{tmpdir}{ A character specifying the name of the temporary directory to store 
  	binary files. }
  \item{splitSubdir}{ A logical. If \code{TRUE}, the subdirectory will be used as
  	grouping factors: all xml under the same subdirectory will be put into the same
  	category. Otherwise, no category structure will be used. 
  	See \code{cypherGenXMList2BinBlocks} for details.}
  \item{verbose}{ A logical or non-negative integer specifying the extend of extra messages to be printed out. }
  \item{\dots}{ Additional optional arguments. }
}
\details{
  This function will import all cyphergenXML files under a certain directory (including 
  subdirectory) into a SQLite database. Each XML file contains one mass spectra.
  XML files can be grouped into subdirectories so that each group of XML files will 
  go into the same SQLite table. Otherwise all XML files are treated as the same
  group. SQLite Tables should not contain too many columns.
  Therefore a limit is given by \code{maxCols}. If the number of XML files in a group 
  is too large, we split the XML files evenly into multiple tables.
  \code{maxRows} determined the size of intermediate binary files. If it is too large,
  the intermediate file might be out of memory and could not be read in.
}
\value{
	It returns a logical indicating whether the importing was successful or not. 
}
\author{ Y Alex Chen }
\examples{
\dontrun{
	xmldir <- "E:\\SQLData\\UPCI-2007-06\\UPCI AUG WCX"
	dbname <- "e:\\mydatabase1.db"
	system.time(p<-importXMLDir(xmldir, dbname, tof=FALSE, split=FALSE, 
	maxRows=5000, tablename="nocattable", verbose=3))
	conn <- dbConnect("SQLite", "e:/mydatabase1.db", cache.size=100000)
	dbListTables(conn)
} 
}
\seealso{ \link{importBin2Sqlite}, \link{cypherGenXMList2BinBlocks} }
\keyword{ interface }
\keyword{ database }
