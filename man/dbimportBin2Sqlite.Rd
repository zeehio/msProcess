\name{importBin2Sqlite}
\alias{binblocks2SQLite}
\alias{importBin2Sqlite}
\title{ Import binary file into SQL }
\description{
  Functions to import binary files into one or multiple SQL tables.
  \code{binblocks2SQLite} reads a series of binary files into a single table.
  \code{importBin2Sqlite} is an extension to \code{binB2SQLite} in order to 
  read a series of binary files into multiple SQLite tables.
}
\usage{
binblocks2SQLite(conn, path, tablename, columnID, pattern, what = "double", 
  append = FALSE, verbose = TRUE, ...)
importBin2Sqlite(conn, path, tablename, sampleTable, what = "double", categoryTable = NULL, 
  prefix = "", verbose = 1, ...)
}

\arguments{
  \item{conn}{ Either a \code{connection} object or a \code{character}. }
  \item{path}{ A \code{character} defining }
  \item{tablename}{ A \code{character} string specifying the SQL table name. }
  \item{columnID}{ A \code{vector} of characters specifying the column names. }
  \item{pattern}{ A \code{character} string specifying patterns in file names. }
  \item{what}{ A \code{character} to specifying the data type in the binary file.
    It is possible that a binary file might contain several types of data, but we 
    don't support that currently. }
  \item{append}{ A logical value. If \code{FALSE}, then existing tables will be overwritten. 
    Otherwise, data will be appended onto the existing table.}
  \item{verbose}{A logical value or non-negative integer to control whether or not to print out extra information.}
  \item{sampleTable}{ A \code{data frame} containing three fields: sampleID, tableID and pattern. }
  \item{categoryTable}{ A \code{data frame} containing categorical information. }
  \item{prefix}{ A character string specifying optional prefix on table names. }
  \item{\dots}{ Additional optional arguments. }
}

\details{

\code{binB2SQLite} reads a series of binary files with names
sharing pattern \code{pattern} into a single SQLite table \code{name}  
on database \code{conn}.

\code{importBin2Sqlite} requires a \code{data frame}, 
(\code{sampleTable}) that contains table topology, 
i.e. what table contains what columns, and what table contains what kind of binary files.
The \code{sampleTable} will be imported to SQL database \code{conn} in additon to all binary files.
If an optional data frame \code{category} is specified, it will also be imported into 
SQLite. In that case, users can use SQL JOIN to get category related data.

}

\value{
 Both functions return a logical indicating whether the importing was successful or not. 
}
\author{ Y Alex Chen <ychen@insightful.com> }
\note{ 
	Users must make sure that all binary files share the same dimensionality.
}
\seealso{ \link[base]{readBin}, \link{readBinMatrix}, \link{writeBinBlocks} }
\keyword{interface}
\keyword{database}
