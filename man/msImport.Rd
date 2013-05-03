%%  WARNING: This file was automatically generated from the associated ms_io.mid 
%%  file. Do NOT edit this Rd file to make a change. Instead, edit the 
%%  ms_io.mid file in the project MID directory. Once the ms_io.mid file has 
%%  been updated, this Rd file, and all other documentation (such as 
%%  corresponding LaTeX, SGML and HTML documentation) should be regenerated 
%%  using the mid.pl Perl script. 

%% R documentation for the msImport function

\name{msImport}
\alias{msImport}
\title{Mass Spectra Data Import}
\concept{data input}
\usage{msImport(path, label="unclassified", type="ASCII",
    pattern="", ...)}
\description{Imports data from (multiple) mass spectrum file(s) and compiles	them
into an object of \code{msList}, i.e., a list of two-column matrices.}

\arguments{
\item{path}{A single character string or a vector of character strings.
Each character string can either be a path to the directory
containing the mass spectra files (i.e., directory path)
or a path to a mass spectra file (i.e., file path).
In the case of a vector of character strings,
the paths should be either all directory paths or all file paths.}
\item{...}{Additional arguments. See \code{importData} for details.}

\item{label}{A single character string or a vector of character strings
defining the classification label(s) for the spectra to be imported.
The length of \code{label} must be equal to the number of elements
in the \code{path} argument.
The labels will be used in mass spectra classification but not in processing.
Default: \code{"unclassified"}.}

\item{pattern}{A character string denoting the pattern to use in filtering the list
of files in the \code{path} directory.
Default: \code{""} (all files in \code{path}).}

\item{type}{A single character string or a vector of character strings
specifying the input data file type(s).
The length of \code{type} must be equal to the number of
elements in the \code{path} variable,
otherwise the first entry is replicated accordingly. This
argument may be any of the supported types in the
\code{importData} function or type \code{"CiphergenXML"},
which represents a Ciphergen XML mass spectrometry data file.
Default: \code{"ASCII"}.}
}

\value{
An object of \code{msList}, which is a list of two-column matrices, one matrix for each imported file.
The first column is named \code{"mz"} and contains \eqn{m/z} values.
The second column is named \code{"intensity"} and contains the intensity values.
The list has an attribute named \code{"type"}, which is a factor and
contains the classification labels for the imported spectra.
}
\details{
%
Each data file has two columns defined by \code{m/z} and \code{intensity} values.
This function also checks if the following conditions hold:
\describe{
\item{1}{the \eqn{m/z} values are distinct;}

\item{2}{the \eqn{m/z} values are positive;}

\item{3}{the \eqn{m/z} values are the same across spectra;}

\item{4}{the lengths of each spectra are the same.}}

and issues warning messages if not.
}
\seealso{
\code{\link{msImportCiphergenXML}}.
%\code{\link[splus:import]{importData}}, 
}

\examples{
## create faux MS data files 
n.file <- 10
files <- file.path(getwd(), paste("ms", seq(n.file), ".csv", sep=""))

for (i in seq(n.file)) {
    ms <- data.frame("m/z"=(1:5), intensity=(6:10)+i)
    if (is.R())
      write.table(ms, file=files[i])
    else
      write.table(ms, file=files[i], dimnames="colnames")
}

## load the mass spectra files into a list of 
## two-column matrices 
msImport(path=files)

## do the same except use the path and pattern 
## arguments 
msImport(path=getwd(), pattern=".csv" )

## remove the files 
unlink(files)
}
\keyword{manip}

