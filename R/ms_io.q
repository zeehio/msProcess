################################################
## S+Proteome data ingestion functions
##
##  msImport
##
################################################

###
# msImport
###

"msImport" <- function(path, label="unclassified", type="ASCII",
  pattern=".", ...)
{
  if (is.R()) is.dir <- function(x) "file_test"("-d", x)

  # define local functions
  "checkExpandString" <- function(x, expansion.length)
  {
    # obtain name of primary input argument
    name <- deparse(substitute(x))

    # check inputs
    if (!is.character(x) && !isVectorAtomic(x))
      stop(name, " must be a vector of character strings.")

    N <- numRows(x)

    if (N != 1 && N != expansion.length)
      stop(name, " must be either a single character string or",
        " a vector of character strings that has length equal to",
        " the length of path.")

    if (N == 1) x <- rep(x, expansion.length)

    x
  }

  # check input arguments
  n.path <- length(path)
  label  <- checkExpandString(label, n.path)
  type   <- checkExpandString(type, n.path)

  if (!is.character(pattern))
    stop("File pattern must be a character string.")

  if (all(is.dir(path))){ # all directories

    file.list <- lapply(path,
      function(x, z) file.path(x, list.files(x, z)), z=pattern)

    l.file  <- sapply(file.list, length)
    file    <- unlist(file.list)
    ms.type <- factor(rep(label, l.file))
    type    <- rep(type, l.file)
  }
  else if(all(!is.dir(path))){ # all files
    file    <- path
    ms.type <- factor(label)
  }
  else stop("Path(s) should be either all directory paths or all file paths.")

  if (!all(file.exists(file)))
    stop("Files do not exist")

  # initialize variables
  n.file    <- length(file)
  if (n.file==0) stop("no file selected")
  ms        <- vector(mode="list", length=n.file)
  names(ms) <- if (is.R()) basename(file) else base.path.name(file)

  # import the data
  for (i in 1:n.file){

    if (pmatch(upperCase(type[i]), "CIPHERGENXML", nomatch=0)){
      ms[[i]] <- msImportCiphergenXML(file[i], ...)
    }
    else{

      # need a better solution here for R
      if (is.R()){

        ms[[i]] <- read.table(file=file[i])

      } else {

        ms[[i]] <- importData(file=file[i], type=type[i], ...)
      }

      names(ms[[i]]) <- c("mz", "intensity")
    }

    ms[[i]] <- as.matrix(ms[[i]]) # for easy plotting
  }

  # sanity checks
  for (i in 1:n.file){
    if (any(duplicated(ms[[i]][, "mz"])))
      warning("Some mz values are not distinct for spectrum:", names(ms)[i])

    if (any(ms[[i]][, "mz"] < 0))
      warning("Some mz values are not positive for spectrum:", names(ms)[i])
  }

  if (n.file > 1){

    for (i in 2:n.file){
      if (!identical(ms[[i]][, "mz"], ms[[1]][, "mz"])){
        warning("The mz values are not the same across spectra")
        break
      }
    }

    lengths <- sapply(ms, function(x) dim(x)[1])
    if (any(diff(lengths))){
      warning("The lengths of the spectra are not the same")
      print(lengths)
    }
  }

  attr(ms, "type") <- ms.type
  oldClass(ms) <- "msList"

  ms
}
