"writeBinBlocks" <-
function(x, pattern, maxRows=10000, path = ".",
  what=c("double", "integer", "character"), append=FALSE, verbose=TRUE)
{
# Write a vector (x) into a bunch of binary files.
# each binary file contains maxRows rows
  what <- match.arg(what)
  # remove files with pattern:
  if (!append)
  	file.remove(dir (path, pattern=paste("^", pattern, "\\.[0-9]+$", sep=""), full=TRUE))
  n <- dim(x)[1]
  if (is.null(n)) n <- length(x)
  if (n<maxRows) {
		rMat <- t(c(1, n));
  } else {
	  ntb <- ceiling (n/maxRows)
	  sx <- seq(from=1, to=n, by=maxRows)
	  ex <- seq(from=maxRows, to=n, by=maxRows)
	  if (rev(ex)[1]!=n) ex <- c(ex, n)
	  if (length(sx)!=length(ex)) {
		  cat ("bug:\n", sx, "\n", ex, "\n")
		  stop()
	  }
	  rMat <- cbind(sx, ex)
  }
  apply(rMat, 1,
    function(lmt, x, pattern, path, maxRows, what, append, verbose)
    {
      i <- ceiling(lmt[2]/maxRows)
      filename <- file.path(path, paste(pattern, i, sep="."))
    	f <- file(filename, "ab")
      dim <- dim(x)
      xr <- lmt[1]:lmt[2]
      if (is.null(dim)) {
	      x <- x[xr]
      	if (!append) writeBin(as.integer(c(1, length(x))), f)
	    } else {
	       x <- x[xr,]
	       if (!append) writeBin(as.integer(c(length(dim), dim)), f)
	    }
    	if (verbose) {
    		if (append)
    			cat("Append to file", filename, "with", length(xr), "rows\n")
    		else
    		  cat("Create file", filename, "with", length(xr), "rows\n")
    	}
	    writeBin(switch(what,
	        double=as.double(x),
	        integer=as.integer(x),
	        character=as.character(x)), f)
	    close(f)
      invisible(NULL)
    }, x, pattern, path, maxRows, what, append, verbose)
  invisible(NULL)
}

"readBinMatrix" <-
function(name, what="double", ncol=1000000, ...)
{
   f <- file(name, "rb")
   n <- as.integer(readBin(f, what="integer", n=1))
   nrow <- as.integer(readBin(f, what="integer", n=n))[1]
   v <- readBin(f, what=what, n=nrow*ncol)
   close(f)
   matrix(v, ncol=ncol)
}

"cypherGenXML2Bin" <-
function(name, pattern, path, mz, tof=FALSE, maxRows=10000,  append=TRUE, verbose=TRUE, ...)
{
  nf <- ceiling(length(mz)/maxRows)
  # read xmlfile into memory
  if (tof) {
    # for tof: calculate mz on the fly
    nodeList <-
        list(a="spectrum/processingParameters/massCalibration/massCalibrationA/text()",
             b="spectrum/processingParameters/massCalibration/massCalibrationB/text()",
             t0="spectrum/processingParameters/massCalibration/massCalibrationT0/text()",
             u="spectrum/acquisitionInfo/setting/ionSourceVoltage/text()",
             Fs="spectrum/acquisitionInfo/setting/digitizerRate/text()",
             N="spectrum/tofData/tofDataNumSamples/text()",
             time0="spectrum/tofData/tofDataTimeZero/text()",
             data="spectrum/tofData/tofDataSamples/text()")
    v <- parseXMLPathFile(name, nodeList, default=NA, delimiter=" \t\n,")
    names(v) <- names(nodeList)
    if (length(v$time0)==0) v$time0=0
    lv <- sapply(v, function(x) length(x)!=1)
    if (any(lv)) stop ("\n\t", nodeList[[lv]]," cannot be found / in the wrong format\n")
    v <- lapply(v, function(x) as.numeric(unlist(x)[1]))
    ts <- seq(from=v$time0-v$t0, to=(v$time0-v$t0)+v$N/v$Fs, by=1/v$Fs)
    v$mz <- v$u*sign(ts)*v$a*ts^2+v$b
  } else {
    # for processedData: use mz directly
    nodeList <-
      list(mzdata="spectrum/processedData/processedDataSamples/text()")
    vx <- parseXMLPathFile(name, nodeList, default=NA, delimiter=" \t\n,")
    if (length(vx[[1]])==0)
      stop ("no processed data are found in ", name, ". Please set TOF to be TRUE.")
    v <- vector(length=2, mode="list")
    v[1:2] <- split(as.numeric(vx[[1]][[1]]), 1:2)
    names(v) <- c("mz", "data")
  }

  # use mz to calibrate/approx
  if (length(v$mz)!=length(mz) || any(v$mz!=mz)){
    v <- approx(x=v$mz, y=v$data, xout=mz,
      method="linear", rule=2, f=0.5)$y
  } else {
  	v <- v$data
  }
  # split mz/data into nf parts
  # append to those files in binary format
  if (!append) {
     # also write mz into the tables:
  	v <- cbind(mz, v)
  }
  writeBinBlocks(v, pattern=pattern, path=path, maxRows=maxRows, what="double",
  	append=append, verbose=verbose)
}

"cypherGenXMList2BinBlocks" <-
function(x, pattern, tof=FALSE, maxRows=10000, maxCols=NULL,
path=".", category=NULL, verbose=1, ...)
{
  if (is.R())
    stop ("\nNot for R yet: require SPLUS specific library: SPXML\n")
  if (length(category)) {
    if (length(category)!=length(x))
      stop ("length of non-null category must be equal to that of x.")
    if (length(unique(category))==0)
      category <- NULL
  }
  if (length(maxCols) && maxCols <= 0) {
  	stop ("A valid maxCols should a positive integer.")
  }
  nodeList <-
      list(a="spectrum/processingParameters/massCalibration/massCalibrationA/text()",
           b="spectrum/processingParameters/massCalibration/massCalibrationB/text()",
           t0="spectrum/processingParameters/massCalibration/massCalibrationT0/text()",
           u="spectrum/acquisitionInfo/setting/ionSourceVoltage/text()",
           Fs="spectrum/acquisitionInfo/setting/digitizerRate/text()",
           N="spectrum/tofData/tofDataNumSamples/text()",
           time0="spectrum/tofData/tofDataTimeZero/text()")
  if (verbose>0)
  	cat("scanning list of XML files ...\n")
  mzR <- sapply(x, function(x, nodeList) {
      v <- parseXMLPathFile(x, nodeList, default=NA, delimiter=" \t\n,")
      names(v) <- names(nodeList)
      if (length(v$time0)==0) v$time0=0
      lv <- sapply(v, function(x) length(x)!=1)
      if (any(lv)) stop ("\n\t", nodeList[[lv]]," cannot be found / in the wrong format\n")
      v <- lapply(v, function(x) as.numeric(unlist(x)[1]))
      xlim <- c(v$time0-v$t0, (v$time0-v$t0)+v$N/v$Fs)
       v$u*sign(xlim)*v$a*xlim^2+v$b
     }, nodeList)

  iMin <- which.max(mzR[1,])
  iMax <- which.min(mzR[2,])

  # now map back to time domain to get the actual mz range:
  v <- parseXMLPathFile(x[iMin], nodeList, default=NA, delimiter=" \t\n,")
  names(v) <- names(nodeList)
  v <- lapply(v, function(x) as.numeric(unlist(x)[1]))
  ts <- seq(from=v$time0-v$t0, to=sqrt((mzR[2, iMax]/v$u-v$b)/v$a), by=1/v$Fs)
  mz <- v$u*sign(ts)*v$a*ts^2+v$b

  if(verbose>0)
  	cat("mz range to be used: ", range(mz), "(", length(mz), "peaks in total)\n")
  # now using xlim to read those files in batch again:
  # for each file, using predefined table blocks to
  # store temporary data:

  # a table having part information
  # get file name out as ID:
  sampleID <- sapply(strsplit(x, "\\", fixed=TRUE),
    function(x) unlist(strsplit(rev(x)[1], ".xml", fixed=TRUE)))

  if (length(category))
  {
    tableID <- tapply(x, category, function(x, maxCols)
      {
        n <- length(x)
        # balanced design:
        ifelse1(length(maxCols)==0 || n<=maxCols,
        	rep("", length=n),
          sort(rep(1:ceiling(n/maxCols), length=n)))
      }, maxCols)
    tableID <- sapply(names(tableID),
        function(x, tableID)
          paste(x, tableID[[x]], sep=""),
        tableID)
    categoryID <- sapply(tableID, unique)
    categoryTable <- data.frame(tableID=unlist(categoryID),
      categoryID=rep(names(categoryID), sapply(categoryID, length)),
      row.names=NULL)
    tableID <- unlist(tableID)
  } else {
    categoryTable <- NULL
    n <- length(x)
    tableID <- ifelse1(length(maxCols)==0 || n<=maxCols,
        	rep("", length=n),
          sort(rep(1:ceiling(n/maxCols), length=n)))
  }
  tableID <- paste(pattern, tableID, sep="")
  # now each element of tbList will be a SQLite table.
  # split each huge table into files containing nrow records:
  sampleTable <- data.frame(sampleID=sampleID,  tableID=tableID)
# mz can be used as the primary key in the database. However, it is redundant
# here (all tables share the same mz vector. We would have mz be returned as
# a single vector.

  dupfile <- duplicated(tableID)
  for (i in 1:length(x))
  {
  	if (verbose>1)
    	cat (x[i], "=>", tableID[i], "\n")
    cypherGenXML2Bin(x[i], pattern=tableID[i], path=path,
      mz=mz, maxRows=maxRows, tof=tof, append=dupfile[i], verbose=verbose>1, ...)
  }
  list(sampleTable=sampleTable, categoryTable=categoryTable)
}

"binblocks2SQLite" <-
function(conn, path, tablename, columnID,
	pattern, what="double", append=FALSE, verbose=TRUE, ...)
{
    flist <- dir(path,
    	pattern=paste("^", pattern, "\\.[0-9]+$", sep=""), full.names=TRUE)
    if (length(flist)==0) {
    	cat ("cannot find any file with pattern", pattern, "in path", path)
    	return (FALSE)
    }
    oo <- order(sapply(strsplit(flist, "\\."), function(x) as.integer(rev(x)[1])))
    flist <- flist[oo]
    newConn <- !is(conn, "DBIConnection")
    if (newConn) {
    	dbinit <- dbDriver("SQLite")
    	conn <- dbConnect(dbinit, dbname = conn)
    }
    ans <- TRUE
    if (dbExistsTable(conn, tablename) & !append) {
        ans <- dbRemoveTable(conn, tablename)
    }
    # read flist one by one and append to tablename in conn
    if (ans) {
	    ans <- all(sapply(flist,
	        function(f, conn, tablename, columnID, what, f1, verbose, ...) {
	            d <- readBinMatrix(f, ncol=length(columnID), what=what, ...)
	            if (verbose) cat (f, ": Dim=", dim(d), "\n")
	            colnames(d) <- columnID
	            dbWriteTable(conn, tablename, data.frame(d), overwrite=(f==f1),
	            	append=(f!=f1), row.names=FALSE, ...)
	        }, conn, tablename, columnID, what=what, flist[1], verbose, ...))
    }
    if(newConn)
    	dbDisconnect(newConn)
    return (ans)
}


"importBin2Sqlite" <-
function(conn, path, tablename, sampleTable,
  what="double", categoryTable=NULL, prefix="",
  verbose=1, ...)
{
    if (length(tablename)>1) {
        stop("more than one table names are provided, cannot proceed!")
    }
    if (any(is.na(match(c("sampleID", "tableID"), colnames(sampleTable)))))
        stop ("Required columns sampleID and tableID not exist in sampleTable")
    newConn <- is.character(conn)
    if (newConn) {
      dbinit <- dbDriver("SQLite")
      conn <- dbConnect(dbinit, dbname = conn)
    }
    ans <- TRUE
    sampleTableName <- paste(tablename, "sample", sep="_")
    sampleTable$sampleID <- make.db.names.default(as.character(sampleTable$sampleID))
    ans <- dbWriteTable(conn, sampleTableName, sampleTable, append=FALSE, overwrite=TRUE, row.names=FALSE)
    if (ans && length(categoryTable)) {
      	categoryTableName <- paste(tablename, "category", sep="_")
      	if (!is.data.frame(categoryTable))
      		stop ("categoryTable must be a data.frame.")
          ans <- dbWriteTable(conn, categoryTableName, categoryTable, overwrite=TRUE, append=FALSE, row.names=FALSE)
    }
    tbprefix <- as.character(sampleTable[["tableID"]])
    lf <- split(cbind(sampleTable["sampleID"], pattern=tbprefix), tbprefix)
    nmz <- -1
    for (i in 1:length(lf))
    {
      if (ans) {
        if (verbose>0)
          cat("Importing", names(lf)[i], "\n")
        ans <- binblocks2SQLite(conn, path=path,
             tablename=names(lf)[i],
             columnID=c("mz", as.character(lf[[i]][["sampleID"]])),
             pattern=as.character(lf[[i]][["pattern"]][1]),
            what=what, verbose=verbose>1)
        if (ans) {
          ntbrow <- dbGetQuery(conn,
          	paste("SELECT max(rowid) from", names(lf)[i]))[1,1]
          if (verbose>1)
          	cat ("Read", ntbrow, "rows in", names(lf)[i], "\n")
          if (i==1) nmz <- ntbrow
          else if (nmz != ntbrow) {
            if (verbose>0) {
              cat ("intensity table", names(lf)[i],
                  "has a different length (", ntbrow, ") from mz table (", nmz, ")\n")
              ans <- FALSE
              dbRemoveTable(conn, names(lf)[i])
            }
          }
        } else {
        	cat("error occurred in binblocks2SQLite\n")
        	dbRemoveTable(conn, names(lf)[i])
        	return (FALSE)
        }
        # OPTIMIZE:
        if (ans) {
          if (verbose>0)
             cat("Creating index on mz\n")
          idx <- paste(names(lf)[i], "mzIndex", sep="_")
          rc <- try(dbGetQuery(conn, paste(
              "CREATE INDEX", idx,
              "ON", names(lf)[i], "( mz )")))
          if (is.R())
          	ans <- !inherits(rc, "try-error")
          else
          	ans <- !inherits(rc, "Error")
        }
      }
    }
    if (newConn) {
      dbDisconnect(conn)
    }
    ans
}

"importXMLDir" <-
function (xmldir, dbname, tablename, tof = FALSE, maxRows = 10000, maxCols = NULL,
    tmpdir = tempdir(), splitSubdir=TRUE, verbose = 0,
    ...)
{
    f <- dir(xmldir, recursive = TRUE, full.names = TRUE)
    if (splitSubdir)
    	category <- sapply(strsplit(f, "\\", fixed = TRUE), function(x) rev(x)[2])
    else
    	category <- NULL
    dir.create(tmpdir, showWarnings=FALSE)
    # remove tempfiles from tempdir:
    tmpfiles <- dir(tmpdir,
    	pattern=paste("^", tablename, "[0-9]*\\.[0-9]+$", sep=""), full.names=TRUE)
    if (length(tmpfiles)>0)  file.remove(tmpfiles)
    if (verbose>0)
    	cat ("Convert XML files to binary blocks...\n")
    p <- cypherGenXMList2BinBlocks(f, pattern = tablename, tof = tof, maxRows = maxRows,
        maxCols = maxCols, path = tmpdir, category = category, verbose=verbose-1)
    if (verbose>0)
    	cat ("Import binary blocks into SQLite...\n")
    rc <- try(importBin2Sqlite(conn = dbname, path = tmpdir, tablename = tablename,
        sampleTable = p$sampleTable, categoryTable = p$categoryTable,
        verbose = verbose-1, ...))
    ans <- !inherits(rc, ErrorClass)
    if (ans) {
 	    tmpfiles <- dir(tmpdir,
    		pattern=paste("^", tablename, "[0-9]*\\.[0-9]+$", sep=""), full.names=TRUE)
    	if (length(tmpfiles)>0)  {
    		if (verbose>0) {
    			cat("Removing", length(tmpfiles), "temporary files\n")
    	  }
    		file.remove(tmpfiles)
    	}
    }
    else {
			print(rc)
    	cat ("An error occurred.\n")
    }
    ans
}

