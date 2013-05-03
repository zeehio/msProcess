################################################
## S+Proteome XML functions
##
##  msImportCiphergenXML
##
################################################

###
# msImportCiphergenXML
###

"msImportCiphergenXML" <- function(x, tof=FALSE, mz.calc=TRUE, digits=3)
{
  if(!file.exists(x))
    stop("File does not exist")

  if (mz.calc){
    digits <- round(digits)

    if (digits > 17){
      warning("digits is being reduced to 17")
      digits <- 17
    }

    if (digits < 1)
      stop("digits must be positive")
  }

  # form XPaths
  msCalNames1 <- c("a", "b", "t0") # required
  msCalNames2 <- c("u", "Fs", "N") # required
  msCalNames3 <- "time0"           # optional

  msCalNames  <- c(msCalNames1, msCalNames2, msCalNames3)
  msDataNames <- c("tofData", "processedData")
  allnames    <- c(msCalNames, msDataNames)

  msCalXMLPaths <- c(
    paste("spectrum/processingParameters/massCalibration/massCalibration",
      casefold(msCalNames1, upper=TRUE), "/text()", sep=""),
    paste("spectrum/acquisitionInfo/setting/", c("ionSourceVoltage","digitizerRate"),
      "/text()", sep=""),
    paste("spectrum/tofData/", c("tofDataNumSamples","tofDataTimeZero"),
      "/text()", sep="")
  )

  msDataXMLPaths <- c("spectrum/tofData/tofDataSamples/text()",
    "spectrum/processedData/processedDataSamples/text()")

  xpaths   <- c(msCalXMLPaths, msDataXMLPaths)

  if (is.R()){

    xpaths <- paste("//", gsub("/text.*$", "", xpaths), sep="")
    doc <- xmlTreeParse(x, useInternalNodes=TRUE)
    z <- lapply(xpaths, function(x, doc){
       str <- unlist(xpathApply(doc, x, xmlValue))
       str <- unlist(strsplit(str, "[ \n\t,]"))
       bad <- which(nchar(str) == 0)
       if (length(bad)) str <- str[-bad]
       as.numeric(str)
       } , doc=doc)

  } else {

    z <- parseXMLPathFile(x, xpaths, delimiter=" \t\n,")
  }

  names(z) <- allnames

  # substitute NAs for any missing values
  missing.data    <- unlist(lapply(z,function(x){length(x) == 0}))
  z[missing.data] <- NA
  missing.names   <- names(missing.data[missing.data])

  coeffs <- as.double(unlist(z[seq(along=msCalNames)]))
  names(coeffs) <- msCalNames

  # define default for "time0" if missing
  if (anyMissing(coeffs["time0"])) coeffs["time0"] <- 0

  if (!tof && is.element("processedData", missing.names)){
    warning("No intensity data exists in file. Using TOF data instead.")
    tof <- TRUE
  }

  zz <- vector("list",length=2)

  if (!(tof && mz.calc)){
    if(is.element("processedData", missing.names))
      stop("Intensity data does not exist in file")

    zz[1:2] <- if (is.R()) split(z$processedData, 1:2) else split(as.numeric(z$processedData[[1]]), 1:2)
  }

  if (mz.calc){

    # make sure all required coefficients are present
    if (any(is.element(c(msCalNames1, msCalNames2), missing.names))){

      if (!tof){
        warning("\nThere are missing mass calibration coefficients:\n",
          "using m/z axis defined in file")
      }
      else stop("\nThere are missing mass calibration coefficients:\n",
        "and m/z axis is not explicitly defined in file")
    }

    # calculate m/z axis
    dt <- 1 / coeffs["Fs"]
    t  <- seq(from=coeffs["time0"], by=dt, length=coeffs["N"] )

    zz[[1]] <- msCalibrate(coeffs, t, digits=digits)
  }

  if (tof){

    if(is.element("tofData", missing.names))
      stop("TOF data do not exist in file")

    zz[[2]] <- if (is.R()) z$tofData else as.numeric(z$tofData[[1]])
  }

  names(zz) <- c("mz", ifelse(tof,"tof","intensity"))

  as.data.frame(zz)
}
