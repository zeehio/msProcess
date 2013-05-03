################################################
## S+Proteome example and demo functions.
##
##  msHelp
###  msExample
###  msDemo
##
################################################

################################################
##  msHelp
################################################

"msHelp" <- function(keyword="", section="msProcess")
{
  if (is.R() || (platform() != "WIN386")) {
    cat("This function is only used in the S-PLUS version of the msProcess package on Windows\n")
    return(invisible(NULL))
  }

  db <- unname(attached.where(section, nomatch=0))

  if (!db)
    stop("No documentation for section ", section)
  else if (keyword!="" && !is.element(keyword, objects(db)))
    help(quoteSyntax(keyword))
  else {
    chmfile <- file.path(searchPaths()[db], paste(section, "chm", sep="."))
    callBrowse(chmfile, keyword=keyword, option=4)
  }

  invisible(NULL)
}

################################################
##  msExample
################################################

#"msExample" <- function(index, run=FALSE, graphics=FALSE)
#{
#  if (!is.ui.app("s+gui")) {
#    stop("can only be run from the S-PLUS for Windows GUI.")
#  }
#
#  parts <- matrix(c(
#    "processing",          system.file("appexams", package="proteome"),
#    "dimension reduction", system.file("appexams", package="proteome"),
#    "classification",      system.file("appexams", package="forest")),
#    ncol=2, byrow=T, dimnames=list(NULL, c("title", "dir")))
#
#  counts <- c(12, 1, 4)
#
#  funs <- matrix(c(
#    "data ingestion",                        "msImport.ssc",
#    "mass calibration",                      "msCalibrate.ssc",
#    "data preparation",                      "msPrepare.ssc",
#    "noise reduction",                       "msDenoise.ssc",
#    "local noise estimation",                "msNoise.ssc",
#    "baseline correction",                   "msDetrend.ssc",
#    "intensity normalization",               "msNormalize.ssc",
#    "peak detection",                        "msPeak.ssc",
#    "peak alignment",                        "msAlign.ssc",
#    "peak quantification",                   "msQuantify.ssc",
#    "quality assessment",                    "msQualify.ssc",
#    "pipeline processing history",           "eventHistory.ssc",
#    "univariate variable selection",         "vsUni.ssc",
#    "fitting random forests",                "forest.ssc",
#    "variable importance in random forests", "rfVariableImportance.ssc",
#    "partial dependence in random forests",  "rfPartialDependence.ssc",
#    "sample proximity in random forests",    "rfSampleProximity.ssc"),
#     ncol=2, byrow=T, dimnames=list(NULL, c("menu", "file")))
#
#  # interactive
#  if (missing(index)) {
#    items <- paste(rep(parts[, "title"], counts), ":", funs[, "menu"])
#    msExample(menu(items, graphics=graphics, title="S+Proteome Examples:"),
#      graphics=graphics, run=run)
#    return(invisible(NULL))
#  }
#
#  # create the name of the example script file
#  if (!is.integer(index)) stop("index must be an integer.")
#  if (index==0) return(invisible(NULL))
#
#  fullfile <- paste(rep(parts[, "dir"], counts),
#    funs[, "file"], sep = "/")[index]
#
#  if (!file.exists(fullfile))
#    stop(fullfile, " doesn't exist.")
#
#  # open it or run it
#  if (run) {
#  	graphsheet(pages=TRUE)
#  	source(fullfile)
#  }
#  else {
#    guiOpen("Script",
#	  FileName = fullfile,
#	  Hide = FALSE,
#	  Show = "Normal",
#	  Top = "Auto",
#	  Left = "Auto",
#	  Width = "678",
#	  Height = "576")
#  }
#
#  invisible(NULL)
#}

################################################
##  msDemo
################################################

#"msDemo" <- function(index, run=FALSE, graphics=FALSE)
#{
#  if (!is.ui.app("s+gui")) {
#    stop("can only be run from the S-PLUS for Windows GUI.")
#  }
#
#  # interactive
#  if (missing(index)) {
#    items <- c(
#      "protein mass spectra processing (Coombes et al. 2005)",
#      "protein mass spectra processing (Morris et al. 2005)",
#      "protein mass spectra processing and quality assessment (Coombes et al. 2003)",
#      "protein mass spectra processing (Yasui et al. 2003)",
#      "protein mass spectra classification (iris)",
#      "protein mass spectra classification (prostate 2002 peaks)",
#      "protein mass spectra processing and classification (prostate 2000)",
#      "protein mass spectra processing and classification (prostate 2002)")
#    msDemo(menu(items, graphics=graphics, title="S+Proteome Demonstrations:"),
#      graphics=graphics, run=run)
#    return(invisible(NULL))
#  }
#
#  # create the name of the example script file
#  if (!is.integer(index))
#    stop("index must be an integer.")
#  if (index==0)
#    return(invisible(NULL))
#
#  # create a mapping
#  demofiles <- c(
#  	"coombes_proteomics_2005.ssc",
#  	"morris_bioinformatics_2005.ssc",
#  	"coombes_clinchem_2003.ssc",
#  	"yasui_biostatistics_2003.ssc",
#  	"ms_demo5.ssc",
#  	"ms_demo6.ssc",
#  	"ms_demo7.ssc",
#  	"ms_demo8.ssc")
#
#  fullfile <- paste(system.file("demos", package="proteome"),
#    demofiles[index], sep = "/")
#  if (!file.exists(fullfile))
#    stop(fullfile, " doesn't exist.")
#
#  # open it or run it
#  if (run) {
#    graphsheet(pages=TRUE)
#    source(fullfile)
#  }
#  else {
#    guiOpen("Script",
#	  FileName = fullfile,
#	  Hide = FALSE,
#	  Show = "Normal",
#	  Top = "Auto",
#	  Left = "Auto",
#	  Width = "678",
#	  Height = "576")
#  }
#
#  invisible(NULL)
#}
