################################################
## S+Proteome GUI functions
##
##	msVisual
##	msObjects
##  msLogic
##	msLaunchExample
##  
################################################

###
# msVisual
###

"msVisual" <- function(x, FUN="plot")
{
	## sanity checks
  if (!is(x,"msSet"))
    stop("input 'x' must be of class msSet")
    
	ans <- switch (match.arg(FUN, choices=c("plot", "image")),
		"plot" = {
			processes <- c("msDenoise", "msNoise", "msDetrend", "msNormalize", "msPeak", "msAlign")
			elements <- c("noise", "noise.local", "baseline", "tic", "peak.list", "peak.class")
			c("msPrepare", processes[is.element(elements, names(x))])
		},
		"image" = {
			elements <- c("noise", "noise.local", "baseline", "peak.list", "peak.matrix")
			c("spectra", elements[is.element(elements, names(x))])
		})
		
	## list the last process first
	rev(ans)
}

###
# msObjects
###
"msObjects" <- function(process="msDenoise")
{
	# TODO: an alternative is to use the "test" argument of the "objects" function
	if (!is.R()) {
		obs <- objects(classes="msSet")
	} else {
		obs <- objects()
		obs <- obs[sapply(obs, function(x) class(eval(as.name(x))))=="msSet"]
	}
	if (length(obs)==0) return(character(0))
	mds <- lapply(obs, function(x, process) msLogic(eval(as.name(x)), process), 
		process=process)
	ind <- (sapply(mds, length)!=0)
	obs[ind]
}

###
# msLogic
###

"msLogic" <- function(x, process="msDenoise")
{
	## sanity checks
  if (!is(x,"msSet"))
    stop("input 'x' must be of class msSet")
  if (!is.character(process))
  	stop("input 'process' must be of class character")

	switch(process,
		"msDenoise"={
			supported <- c("wavelet", "smooth", "mrd")
		},
		"msNoise"={
			supported <- c("spline", "supsmu", "ksmooth", "loess", "mean")
		  if (!is.element("noise", names(x))) NULL
		  else supported
		},
		"msDetrend"={
			supported <- c("loess", "spline", "supsmu", "approx", "monotone", "mrd")
		},
		"msNormalize"={
			supported <- c("tic", "snv")
		},
		"msPeak"={
			supported <- c("simple", "search", "cwt", "mrd")
	  	if (is.null(x$mrd) || length(x$mrd$levels) != 1) supported[-4]
	  	else supported
		},
		"msAlign"={
			supported <- c("cluster", "gap", "vote", "mrd")
			if (is.null(x$peak.list)) NULL
			else supported
		},
		"msQuantify"={
			supported <- c("intensity", "count")
			if (is.null(x$peak.class)) NULL
			else if (is.null(x$peak.list)) supported[-2]
			else supported
		},
		stop("process must be either 'msDenoise', 'msNoise', 'msDetrend',
			'msNormalize', 'msPeak', 'msAlign', or 'msQuantify'")
	)
}

###
# msLaunchExample
###
"msLaunchExample" <- function(x, open=TRUE, run=TRUE, 
		type="R-ex")
{
	# check input arguments
	if (!is.character(x) || length(x) > 1.0)
		stop("x must be a single character string")
	type <- match.arg(lowerCase(type), c("r-ex","bookexams","demo"))
	
	# add extension to file name
	ext <- ".R"
	if (!length(grep(ext, x)))
		x <- paste(x, ext, sep="")
	
	exampleDir <- file.path(.find.package(package="msProcess")[1], type)
	path <- file.path(exampleDir, x)
	if (!file.exists(path))
		stop(path, " does not exist.")
	
	# TODO: use callBrowse() instead?
	if (open)
		guiOpen("Script", FileName=path, Hide=FALSE, Show="Normal", 
				Top="Auto", Left="Auto", Width="Auto", Height="Auto")
	
	if (run)
		source(path)
	
	invisible(NULL)
}
