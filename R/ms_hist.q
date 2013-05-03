################################################
## Class eventHistory
## Constructor function: eventHistory
## Functions:
##
##  assignEvent
##  catchEvent
##  eventHistory
##  existHistory
##  getHistory
##  isProcessRecorded
##  msAssign
##  msExists
##  msGet
##  msGlobalEnv
##  msRemove
##  throwEvent
##
## Methods:
##
##  [.eventHistory
##  print.eventHistory
##
################################################

###
# assignEvent
###

"assignEvent" <- function(record, process=NULL, histname="event.history", envir=NULL)
{
  if (is.null(envir))
   if (exists("msProcessEnv")) envir <- get("msProcessEnv") else return(invisible(NULL))

  # check for the existence of the thrown event
  if (!msExists(histname, envir))
    return(invisible(NULL))

  # obtain list of current events
  events <- msGet(histname, envir)
  nevent <- length(events)

  # obtain event data
  event   <- events[[nevent]]
  oldhist <- event$history

  # add new history
  quotedEventName <- paste("", event$name, "", sep="\"")
  newstr <- paste("newhist <- eventHistory(oldhist,",
    quotedEventName, "=record)")
  eval(parse(text=newstr))

  # update event history and re-assign
  events[[nevent]]$history  <- newhist
  events[[nevent]]$recorded <- c(event$recorded, process)
  msAssign(histname, events, envir)

  invisible(NULL)
}

###
# catchEvent
###

"catchEvent" <- function(x, histname="event.history", envir=NULL){

  if (is.null(envir))
   if (exists("msProcessEnv")) envir <- get("msProcessEnv") else return(invisible(NULL))

  # obtain list of current events
  events <- msGet(histname, envir)
  nevent <- length(events)

  # obtain history for current event and update
  # primary object with the new history
  z <- eventHistory(x, events[[nevent]]$history)

  # remove last event in list
  events[[nevent]] <- NULL

  # remove assigned events if none left to catch.
  # otherwise update assigned event list
  if (length(events))
    msAssign(histname, events, envir)
  else
    msRemove(histname, envir)

  # remove the global environment
  #envstr <- deparse(substitute(envir))
  #if (msExists(envstr)) msRemove(envstr, msGlobalEnv())

  z
}

###
# existHistory
###

"existHistory" <- function(x, event=NULL)
{
  p <- getHistory(x)
  if (any(is.null(p)))
    return(FALSE)

  if (!is.null(event) && is.character(event))
    is.element(event, names(p))
  else TRUE
}

###
# getHistory
###

"getHistory" <- function(x, event=NULL)
{
  if (is(x,"eventHistory")){
    if (is.null(event))
      return(x)
    else
      return(x[event])
  }

  # seek attribute of class "eventHistory"
  xatt <- attributes(x)
  xcls <- unlist(lapply(names(xatt), function(n,z) class(attr(z,n)), z=x))
  ih   <- match("eventHistory", xcls)

  if (is.null(ih))
    return(NULL)

  z <- xatt[[ih]]
  histname <- names(xatt)[ih]

  if (!is.null(event))
    z <- z[event]

  if (is.null(z) || anyMissing(z))
    return(NULL)
  attr(z,"histname") <- histname
  z
}

###
# isProcessRecorded
###

"isProcessRecorded" <- function(process, histname="event.history", envir=NULL)
{
  if (is.null(envir))
   if (exists("msProcessEnv")) envir <- get("msProcessEnv") else return(FALSE)

  if (!msExists(histname, envir))
    return(FALSE)

  events <- msGet(histname, envir)
  nevent <- length(events)
  is.element(process, events[[nevent]]$recorded)
}

###
# eventHistory
###

"eventHistory" <- function(x, ..., sub.label="   ", time.stamp=date(), action="append")
{
  if (is.R()){
    format.character <- function(x, ...)
    {
        args <- list(...)
        nms <- names(args)
        justify <- if (length(grep("justify",nms))) args$justify else "right"

    	if(length(x) == 0)
    		return(character(0))
    	adj.ind <- charmatch(justify, c("none", "left", "right", "center"),
    		nomatch = NA)
    	if(is.na(adj.ind))
    		stop(paste("unknown value for justify:", justify))
    	else justify <- c("none", "left", "right", "center")[adj.ind]
    	maxlen <- max(c(len <- nchar(x), as.integer(0)))

          width <- if (length(grep("width",nms))) args$width else maxlen

    	i <- len > width
    	if(any(i)) {
    		if(justify == "right")
    			x[i] <- substring(x[i], len[i] - width + 1, len[i])
    		else if(justify == "left" || justify == "none")
    			x[i] <- substring(x[i], 1, width)
    		else if(justify == "center") {
    			first <- trunc((len[i] - width)/2 + 1)
    			x[i] <- substring(x[i], first, first + width - 1)
    		}
    		else stop("Illegal value for justify")
    	}
    	if(any(i <- !i)) {
    		spaces <- paste(rep(" ", width), collapse = "")
    		if(justify == "right")
    			x[i] <- paste(substring(spaces, 1, width - len[i]),
    				x[i], sep = "")
    		else if(justify == "left")
    			x[i] <- paste(x[i], substring(spaces, 1, width - len[
    				i]), sep = "")
    		else if(justify == "center") {
    			half <- trunc((width - len[i])/2)
    			x[i] <- paste(substring(spaces, 1, half), x[i],
    				substring(spaces, 1, width - len[i] - half),
    				sep = "")
    		}
    		else if(justify == "none") {
    		}
    		else stop("Illegal value for justify")
    	}
    	x
    }
  }

  # define local functions
  "historyString" <- function(x, sub.label, time.stamp){

    if (!is.list(x))
      stop("Input must be a list")

    categories <- names(x)
    ncat <- length(x)

    subcat.text <- unlist(lapply(x,
      function(x, label, time.stamp){

        # append time.stamp
        if (!is.missing(time.stamp) && is.character(time.stamp))
          x <- c(x, list(Timestamp=time.stamp))

        x       <- x[nchar(names(x)) > 0]
        subcats <- names(x)
        width   <- max(nchar(subcats))

        subcategories <- format.character(subcats, justify="left", width=width)
        descriptions  <- unlist(lapply(x, function(x) paste(x,collapse=" ")))
        subtext       <- paste(subcategories, descriptions, sep=" : ")

        return(paste(label, subtext, collapse="\n"))

      }, label=sub.label, time.stamp=time.stamp))

    cat.text <- paste(categories, "-", sep="")
    z <- paste(cat.text, subcat.text, sep="\n")
    names(z) <- categories
    z
  }

  # check inputs
  action <- match.arg(action, c("prepend","append","merge","replace"))

  # obtain documentation arguments
  doc <- list(...)
  if (length(doc) == 0)
    stop("\nMust supply at least one documentation variable ala:\n",
      "  category = list([named list of strings])")

  # if the second argument is NULL just return
  # the original input
  if (is.null(..1)) return(x)

  if (is(..1, "eventHistory")){

    newhist <- ..1
  }
  else{

    # find args that were input like eventHistory("cat","dog").
    # These are just names without further description. Rename
    # the corresponding list names of doc accordingly
    nms           <- names(doc)
    nms.blank     <- (nchar(nms) == 0)
    single.entry  <- (unlist(lapply(doc,length)) == 1)
    is.char.entry <- unlist(lapply(doc,is.character))
    single.char.entry <- (single.entry & is.char.entry)

    if (is.null(nms))
      events.only <- which(single.char.entry)
    else
      events.only <-  which(nms.blank & single.char.entry)

if (length(events.only) > 0){
    nms[events.only] <- unlist(doc[events.only])
    doc[events.only] <- NA
    names(doc)       <- nms
}
    # create new history text
    newhist <- historyString(doc, sub.label=sub.label, time.stamp=time.stamp)
  }

  # obtain previous history
  oldhist <- getHistory(x)

  if (is.null(oldhist) && !is.R())
    attr(oldhist, "histname") <- "eventHistory"

  # form history
  # no need to do anything if action="replace"
  # no need to provide default as match.arg() has already been called
  if (action == "merge"){

    old.events <- names(oldhist)
    new.events <- names(newhist)

    common.events <- intersect(old.events, new.events)

    if (length(common.events)){
      oldhist[common.events] <- newhist[common.events]
      newhist <- c(oldhist, newhist[-which(is.element(common.events,new.events))])
    }
    else newhist <- c(oldhist, newhist)
  }
  else if (action == "prepend"){
    newhist <- c(newhist, oldhist)
  }
  else if (action == "append"){
    newhist <- c(oldhist, newhist)
  }


  oldClass(newhist) <- "eventHistory"
  attr(newhist,"index") <- seq(along=newhist)

  if (is(x,"eventHistory") || is.null(x)) return(newhist)

#  if (!is.null(oldhist) && !is.null(attr(oldhist, "histname")))
#    attr(x, attr(oldhist, "histname")) <- newhist
#  else
    attr(x, "event.history") <- newhist
  x
}

###
# msAssign
###

"msAssign" <- function(x, value, envir){

  if (is.R()) assign(x, value, envir=envir) else assign(x, value, frame=envir)
  invisible(NULL)
}

###
# msExists
###

"msExists" <- function(x, envir){

  if (is.R()) exists(x, envir=envir) else exists(x, frame=envir)
}

###
# msGet
###

"msGet" <- function(x, envir){

  if (is.R()) get(x, envir=envir) else get(x, frame=envir)
}

###
# msGlobalEnv
###

"msGlobalEnv" <- function() if (is.R()) globalenv() else 0

###
# msNewEnv
###

"msNewEnv" <- function() if (is.R()) new.env() else 1

###
# msRemove
###

"msRemove" <- function(x, envir){
  if (is.R())
    do.call("remove", list(x), quote=FALSE, envir=envir)
  else
    do.call("remove", list(x, frame=envir))
}

###
# throwEvent
###

"throwEvent" <- function(x, histname="event.history", envir=msNewEnv()){

  # create new environment for temporary storage of pipeline history data
  # assign it to the session frame to remain global
 # if (!exists("msProcessEnv")){
    msAssign("msProcessEnv", envir, msGlobalEnv())
 # }

  # form event
  event <- list(list(name=x, history=NULL, recorded=NULL))

  # check for existing thrown event list.
  # if it exists, append cuurent event to the list
  if (msExists(histname, msProcessEnv))
    event <- c(msGet(histname, msProcessEnv), event)

  # assign the event list to the specified frame
  msAssign(histname, event, msProcessEnv)
  invisible(NULL)
}

################################################
# Class Methods
################################################

###
# print.eventHistory
###

"print.eventHistory" <- function(x,
  pre=paste("[", attr(x,"index"), "]", sep=""), ...)
{
  cat(paste("\n", as.character(pre), as.vector(x),"\n"))
}

###
# [.eventHistory
###

"[.eventHistory" <- function(x, ...)
{
  if (is.character(..1)){
    i   <- pmatch(..1, names(x), nomatch=0)
    nms <- (..1)[i]
    if (!length(nms))
      return(NA)

  }
  else if (is.numeric(..1)){
    index <- as.integer(..1)
    i <- index[abs(index) > 0 & abs(index) <= length(x)]

    if (!length(i))
      return(NA)
    nms <- names(x)[i]
  }
  else
    stop("First input must of class \"integer\" or \"character\"")

  z               <- as.vector(x)[i]
  names(z)        <- nms
  attr(z,"index") <- attr(x,"index")[i]
  oldClass(z)     <- "eventHistory"

  z
}



