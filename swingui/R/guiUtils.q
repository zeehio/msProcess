## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/guiUtils.q#7 $
## $DateTime: 2008/08/27 09:46:32 $

######################
####  plot.msSet  ####
######################

plotFromGUI = function(x,
					   process,
					   spectra.offset, 
					   spectra.subset,
					   xaxis.variable,
					   data.name = deparse(substitute(x))){
					   	
					   	
	if(!is.null(spectra.offset)){
		if(spectra.offset == "<Auto>" || is.all.white(spectra.offset)) spectra.offset = NULL
		else spectra.offset = as.numeric(spectra.offset)
	}
	if(!is.null(spectra.subset)){
		if(spectra.subset == "<All>" || is.all.white(spectra.subset)) spectra.subset = NULL	
		if(length(grep(",", spectra.subset))){
			if(length(grep("c", spectra.subset))) spectra.subset = eval(parse(text = spectra.subset))
			else spectra.subset = eval(parse(text = paste("c(", spectra.subset, ")")))
					
		} else  { #if(length(grep(" ", levels)))
				ll = unlist(unpaste(spectra.subset, sep = " "))
				ll = ll[ll != ""]
				spectra.subset = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
		}
	}
	
	## plot the spectra
	plot.msSet(	x, 
		 		process = process, 
		 		subset = spectra.subset, 
		 		offset = spectra.offset, 
		 		xaxis = xaxis.variable,
		 		main = paste(process, "for", data.name))		
		 			   	
	invisible()				   					   	
}

#######################
####  image.msSet  ####
#######################

imageFromGUI = function(x,
						what,
						spectra.subset,
						xaxis.variable,
						data.name = deparse(substitute(x))){
							
	if(!is.null(spectra.subset)){
		if(spectra.subset == "<All>" || is.all.white(spectra.subset)) spectra.subset = NULL	
		if(length(grep(",", spectra.subset))){
			if(length(grep("c", spectra.subset))) spectra.subset = eval(parse(text = spectra.subset))
			else spectra.subset = eval(parse(text = paste("c(", spectra.subset, ")")))
					
		} else  { #if(length(grep(" ", levels)))
			ll = unlist(unpaste(spectra.subset, sep = " "))
			ll = ll[ll != ""]
			spectra.subset = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
		}
	}
		image.msSet(x, 
			  what = what,
			  subset = spectra.subset,
			  xaxis = xaxis.variable,
			  main = paste(what, "for", data.name))							
						
	invisible()							
}

#############################
####  parseVectorString  ####
#############################
##
##  this function converts a string representing a vector of values into
##    a form recognizable by S-PLUS
##  example acceptable strings are 
##    "2 3 4"
##    "2, 3, 4"
##  also S-PLUS expressions are acceptable
##    "c(2, 3, 4)"
##    2:4
##
parseVectorString = function(x, checkSpaces = T){
	if(is.all.white(x)){
		guiDisplayMessageBox("Must enter a set of valid values.",
      						button = c("Ok"),
							icon = c("error"))
	}
	
	if(length(grep(",", x))){
		if(length(grep("c\\(", x))) x = eval(parse(text = x))
		else if( length(grep("[a-zA-Z]", x)) ){
			ll = unlist(unpaste(x, sep = ","))
			ll = ll[ll != ""]	
			x = eval(parse(text = paste("c(", paste(paste("'",ll, "'", sep = ""), collapse = ","), ")")))		
		}
		else x = eval(parse(text = paste("c(", x, ")")))
					
	} else  { #if(length(grep(" ", x)))
		if( !checkSpaces ) return(x)
		if( length(grep("[a-zA-Z]", x)) ){
			ll = unlist(unpaste(x, sep = " "))
			ll = ll[ll != ""]	
			x = eval(parse(text = paste("c(", paste(paste("'",ll, "'", sep = ""), collapse = ","), ")")))		
		} else {	
	
			ll = unlist(unpaste(x, sep = " "))
			ll = ll[ll != ""]
			x = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
		}
	}
	x
}

getMethodProps = function(pattern){
	item = guiGetObjectNames("Property")
	item[grep(pattern, item)]
}
										 						 
