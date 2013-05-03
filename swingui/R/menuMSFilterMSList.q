## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSFilterMSList.q#5 $
## $DateTime: 2008/08/26 16:31:39 $

menuMSFilterMSList = function(obj, 
							  cols = NULL,
							  includeExclude = "Include", 
						 	  printObj = T, 
						 	  printSummary = T, 
						 	  saveAs = paste(substitute(obj), ".subset", sep = "")){


#	if(is.null(cols) || is.all.white(cols)) stop("must select some columns for filtering")
	
	obj = get(obj)
	if(is.null(cols) || is.all.white(cols) || cols == "<All>"){ 
		cols = names(obj)[seq(length(obj))]
	} else {
		cols = parseVectorString(cols)
	}
	
	Names = names(obj)	
	if(includeExclude == "Include"){
		obj = obj[seq(Names)[match(cols, Names)]]
	} else {
		obj = obj[-seq(Names)[match(cols, Names)]]
	}
	
	assign(saveAs, obj, where = 1)
	
	if(printObj) print(obj)
	
	if(printSummary) print(summary(obj))
	invisible()
}
