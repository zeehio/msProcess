## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSFilterMSSet.q#3 $
## $DateTime: 2008/08/08 17:31:42 $

menuMSFilterMSSet = function(obj, 
							  cols = NULL,
							  colIncludeExclude = "Include", 
							  rowsMZMin = min(obj[["mz"]]),
							  rowsMZMax = max(obj[["mz"]]),
						 	  printObj = T, 
						 	  saveAs = paste(deparse(substitute(x)), ".subset", sep = "")){

	obj = get(obj)
	
	## extract columns	
	if(is.null(cols) || is.all.white(cols) || cols == "<All>"){
		cols = colIds(obj[["intensity"]])[seq(numCols(obj$intensity))]
	} else {
		cols = parseVectorString(cols)
	
		Names = colIds(obj[["intensity"]])	
		if(colIncludeExclude == "Include"){
			obj = obj[, seq(Names)[match(cols, Names)]]
		} else {
			obj = obj[, -seq(Names)[match(cols, Names)]]
		}
	}

	## extract rows	
	rows = seq(obj$mz)
	rows = rows[(obj$mz >= rowsMZMin) & (obj$mz <= rowsMZMax)]
	obj = obj[rows, ]
	
	## save object
	assign(saveAs, obj, where = 1)
	
	if(printObj) print(obj)
	invisible()
}
