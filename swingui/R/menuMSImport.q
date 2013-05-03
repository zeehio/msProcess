## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSImport.q#1 $
## $DateTime: 2008/08/27 12:07:31 $

menuMSImport = function(dir = "", 				#1
							  type = "ASCII",			#2
							  pattern = "<All>",		#3
							  fileList = "",			#4
							  useFilterPattern = F,		#5
							  label = "unclassified",	#6		
							  printObj = T,				#7
							  printSummary = T, 		#8
							  saveAs = "my.msList")		#9
							  {

	if(is.all.white(dir) || !is.dir(dir)){
      	guiDisplayMessageBox(paste("directory '", dir, "'", 
      		"does not exist. Please enter another directory name for data import."),
      		button = c("Ok"),
			icon = c("error"))
		return(invisible())
	}							  	
	if(pattern == "<All>") pattern = "."
	if(useFilterPattern){							  	
		out = msImport(path = dir, type = type, pattern = pattern, label = label)
	} else {
		## Must fix this for a selection of files parseVectorString is not working if there 
		##    are spaces in the file names
		fileList = paste(dir, parseVectorString(fileList, checkSpaces = F), sep = dirSeparator())
		out = msImport(path = fileList, type = type, label = label)
	}
	if(!is.all.white(saveAs)){
		assign(saveAs, out, where = 1)
	} else {
		assign("my.msList", out, where = 1)
		warning("No object name provided, assigning output to my.msList.")			
	}

	if(printObj) print(out)
	
	if(printSummary) print(summary(out))
		
	invisible()							  	
}
							  


