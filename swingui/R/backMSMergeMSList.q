## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSMergeMSList.q#3 $
## $DateTime: 2008/08/28 16:00:53 $

backMSMergeMSList = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	
	# set startup properties
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSMergeMSListDataSets", paste(objects(classes = "msList"), collapse = ","))
	}

	if(activeprop == "MSMergeMSListDataSets"){
		cv = cbGetCurrValue(data, "MSMergeMSListDataSets")
		if(is.all.white(cv)){
			guiDisplayMessageBox(
				paste("object(s) '", cv, "' does not exist. Please enter other data set name(s)."),
      			button = c("Ok"),
				icon = c("error"))
		} else {
			dataSets = parseVectorString(cv)
			dataExists = sapply(dataSets, exists)
			if(!all(dataExists)){
	      		guiDisplayMessageBox(paste(paste(dataSets[!dataExists], collapse = ", "), "does not exist. Please enter other data set name(s)."),
	      			button = c("Ok"),
					icon = c("error"))				
			}
		}
	}
	
	data
}
