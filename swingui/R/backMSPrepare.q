## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSPrepare.q#8 $
## $DateTime: 2008/08/28 16:00:53 $

backMSPrepare = function(data){
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	
	# set startup properties
	all.msList = objects(classes = "msList")
	if(initialmsg || rollbackmsg){
		if (length(all.msList)!=0){
			data = cbSetOptionList(data, "MSPrepDataSet", paste(all.msList, collapse = ","))
			data = cbSetCurrValue(data, "MSPrepDataSet", all.msList[1])
			activeprop = "MSPrepDataSet"
		}
	}

	if(activeprop == "MSPrepDataSet"){
		if(exists(cbGetCurrValue(data, "MSPrepDataSet"))){
#			data = cbSetCurrValue(data, 
#							  	  "MSPrepSaveAs", 
#							  	  paste(cbGetCurrValue(data, "MSPrepDataSet"), ".prep", sep = ""))
#			data = cbSetCurrValue(data,
#								  "MSPrepMassMin",
#								  max(sapply(get(cbGetCurrValue(data, "MSPrepDataSet")), function(x) min(x[,"mz"]))))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPrepDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))				
		}
	}
	msPrepTrans = cbGetCurrValue(data, "MSPrepTransform")
	if(activeprop == "MSPrepTransform" && 
	   !is.element(msPrepTrans, c("<None>", "cubert")) && 
	   !exists(cbGetCurrValue(data, "MSPrepTransform"))){
      guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPrepTransform"), 
      							 "does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}

	data
}
