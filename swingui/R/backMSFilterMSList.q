## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSFilterMSList.q#5 $
## $DateTime: 2008/08/28 16:00:53 $

backMSFilterMSList = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	
	# set startup properties
	if(initialmsg){
		all.msList = objects(classes = "msList")
		data = cbSetOptionList(data, "MSFilterMSListDataSet", paste(all.msList, collapse = ","))
		if (length(all.msList)!=0){
			data = cbSetCurrValue(data, "MSFilterMSListDataSet", all.msList[1])
			activeprop = "MSFilterMSListDataSet"
		}
	}
	if(activeprop == "MSFilterMSListDataSet"){
		if(exists(cbGetCurrValue(data, "MSFilterMSListDataSet"))){
			data = cbSetCurrValue(data, 
							  	  "MSFilterMSListSaveAs", 
							  	  paste(cbGetCurrValue(data, "MSFilterMSListDataSet"), ".subset", sep = ""))
		
			data = cbSetOptionList(data,
								  "MSFilterMSListColumns",
								  paste(names(get(cbGetCurrValue(data, "MSFilterMSListDataSet"))), collapse = ","))
								  
			data = cbSetCurrValue(data, "MSFilterMSListColumns", "<All>")
		
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSFilterMSListDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))				
		}
	}

	data
}
