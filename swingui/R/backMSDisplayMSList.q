## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSDisplayMSList.q#3 $
## $DateTime: 2008/08/28 16:00:53 $

backMSDisplayMSList = function(data){

	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	plotProps = c("MSDisplayMSListType", 
		"MSDisplayMSListMfrowRow", "MSDisplayMSListMfrowCol")

	all.msList = objects(classes = "msList")
	if(initialmsg || rollbackmsg){
		if (length(all.msList)!=0){
			data = cbSetOptionList(data, "MSDisplayMSListDataSet", paste(all.msList, collapse = ", "))
			data = cbSetCurrValue(data, "MSDisplayMSListDataSet", all.msList[1])
			activeprop = "MSDisplayMSListDataSet"
		}
	}

	if(activeprop == "MSDisplayMSListDataSet"){
		data = cbSetOptionList(data,
			"MSDisplayMSListSpectraSubset",
			paste(names(get(cbGetCurrValue(data, "MSDisplayMSListDataSet"))), collapse = ","))
		data = cbSetCurrValue(data, "MSDisplayMSListSpectraSubset", "<All>")					
	}
	
	## check values of subset
	if(activeprop == "MSDisplayMSListSpectraSubset"){
		value = cbGetCurrValue(data, "MSDisplayMSListSpectraSubset")
		if(is.all.white(value)){ 
			guiDisplayMessageBox(paste("Must provide at least one spectrum for plotting."),
      			button = c("Ok"),
				icon = c("error"))	
			data = cbSetCurrValue(data, "MSDisplayMSListSpectraSubset", "<All>")					
		}
	}
	
	## for plot option
	if(activeprop == "MSDisplayMSListDoPlot"){
		checked = as.logical(cbGetCurrValue(data, "MSDisplayMSListDoPlot"))	
		for(i in plotProps){
			data = cbSetEnableFlag(data, i, checked)
		}
	}
		
	data
}
