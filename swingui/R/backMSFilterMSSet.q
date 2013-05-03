## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSFilterMSSet.q#4 $
## $DateTime: 2008/08/29 16:56:49 $

backMSFilterMSSet = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	
	# set startup properties
	if(initialmsg){
		all.msSet = objects(classes = "msSet")
		data = cbSetOptionList(data, "MSFilterMSSetDataSet", paste(all.msSet, collapse = ","))
		if (length(all.msSet)!=0){
			data = cbSetCurrValue(data, "MSFilterMSSetDataSet", all.msSet[1])	
			activeprop = "MSFilterMSSetDataSet"
		}
	}
	if(activeprop == "MSFilterMSSetDataSet"){
		if(exists(cbGetCurrValue(data, "MSFilterMSSetDataSet"))){
			data = cbSetCurrValue(data, 
							  	  "MSFilterMSSetSaveAs", 
							  	  paste(cbGetCurrValue(data, "MSFilterMSSetDataSet"), ".subset", sep = ""))
		
			data = cbSetOptionList(data,
								  "MSFilterMSSetColumns",
								  paste(colIds(get(cbGetCurrValue(data, "MSFilterMSSetDataSet"))[["intensity"]]), collapse = ","))
								  
#			data = cbSetCurrValue(data, "MSFilterMSSetColumns", "")

			data = cbSetCurrValue(data,
								  "MSFilterMSSetRowMZMin",
								  min(get(cbGetCurrValue(data, "MSFilterMSSetDataSet"))[["mz"]]))
		
			data = cbSetCurrValue(data,
								  "MSFilterMSSetRowMZMax",
								  max(get(cbGetCurrValue(data, "MSFilterMSSetDataSet"))[["mz"]]))
								  						  
#			data = cbSetCurrValue(data, "MSFilterMSSetRows", "")


		
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSFilterMSSetDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))				
		}
	}

	if(activeprop == "MSFilterMSSetRowMZMin"){
		rowMZMin = as.numeric(cbGetCurrValue(data, "MSFilterMSSetRowMZMin"))
		rowMZMax = as.numeric(cbGetCurrValue(data, "MSFilterMSSetRowMZMax"))
		rowMZLow = min(get(cbGetCurrValue(data, "MSFilterMSSetDataSet"))[["mz"]])
		if (rowMZMin>=rowMZMax || rowMZMin<rowMZLow) {
			guiDisplayMessageBox(
      			paste("Min m/z must be less than Max m/z and greater than or equal to", rowMZLow),
      			button = c("Ok"),
				icon = c("error"))
			data = cbSetCurrValue(data, "MSFilterMSSetRowMZMin", rowMZLow)
		}		
	}
	
	if(activeprop == "MSFilterMSSetRowMZMax"){
		rowMZMin = as.numeric(cbGetCurrValue(data, "MSFilterMSSetRowMZMin"))
		rowMZMax = as.numeric(cbGetCurrValue(data, "MSFilterMSSetRowMZMax"))
		rowMZUpp = max(get(cbGetCurrValue(data, "MSFilterMSSetDataSet"))[["mz"]])
		if (rowMZMax<=rowMZMin || rowMZMax>rowMZUpp) {
			guiDisplayMessageBox(
      			paste("Max m/z must be greater than Min m/z and less than or equal to", rowMZUpp),
      			button = c("Ok"),
				icon = c("error"))
			data = cbSetCurrValue(data, "MSFilterMSSetRowMZMax", rowMZUpp)
		}
	}

	data
}
