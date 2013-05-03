## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSNormalize.q#3 $
## $DateTime: 2008/08/28 16:00:53 $

backMSNormalize = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	plotProps = c("MSNormalizePlotXAxisVariable",  "MSNormalizePlotSpectraSubset",  "MSNormalizePlotSpectraOffset")

	imageProps = c("MSNormalizeImageXAxisVariable", "MSNormalizeImageSpectraSubset" )

	displayProps = c("MSNormalizePrintObject", "MSNormalizePrintHistory", "MSNormalizePlotResult", "MSNormalizeImageResult")


	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSNormalizeDataSet", paste(msObjects("msNormalize"), collapse = ","))	
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, FALSE)
		}
	}

	## actions based on selecting the data set
	if(activeprop == "MSNormalizeDataSet"){
		if(exists(cbGetCurrValue(data, "MSNormalizeDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSNormalizeSaveAs", 
							  paste(cbGetCurrValue(data, "MSNormalizeDataSet"), ".norm", sep = ""))
			data = cbSetOptionList(data, "MSNormalizeDataType", paste(names(get(cbGetCurrValue(data, "MSNormalizeDataSet"))), collapse = ","))
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSNormalizeDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}

	if(activeprop == "MSNormalizePlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSNormalizePlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSNormalizeImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSNormalizeImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}
	
	data
}
