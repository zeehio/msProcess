## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSAlign.q#5 $
## $DateTime: 2008/08/28 16:00:53 $

backMSAlign = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)


#> getMethodProps("MSAlign")
#	 "MSAlignDataSet"            "MSAlignFUN"          		 "MSAlignMZPrecision"        "MSAlignSNRThreshold"      
#      "MSAlignSaveAs"            "MSAlignMRDWavelet"         "MSAlignMRDLevels"    	 "MSAlignMRDReflect" 
#      "MSAlignPrintObject"       "MSAlignPrintHistory"       "MSAlignPlotResult"        "MSAlignPlotXAxisVariable"  
#      "MSAlignPlotSpectraSubset" "MSAlignPlotSpectraOffset"  "MSAlignImageResult" 		 "MSAlignImageXAxisVariable" 
#      "MSAlignImageSpectraSubset"     

	motherProps = c( "MSAlignFUN", "MSAlignMZPrecision", "MSAlignSNRThreshold", "MSAlignSaveAs" )
	mrdProps = c( "MSAlignMRDWavelet", "MSAlignMRDLevels", "MSAlignMRDReflect" )
	plotProps = c("MSAlignPlotXAxisVariable", "MSAlignPlotSpectraSubset", "MSAlignPlotSpectraOffset")
	imageProps = c("MSAlignImageXAxisVariable", "MSAlignImageSpectraSubset" )
	displayProps = c( "MSAlignPrintObject", "MSAlignPrintHistory", 
		"MSAlignPlotResult", plotProps, "MSAlignImageResult", imageProps)

	allMethodProps = c(motherProps, mrdProps, displayProps)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}

	if(initialmsg || rollbackmsg){
#		data = cbSetOptionList(data, "MSAlignDataSet", paste(objects(classes = "msSet"), collapse = ", "))
		data = cbSetOptionList(data, "MSAlignDataSet", paste(msObjects("msAlign"), collapse = ", "))
	}	

	## actions based on selecting the data set
	if(activeprop == "MSAlignDataSet"){
		if(exists(cbGetCurrValue(data, "MSAlignDataSet"))){
			data = cbSetCurrValue(data, 
							  	  "MSAlignSaveAs", 
							  	  paste(cbGetCurrValue(data, "MSAlignDataSet"), ".align", sep = ""))
			methods = msLogic(get(cbGetCurrValue(data, "MSAlignDataSet")), "msAlign")
			if(is.null(methods)){
				guiDisplayMessageBox(paste("Can not apply msAlign to", cbGetCurrValue(data, "MSAlignDataSet")),
      								 button = c("Ok"),
									 icon = c("error"))				
			} else {
				data = cbSetOptionList(data, "MSAlignFUN", paste(methods, collapse = ","))
			}
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSAlignDataSet"), 
      							 	   "does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))				
		}
		for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
		}
		for(i in imageProps){
			data = cbSetEnableFlag(data, i, F)
		}
	}


	## actions based on selecting the method
	if(activeprop == "MSAlignFUN"){
		method = cbGetCurrValue(data, "MSAlignFUN")
		if(method == "mrd"){
				for(i in mrdProps){
					data = cbSetEnableFlag(data, i, T)
				}
		} else {
				for(i in c(mrdProps)){
					data = cbSetEnableFlag(data, i, F)
				}
		}

	}
	if(activeprop == "MSAlignPlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSAlignPlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSAlignImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSAlignImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}
	
	data
}
