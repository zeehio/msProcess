## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSQuantify.q#2 $
## $DateTime: 2008/08/28 16:00:53 $

backMSQuantify = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)


#> getMethodProps("MSQuantify")
#  "MSQuantifyDataSet"            "MSQuantifyFUN"               
#  "MSQuantifyXNew"               "MSQuantifySaveAs"      	    "MSQuantifyPrintObject"        
#  "MSQuantifyPrintHistory"       "MSQuantifyPlotResult"         "MSQuantifyPlotXAxisVariable"  
#  "MSQuantifyPlotSpectraSubset"  "MSQuantifyPlotSpectraOffset"  "MSQuantifyImageResult"       
#  "MSQuantifyImageXAxisVariable" "MSQuantifyImageSpectraSubset" 


	motherProps = c( "MSQuantifyFUN", "MSQuantifyXNew", "MSQuantifySaveAs" )
	plotProps = c("MSQuantifyPlotXAxisVariable", "MSQuantifyPlotSpectraSubset", 
					  "MSQuantifyPlotSpectraOffset")
	imageProps = c("MSQuantifyImageXAxisVariable", "MSQuantifyImageSpectraSubset" )
	displayProps = c( "MSQuantifyPrintObject", "MSQuantifyPrintHistory", 
		"MSQuantifyPlotResult", plotProps, "MSQuantifyImageResult", imageProps)

	allMethodProps = c(motherProps, displayProps)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}

	if(initialmsg || rollbackmsg){
#		data = cbSetOptionList(data, "MSQuantifyDataSet", paste(objects(classes = "msSet"), collapse = ", "))
		data = cbSetOptionList(data, "MSQuantifyDataSet", paste(msObjects("msQuantify"), collapse = ", "))
		data = cbSetOptionList(data, "MSQuantifyXNew", paste(objects(classes = "msSet"), collapse = ", "))
	}	

	## actions based on selecting the data set
	if(activeprop == "MSQuantifyDataSet"){
		if(exists(cbGetCurrValue(data, "MSQuantifyDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSQuantifySaveAs", 
							  paste(cbGetCurrValue(data, "MSQuantifyDataSet"), ".quant", sep = ""))
			methods = msLogic(get(cbGetCurrValue(data, "MSQuantifyDataSet")), "msQuantify")
			if(is.null(methods)){
				guiDisplayMessageBox(paste("Can not apply msQuantify to", cbGetCurrValue(data, "MSQuantifyDataSet"), "- choose another data set."),
      						button = c("Ok"),
							icon = c("error"))				
			} else {
				data = cbSetOptionList(data, "MSAlignFUN", paste(methods, collapse = ","))
			}
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSQuantifyDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
		for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
		}
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, F)
		}
	}

	if(activeprop == "MSQuantifyPlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSQuantifyPlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSQuantifyImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSQuantifyImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}

	data
}
