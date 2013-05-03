## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSDisplayMSSet.q#6 $
## $DateTime: 2008/08/29 16:56:49 $

backMSDisplayMSSet = function(data){

	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)
	
	#> getMethodProps("MSDisplay")
	# [1] "MSDisplayMSSetDataSet"       "MSDisplayMSSetPlotVariable"   "MSDisplayMSSetXAxisVariable"
	# [4] "MSDisplayMSSetSpectraSubset" "MSDisplayMSSetSpectraOffset"     
	# [6] "MSDisplayMSSetXlim"          "MSDisplayMSSetPCH"           "MSDisplayMSSetLTY"          
	# [9] "MSDisplayMSSetCOL"           "MSDisplayMSSetLWD"           "MSDisplayMSSetAdd"          

	plotProps = c("MSDisplayMSSetPlotVariable", "MSDisplayMSSetPlotXAxisVariable", "MSDisplayMSSetPlotSpectraSubset",
				  "MSDisplayMSSetPlotSpectraOffset", "MSDisplayMSSetPlotXlim", "MSDisplayMSSetPlotPCH", "MSDisplayMSSetPlotLTY", 
				  "MSDisplayMSSetPlotCOL", "MSDisplayMSSetPlotLWD", "MSDisplayMSSetPlotAdd")

	imageProps = c("MSDisplayMSSetImageVariable", "MSDisplayMSSetImageXAxisVariable",
					"MSDisplayMSSetImageXlim", "MSDisplayMSSetImageXAXS", "MSDisplayMSSetImageYAXS", 
					"MSDisplayMSSetImageAdd", "MSDisplayMSSetImageSpectraSubset")

	nonGraphProps = c("MSDisplayMSSetDoPlot", "MSDisplayMSSetDoImage", "MSDisplayMSSetPrintObject",
            	  	  "MSDisplayMSSetPrintSummary")

	allProps = c(nonGraphProps, plotProps, imageProps)
	
	if(initialmsg){
		for(i in allProps){
				data = cbSetEnableFlag(data, i, F)		
		}	
	}
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSDisplayMSSetDataSet", paste(objects(classes = "msSet"), collapse = ", "))
	}	

	if(activeprop == "MSDisplayMSSetDataSet"){
		if(exists(cbGetCurrValue(data, "MSDisplayMSSetDataSet"))){
			plotMethods = msVisual(get(cbGetCurrValue(data, "MSDisplayMSSetDataSet")), FUN = "plot")
			data = cbSetOptionList(data, "MSDisplayMSSetPlotVariable", paste(plotMethods, collapse = ","))
			data = cbSetCurrValue(data, "MSDisplayMSSetPlotVariable", plotMethods[1])

			imageMethods = msVisual(get(cbGetCurrValue(data, "MSDisplayMSSetDataSet")), FUN = "image")
			data = cbSetOptionList(data, "MSDisplayMSSetImageVariable", paste(imageMethods, collapse = ","))
			data = cbSetCurrValue(data, "MSDisplayMSSetImageVariable", imageMethods[1])

			data = cbSetOptionList(data,
				"MSDisplayMSSetPlotSpectraSubset",
				paste(colIds(get(cbGetCurrValue(data, "MSDisplayMSSetDataSet"))[["intensity"]]), collapse = ","))
			data = cbSetCurrValue(data, "MSDisplayMSSetPlotSpectraSubset", "<All>")

			data = cbSetOptionList(data,
				"MSDisplayMSSetImageSpectraSubset",
				paste(colIds(get(cbGetCurrValue(data, "MSDisplayMSSetDataSet"))[["intensity"]]), collapse = ","))
			data = cbSetCurrValue(data, "MSDisplayMSSetImageSpectraSubset", "<All>")
			
			for(i in nonGraphProps){
				data = cbSetEnableFlag(data, i, T)
			}
				
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSDisplayMSSetDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							 button = c("Ok"),
								 icon = c("error"))	
			data = cbSetCurrValue(data, "MSDisplayMSSetDataSet", "")			
		}

	}
	## check values of subset
	if(activeprop == "MSDisplayMSSetPlotSpectraSubset"){
		value = cbGetCurrValue(data, "MSDisplayMSSetPlotSpectraSubset")
		if(is.all.white(value)){ 
			guiDisplayMessageBox(paste("Must provide at least one spectrum for plotting."),
      							 button = c("Ok"),
								 icon = c("error"))	
			data = cbSetCurrValue(data, "MSDisplayMSSetPlotSpectraSubset", "<All>")					
		}
	}

	## check values of subset
	if(activeprop == "MSDisplayMSSetImageSpectraSubset"){
		value = cbGetCurrValue(data, "MSDisplayMSSetImageSpectraSubset")
		if(is.all.white(value)){ 
			guiDisplayMessageBox(paste("Must provide at least one spectrum for Imaging."),
      							 button = c("Ok"),
								 icon = c("error"))	
			data = cbSetCurrValue(data, "MSDisplayMSSetImageSpectraSubset", "<All>")					
		}
	}
					
	## check values of offset
	if(activeprop == "MSDisplayMSSetSpectraOffset"){
		value = cbGetCurrValue(data, "MSDisplayMSSetSpectraOffset")
		if(is.all.white(value)){ 
			guiDisplayMessageBox(paste("Must provide a numeric value for the sprectra offset."),
      							 button = c("Ok"),
								 icon = c("error"))	
			data = cbSetOptionList(data, "MSDisplayMSSetSpectraOffset", "<Auto>")					
		}
	}
	
	## check values of xlim
	if(activeprop == "MSDisplayMSSetXlim"){
		value = cbGetCurrValue(data, "MSDisplayMSSetXlim")
		if(is.all.white(value)){ 
			guiDisplayMessageBox(paste("Must provide a numeric value for the x-axis limits."),
      							 button = c("Ok"),
								 icon = c("error"))	
			data = cbSetOptionList(data, "MSDisplayMSSetXlim", "<Auto>")					
		}
	}
	
	## plot option
	if(activeprop == "MSDisplayMSSetDoPlot"){
		if(as.logical(cbGetCurrValue(data, "MSDisplayMSSetDoPlot"))){
			for(i in plotProps){
				data = cbSetEnableFlag(data, i, T)
			}
				
		} else {
			for(i in plotProps){
				data = cbSetEnableFlag(data, i, F)
			}
		}
	}

	## image option	
	if(activeprop == "MSDisplayMSSetDoImage"){
		if(as.logical(cbGetCurrValue(data, "MSDisplayMSSetDoImage"))){	
			for(i in imageProps){
				data = cbSetEnableFlag(data, i, T)
			}
				
		} else {
			for(i in imageProps){
				data = cbSetEnableFlag(data, i, F)
			}
		}
	}
		
												
data		
}
