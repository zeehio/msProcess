## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSPeak.q#4 $
## $DateTime: 2008/08/28 16:00:53 $

backMSPeak = function(data){


	#  > getMethodProps("MSPeak")
	#mother props
	#"MSPeakDataSet"  "MSPeakFUN"    "MSPeakEventLabel"     "MSPeakUseMean"           
	#"MSPeakSaveAs"  
 	 
 	#simple method props
	#"MSPeakSimpleSNRThreshold" "MSPeakSimpleSpan" 
	 
	#search method props 	       
	#"MSPeakSearchSNRThreshold" "MSPeakSearchSpan" "MSPeakSearchSupsmuSpan"  
      
    #mrd props
	#"MSPeakMRDNLevel" "MSPeakMRDConcvThreshold"  "MSPeakMRDSNRThreshold"  
	 
	#CWT props
	#"MSPeakCWTLengthMin"  "MSPeakCWTOctaveMin"  "MSPeakCWTNScale"  "MSPeakCWTScaleMin"
	#"MSPeakCWTSNRMin"     "MSPeakCWTHolder"   "MSPeakCWTNoiseFun" "MSPeakCWTNoiseMin"
	#"MSPeakCWTNoiseSpan"  "MSPeakCWTTolerance"  

	#Display props	 
	#"MSPeakPrintObject"   "MSPeakPrintHistory" "MSPeakPlotResult"  "MSPeakPlotXAxisVariable"
	#"MSPeakPlotSpectraSubset"  "MSPeakPlotSpectraOffset"  "MSPeakImageResult"   
	#"MSPeakImageXAxisVariable" "MSPeakImageSpectraSubset" 

	motherProps = c("MSPeakFUN", "MSPeakEventLabel", "MSPeakUseMean",
					"MSPeakSaveAs") 

	simpleProps = c("MSPeakSimpleSNRThreshold", "MSPeakSimpleSpan" )

	searchProps = c("MSPeakSearchSNRThreshold", "MSPeakSearchSpan", "MSPeakSearchSupsmuSpan")
		
	mrdProps = c("MSPeakMRDNLevel", "MSPeakMRDConcvThreshold", "MSPeakMRDSNRThreshold")   

	cwtProps = c("MSPeakCWTLengthMin", "MSPeakCWTOctaveMin", "MSPeakCWTNScale",
				 "MSPeakCWTScaleMin", "MSPeakCWTSNRMin", "MSPeakCWTHolder", "MSPeakCWTNoiseFun",
				 "MSPeakCWTNoiseMin", "MSPeakCWTNoiseSpan", "MSPeakCWTTolerance" )

	plotProps = c("MSPeakPlotXAxisVariable", "MSPeakPlotSpectraSubset", "MSPeakPlotSpectraOffset")	

	imageProps = c("MSPeakImageXAxisVariable", "MSPeakImageSpectraSubset" )	

	displayProps = c("MSPeakPrintObject", "MSPeakPrintHistory", 
		"MSPeakPlotResult", plotProps, "MSPeakImageResult", imageProps)	

	allMethodProps = c(motherProps, simpleProps, searchProps, mrdProps, cwtProps, displayProps)
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
		data = cbSetOptionList(data, "MSPeakDataSet", paste(msObjects("msPeak"), collapse = ","))	
	}	

	## actions based on selecting the data set
	if(activeprop == "MSPeakDataSet"){
		if(exists(cbGetCurrValue(data, "MSPeakDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSPeakSaveAs", 
							  paste(cbGetCurrValue(data, "MSPeakDataSet"), ".peak", sep = ""))
			data = cbSetOptionList(data, "MSPeakFUN", paste(msLogic(get(cbGetCurrValue(data, "MSPeakDataSet")), "msPeak"), collapse = ","))

			for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
			}
			for(i in simpleProps){
				data = cbSetEnableFlag(data, i, T)
			}		
			for(i in imageProps){
				data = cbSetEnableFlag(data, i, F)
			}	
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSPeakDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}

	}

	## actions based on selecting the method	
	if(activeprop == "MSPeakFUN"){
		method = cbGetCurrValue(data, "MSPeakFUN")
	    switch(method,
			"simple" = {
				for(i in simpleProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(searchProps, mrdProps, cwtProps)){
					data = cbSetEnableFlag(data, i, F)
				}

			}, 
			"search" = {
				for(i in searchProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(simpleProps, mrdProps, cwtProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"mrd" = {
				for(i in mrdProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(simpleProps, searchProps, cwtProps)){
					data = cbSetEnableFlag(data, i, F)
				}
				data = cbSetCurrValue(data, "MSPeakMRDNLevel", floor(log2(numRows(get(cbGetCurrValue(data, "MSPeakDataSet"))[["intensity"]]))))
			},
			"cwt" = {
				for(i in cwtProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(simpleProps, searchProps, mrdProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			})	
	}			

	if(activeprop == "MSPeakPlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSPeakPlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSPeakImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSPeakImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}

	data
}
