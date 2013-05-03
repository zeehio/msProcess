## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSNoise.q#9 $
## $DateTime: 2008/08/28 16:00:53 $

backMSNoise = function(data){

	#> getMethodProps("MSNoise")
	# "MSNoiseDataSet"  "MSNoiseFUN" "MSNoisePre"  "MSNoiseMargin"             
	# "MSNoiseEventLabel" "MSNoiseDetachNoise"  "MSNoiseSaveAs"     
		
	#> getMethodProps("MSNoiseKsmooth")
	#[1] "MSNoiseKsmoothKernel"       "MSNoiseKsmoothBandwidth"
	
	#> getMethodProps("MSNoiseLoess")
	#[1] "MSNoiseLoessFamily"       "MSNoiseLoessSpan"         "MSNoiseLoessDegree"      

	#> getMethodProps("MSNoiseMean")
	#[1] "MSNoiseMeanHalfSpan"
	
	#> getMethodProps("MSNoiseSpline")
	#[1] "MSNoiseSplineDF"          "MSNoiseSplineSpar"         "MSNoiseSplineCV"          
	#[4] "MSNoiseSplineAllKnots"     "MSNoiseSplineDFOffset"    "MSNoiseSplinePenalty"     

	#> getMethodProps("MSNoiseSupsmu")
	#[1] "MSNoiseSupsmuSpan"         "MSNoiseSupsmuBass"         "MSNoiseSupsmuPeriodic"    

	#> getMethodProps("MSNoisePrint")
	#[1] "MSNoisePrintObject"     "MSNoisePrintHistory"
	#> getMethodProps("MSNoisePlot")
	#[1] "MSNoisePlotResult"        "MSNoisePlotXAxisVariable" "MSNoisePlotSpectraSubset"
	#[4] "MSNoisePlotSpectraOffset"
	#> getMethodProps("MSNoiseImage")
	#[1] "MSNoiseImageResult"        "MSNoiseImageXAxisVariable" "MSNoiseImageSpectraSubset"
	
	motherProps = c("MSNoiseFUN", "MSNoisePre", "MSNoiseMargin", "MSNoiseEventLabel", 
					"MSNoiseDetachNoise",  "MSNoiseSaveAs")
	
	ksmoothProps = c("MSNoiseKsmoothKernel", "MSNoiseKsmoothBandwidth")
	
	loessProps = c("MSNoiseLoessFamily", "MSNoiseLoessSpan", "MSNoiseLoessDegree")
	
	meanProps = "MSNoiseMeanHalfSpan"
	
	splineProps = c("MSNoiseSplineDF", "MSNoiseSplineSpar", "MSNoiseSplineCV", "MSNoiseSplineAllKnots",
				    "MSNoiseSplineDFOffset", "MSNoiseSplinePenalty")
				    
	supsmuProps = c("MSNoiseSupsmuSpan", "MSNoiseSupsmuBass", "MSNoiseSupsmuPeriodic")

	plotProps = c("MSNoisePlotXAxisVariable", "MSNoisePlotSpectraSubset", "MSNoisePlotSpectraOffset")
					 
	imageProps = c("MSNoiseImageXAxisVariable", "MSNoiseImageSpectraSubset")					 
	
	displayProps = c("MSNoisePrintObject", "MSNoisePrintHistory", "MSNoisePlotResult", 
					 "MSNoiseImageResult")
	
	allMethodProps = c(motherProps, ksmoothProps, loessProps, meanProps, splineProps, supsmuProps, displayProps)
					
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSNoiseDataSet", paste(msObjects("msNoise"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSNoiseDataSet"){
		if(exists(cbGetCurrValue(data, "MSNoiseDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSNoiseSaveAs", 
							  paste(cbGetCurrValue(data, "MSNoiseDataSet"), ".noise", sep = ""))

			for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
			}
			for(i in meanProps){
				data = cbSetEnableFlag(data, i, T)
			}		
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSNoiseDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}



	}
	
	## actions based on selecting the method
	if(activeprop == "MSNoiseFUN"){
		method = cbGetCurrValue(data, "MSNoiseFUN")
	    switch(method, 
	    	"ksmooth" ={
				for(i in ksmoothProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(loessProps, meanProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},  
			"loess" = {
				for(i in loessProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(ksmoothProps, meanProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"mean" = {
				for(i in meanProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(ksmoothProps, loessProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}

			},
			"spline" = {
				for(i in splineProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(ksmoothProps, loessProps, meanProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"supsmu" = {
				for(i in supsmuProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(ksmoothProps, loessProps, meanProps, splineProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			})	
	}	
	
	if(activeprop == "MSNoiseFUN" && !exists(cbGetCurrValue(data, "MSNoiseFUN"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSNoiseFUN"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						
		
	if(activeprop == "MSNoisePre" && !exists(cbGetCurrValue(data, "MSNoisePre"))){
      		guiDisplayMessageBox(paste( cbGetCurrValue(data, "MSNoisePre"), 
      							 		"does not exist. Please enter another function."),
      						button = c("Ok"),
							icon = c("error"))		
	}						

	if(activeprop == "MSNoisePlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSNoisePlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSNoiseImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSNoiseImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}

	data
}
