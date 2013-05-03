## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSDetrend.q#9 $
## $DateTime: 2008/08/28 16:00:53 $

backMSDetrend = function(data){
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	#> getMethodProps("MSDetrend")
	# [1] "MSDetrendDataSet"    		"MSDetrendDataGroup"   		  "MSDetrendFUN"                         
	# [7] "MSDetrendMethodGroup"        "MSDetrendEventLabel"         "MSDetrendAttachBaseline"    
	#[10] "MSDetrendOptionsGroup"       "MSDetrendSaveAs"             "MSDetrendSaveAsGroup"       
	#[13] "MSDetrendMotherTab"          "MSDetrendApproxRule"         "MSDetrendApproxOptionsGroup"
	#[16] "MSDetrendLoessFamily"        "MSDetrendLoessSpan"          "MSDetrendLoessDegree"       
	#[19] "MSDetrendLoessOptionsGroup"  "MSDetrendMRDWavelet"         "MSDetrendMRDXForm"          
	#[22] "MSDetrendMRDLevels"          "MSDetrendMRDReflect"         "MSDetrendMRDKeepSmooth"     
	#[25] "MSDetrendMRDKeepDetails"     "MSDetrendMRDOptionsGroup"    "MSDetrendSplineDF"         
	#[28] "MSDetrendSplineSpar"         "MSDetrendSplineCV"           "MSDetrendSplineAllKnots"    
	#[31] "MSDetrendSplineDFOffset"    "MSDetrendSplinePenalty"      "MSDetrendSplineOptionsGroup"
	#[34] "MSDetrendSupsmuSpan"         "MSDetrendSupsmuBass"         "MSDetrendSupsmuPeriodic"    
	#[37] "MSDetrendSupsmuOptionsGroup" "MSDetrendOptionsTab"         "MSDetrendPrintObject"       
	#[40] "MSDetrendPrintHistory"       "MSDetrendPrintOptionsGroup"  "MSDetrendPlotResult"        
	#[43] "MSDetrendPlotXAxisVariable"  "MSDetrendPlotSpectraSubset"  "MSDetrendPlotSpectraOffset" 
	#[46] "MSDetrendPlotOptionsGroup"   "MSDetrendImageResult"        "MSDetrendImageXAxisVariable"
	#[49] "MSDetrendImageSpectraSubset" "MSDetrendImageOptionsGroup"  "MSDetrendDisplayTab"   

	motherProps = c("MSDetrendFUN", "MSDerendEventLabel",
			        "MSDetrendAttachBaseline", "MSDetrendSaveAs") 

	approxProps = "MSDetrendApproxRule" 
	
	loessProps = c("MSDetrendLoessFamily", "MSDetrendLoessSpan", "MSDetrendLoessDegree")
	
	mrdProps = c("MSDetrendMRDWavelet",  "MSDetrendMRDXForm", "MSDetrendMRDLevels", "MSDetrendMRDReflect",
	         	 "MSDetrendMRDKeepSmooth", "MSDetrendMRDKeepDetails")   

	splineProps = c("MSDetrendSplineDF", "MSDetrendSplineSpar", "MSDetrendSplineCV", "MSDetrendSplineAllKnots",
	 				"MSDetrendSplineDFOffset", "MSDetrendSplinePenalty" )
	 				
	supsmuProps = c("MSDetrendSupsmuSpan", "MSDetrendSupsmuBass", "MSDetrendSupsmuPeriodic")   

	plotProps = c("MSDetrendPlotXAxisVariable",  "MSDetrendPlotSpectraSubset",  "MSDetrendPlotSpectraOffset")

	imageProps = c("MSDetrendImageXAxisVariable", "MSDetrendImageSpectraSubset" )

	displayProps = c("MSDetrendPrintObject", "MSDetrendPrintHistory", "MSDetrendPlotResult", "MSDetrendImageResult")
					 
	allMethodProps = c(motherProps, approxProps, loessProps, mrdProps, splineProps, supsmuProps, displayProps)
	
	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	if(initialmsg){	
		for(i in allMethodProps){
					data = cbSetEnableFlag(data, i, F)
		}
	}	
	
	if(initialmsg || rollbackmsg){
		data = cbSetOptionList(data, "MSDetrendDataSet", paste(msObjects("msDetrend"), collapse = ","))	
	}

	## actions based on selecting the data set
	if(activeprop == "MSDetrendDataSet"){
		if(exists(cbGetCurrValue(data, "MSDetrendDataSet"))){
			data = cbSetCurrValue(data, 
							  "MSDetrendSaveAs", 
							  paste(cbGetCurrValue(data, "MSDetrendDataSet"), ".base", sep = ""))
			for(i in c(motherProps, displayProps)){
				data = cbSetEnableFlag(data, i, T)
			}
#			for(i in loessProps){
#				data = cbSetEnableFlag(data, i, T)
#			}
			
		} else {
      		guiDisplayMessageBox(paste(cbGetCurrValue(data, "MSDetrendDataSet"), 
      							 		"does not exist. Please enter another data set name."),
      							button = c("Ok"),
								icon = c("error"))				
		}
	}
	
	## actions based on selecting the method	
	if(activeprop == "MSDetrendFUN"){
		method = cbGetCurrValue(data, "MSDetrendFUN")
	    switch(method,
			"approx" = {
				for(i in approxProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(loessProps, mrdProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}

			}, 
			"loess" = {
				for(i in loessProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(approxProps, mrdProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"monotone" = {
				for(i in c(approxProps, loessProps, mrdProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"mrd" = {
				for(i in mrdProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(approxProps, loessProps, splineProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"spline" = {
				for(i in splineProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(approxProps, loessProps, mrdProps, supsmuProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			},
			"supsmu" = {
				for(i in supsmuProps){
					data = cbSetEnableFlag(data, i, T)
				}
				for(i in c(approxProps, loessProps, mrdProps, splineProps)){
					data = cbSetEnableFlag(data, i, F)
				}
			})	
	}			

	msDetrendFUN = cbGetCurrValue(data, "MSDetrendFUN")
	if(!is.element(msDetrendFUN, c("monotone", "mrd")) && !exists(msDetrendFUN)){
      	guiDisplayMessageBox(paste( msDetrendFUN, 
      							 	"does not exist. Please enter another function."),
      						 button = c("Ok"),
							 icon = c("error"))	
	}						


	## actions based on KeepSmooth and KeepDetails selections
	if(activeprop == "MSDetrendMRDKeepSmooth" || activeprop == "MSDetrendMRDKeepDetails"){
		if( !cbGetCurrValue(data, "MSDetrendMRDKeepSmooth") & 
			!cbGetCurrValue(data, "MSDetrendMRDKeepDetails")){
				guiDisplayMessageBox("You must keep at least one of smooth or details",
      								button = c("Ok"),
									icon = c("error"))	
			}
	}

	if(activeprop == "MSDetrendPlotResult"){
		plotChecked = as.logical(cbGetCurrValue(data, "MSDetrendPlotResult"))
		for(i in plotProps){
				data = cbSetEnableFlag(data, i, plotChecked)
		}
	}

	if(activeprop == "MSDetrendImageResult"){
		imageChecked = as.logical(cbGetCurrValue(data, "MSDetrendImageResult"))
		for(i in imageProps){
				data = cbSetEnableFlag(data, i, imageChecked)
		}
	}
		
	data
}
