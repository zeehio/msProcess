## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSPeak.q#4 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSPeak = function(x, 								#1 
					  FUN = "simple", 					#2 
					  event = "Peak Detection",			#3
					  use.mean = F,						#4
					  simple.SNRThreshold = 2,			#5 
					  simple.span = 3,					#6
					  search.SNRThreshold = 2,			#7 
					  search.span = 3,					#8
					  search.supsmuSpan = 3,			#9					  
					  mrd.nLevel = floor(log2(numRows(x))),	#10
					  mrd.concvThreshold = 0,			#11
					  mrd.SNRThreshold = 2,				#12
					  cwt.lengthMin = 10,				#13
					  cwt.octaveMin = 1,				#14
					  cwt.nScales = 100,				#15
					  cwt.scaleMin = 4,					#16
					  cwt.snrMin = 3,					#17
					  cwt.holder = T,					#18
					  cwt.noiseFun = "quantile",		#19
					  cwt.noiseMin = 5,					#20
					  cwt.noiseSpan = 5,				#21
					  cwt.tolerance = 0,				#22
					  saveAs = paste(deparse(substitute(x)), ".pkDetect", sep = ""),  #23						 											 
					  printObj = T, 					#24 display tab
					  printHistory = T,					#25 display tab
					  plotResult = T,					#26 display tab                       
					  plot.xaxis.variable = "mass",		#27 display tab
					  plot.spectra.subset = 1,			#28 display tab
					  plot.spectra.offset = NULL, 		#29 display tab
					  imageResult = T,					#30 display tab
					  image.xaxis.variable = "mass",	#31 display tab					 
					  image.spectra.subset = NULL		#32 display tab
					  ){

	out = switch(FUN,
	
		"simple" = {
			msPeak(x = x, 
					  FUN = "simple",
					  process = "msPeakSimple", 
					  event = event,
					  use.mean = use.mean,
					  snr.thresh = simple.SNRThreshold,
					  span = simple.span)
		},
		"search" = {
			msPeak(x = x, 
					  FUN = "search", 
					  process = "msPeakSearch", 
					  event = event,
					  use.mean = use.mean,
					  snr.thresh = search.SNRThreshold,	
					  span = search.span,	
					  span.supsmu = search.supsmuSpan)
		},
		"mrd" = {
			msPeak(x = x, 
					  FUN = "mrd",
					  process = "msPeakMRD",  
					  event = event,
					  use.mean = use.mean,
					  n.level = mrd.nLevel,
					  concavity.threshold = mrd.concvThreshold,
					  snr.thresh = mrd.SNRThreshold)
		},
		"cwt" = {
			msPeak(x = x, 
					  FUN = "cwt",
					  process = "msPeakCWT",   
					  event = event,
					  use.mean = use.mean,
					  length.min = cwt.lengthMin,
					  n.octave.min = cwt.octaveMin,
					  n.scale = cwt.nScales,
					  scale.min = cwt.scaleMin,
					  snr.min = cwt.snrMin,
					  holder = cwt.holder,
					  noise.fun = cwt.noiseFun,
					  noise.min = cwt.noiseMin/100,
					  noise.span = cwt.noiseSpan,
					  tolerance = cwt.tolerance)
							 
		})				  
	
	## save		
	assign(saveAs, out, where = 1)
	
	## print if requested
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	## plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msPeak",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "peak.list",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()	
}
