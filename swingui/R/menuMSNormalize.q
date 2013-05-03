## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSNormalize.q#5 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSNormalize = function(	x, 									#1 
						 	FUN = "TIC", 						#2 
						 	event = "Intensity Normalization",	#3
						 	saveAs = paste(deparse(substitute(x)), ".norm", sep = ""),	#4						 											 
						 	printObj = T, 						#5 
						 	printHistory = T,					#6 
						 	plotResult = T,						#7                      
						 	plot.xaxis.variable = "mass",		#8
						 	plot.spectra.subset = 1,			#9 display tab
						 	plot.spectra.offset = NULL, 		#10 display tab
						 	imageResult = T,					#11 display tab
						 	image.xaxis.variable = "mass",		#12 display tab					 
						 	image.spectra.subset = NULL			#13 display tab
						 	){

	FUN = casefold(FUN)						 	
	
	out = msNormalize(	x = x, 
				  		FUN = FUN, 
				  		event = event)
	
	## save		
	assign(saveAs, out, where = 1)
	
	## print if requested
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	## plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msNormalize",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "spectra",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()	
}
