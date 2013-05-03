## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSQuantify.q#3 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSQuantify = function(  x, 								#1 mother function
							FUN = "intensity", 				#2 mother function
							xnew = NULL,					#3 mother function
							saveAs = paste(deparse(substitute(x)), ".denoise", sep = ""), #4
							printObj = T, 					#5 display tab
							printHistory = T, 				#6 display tab
							plotResult = T, 				#7 display tab
							plot.xaxis.variable = "mass",	#8 display tab
							plot.spectra.subset = 1,		#9 display tab
							plot.spectra.offset = NULL, 	#10 display tab
							imageResult = T,				#11 display tab
							image.xaxis.variable = "mass",	#12 display tab					 
							image.spectra.subset = NULL		#13 display tab
							){
						 	
	out = switch(FUN,
			"count" = msQuantify(x = x, 
								measure = "count",
								xnew = xnew),
			"intensity" = msQuantify(x = x, 
									measure = "intensity",
									xnew = xnew)								 
			)

				assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	# plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msAlign",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "peak.matrix",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()
	
}
