## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSAlign.q#4 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSAlign = function( x, 								#1 mother function
						FUN = "cluster", 				#2 mother function
						mz.precision = 0.003, 			#3 mother function
						snr.thresh = 10,				#4 mother function
						waveletMRD = "s8", 				#5 MRD
						reflectMRD = T, 				#6 MRD
						levelsMRD = 1, 					#7 MRD
						saveAs = paste(deparse(substitute(x)), ".denoise", sep = ""), #8
						printObj = T, 					#9 display tab
						printHistory = T, 				#10 display tab
						plotResult = T, 				#11 display tab
						plot.xaxis.variable = "mass",	#12 display tab
						plot.spectra.subset = 1,		#13 display tab
						plot.spectra.offset = NULL, 	#14 display tab
						imageResult = T,				#15 display tab
						image.xaxis.variable = "mass",	#16 display tab					 
						image.spectra.subset = NULL		#17 display tab
						){
						 	
	out = switch(FUN,
			"cluster" = msAlign(x = x, 
								FUN = "cluster", 
								mz.precision = mz.precision,
								snr.thresh = snr.thresh),
			"gap" = msAlign(x = x, 
							FUN = "gap",
							mz.precision = mz.precision,
							snr.thresh = snr.thresh),								 
			"mrd" = {
				if(is.all.white(levelsMRD)){
					guiDisplayMessageBox("Must enter a set of positive integers for the levels.",
      									 button = c("Ok"),
										 icon = c("error"))
				}
				if(length(grep(",", levelsMRD))){
					if(length(grep("c", levelsMRD))) levelsMRD = eval(parse(text = levelsMRD))
					else levels<RD = eval(parse(text = paste("c(", levelsMRD, ")")))
					
				} else  { #if(length(grep(" ", levels)))
						ll = unlist(unpaste(levelsMRD, sep = " "))
						ll = ll[ll != ""]
						levelsMRD = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
				}
										 						 
				msAlign(x = x, 
						FUN = "mrd", 
						mz.precision = mz.precision,
						snr.thresh = snr.thresh,								 				
						wavelet = waveletMRD,
						reflect = reflectMRD, 
						levels = levelsMRD)
			}, 				 
			"vote" =
				msAlign(x = x, 
						FUN = "vote",
						mz.precision = mz.precision,
						snr.thresh = snr.thresh)
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
				 	 what = "peak.list",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()
}
