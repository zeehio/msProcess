## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDetrend.q#8 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSDetrend = function(x, 								#1 
						 FUN = "loess", 					#2 
						 event = "Baseline Correction",		#3
						 attach.base = T, 					#4
						 approx.rule = 2,					#5
						 loess.family = "symmetric",		#6
						 loess.span = 2/3,					#7	
						 loess.degree = 2,					#8
						 mrd.wavelet = "s8",				#9
						 mrd.xform = "modwt",				#10
						 mrd.reflect = T,					#11
						 mrd.levels = 1,					#12
						 mrd.keepDetails = T,				#13
						 mrd.keepSmooth = T,				#14
						 spline.df = 5, 					#15
						 spline.dfOffset = 0,				#16
						 spline.spar = 0, 					#17
						 spline.penalty = 1, 				#18
						 spline.cv = F,						#19
						 spline.allKnots = F, 				#20
						 supsmu.span = "cv",				#21
						 supsmu.bass = 0, 					#22
						 supsmu.periodic = F,				#23
						 saveAs = paste(deparse(substitute(x)), ".noise", sep = ""),	#24						 											 
						 printObj = T, 						#25 
						 printHistory = T,					#26 
						 plotResult = T,					#27
						 plot.xaxis.variable = "mass",		#28
						 plot.spectra.subset = 1,			#29 display tab
						 plot.spectra.offset = NULL, 		#30 display tab
						 imageResult = T,					#31 display tab
						 image.xaxis.variable = "mass",		#32 display tab					 
						 image.spectra.subset = NULL		#33 display tab
						 )
{

	out = switch(FUN,
	
		"approx" = {
			msDetrend(x = x, 
					  FUN = "approx", 
					  event = event,
				  	  attach.base = attach.base, 
					  rule = approx.rule)
		},
		"loess" = {
			msDetrend(x = x, 
					  FUN = "loess", 
					  event = event,
				  	  attach.base = attach.base, 
					  family = loess.family,
					  span = loess.span,
					  degree = loess.degree)
		},
		"monotone" = {
			msDetrend(x = x, 
					  FUN = "monotone", 
					  event = event,
				  	  attach.base = attach.base)
		},
		"mrd" = {
				if(is.all.white(mrd.levels)){
					guiDisplayMessageBox("Must enter a set of positive integers for the levels.",
      									 button = c("Ok"),
										 icon = c("error"))
				}
				if(length(grep(",", mrd.levels))){
					if(length(grep("c", mrd.levels))) mrd.levels = eval(parse(text = mrd.levels))
					else mrd.levels = eval(parse(text = paste("c(", mrd.levels, ")")))
					
				} else  { #if(length(grep(" ", levels)))
						ll = unlist(unpaste(mrd.levels, sep = " "))
						ll = ll[ll != ""]
						mrd.levels = eval(parse(text = paste("c(", paste(ll, collapse = ","), ")")))
				}			
				msDetrend(x = x, 
						  FUN = "mrd", 
						  event = event,
				  		  attach.base = attach.base, 
						  wavelet = mrd.wavelet, 
						  xform = mrd.xform, 
						  reflect = mrd.reflect, 
						  levels = mrd.levels, 
						  keep.details = mrd.keepDetails, 
						  keep.smooth = mrd.keepSmooth,)
		},
		"spline" = {
			msDetrend(x = x, 
					  FUN = "spline", 
					  event = event,
				  	  attach.base = attach.base, 
					  df = spline.df,
					  df.offset = spline.dfOffset,
					  spar = spline.spar,
					  penalty = spline.penalty,
					  cv = spline.cv,
					  all.knots = spline.allKnots)
							 
		},
		"supsmu" ={	 
			msDetrend(x = x, 
					  FUN = "supsmu", 
					  event = event,
				  	  attach.base = attach.base, 
					  span = supsmu.span,
					  periodic = supsmu.periodic,
					  bass = supsmu.bass)
		})
		
	## save				  
	assign(saveAs, out, where = 1)

	## print if requested
	if(printObj) print(out)
	if(printHistory) print(summary(out))

	## plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msDetrend",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	## image options
	
	if(imageResult){
		imageFromGUI(out,
				 	 what = "baseline",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}
	invisible()	
}
