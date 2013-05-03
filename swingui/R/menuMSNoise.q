## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSNoise.q#9 $
## $DateTime: 2008/08/27 10:44:32 $

menuMSNoise = function(x, 							#1 
						 FUN = "spline", 			#3 
						 pre = abs,					#4	
						 event = "Local Noise Estimation",	#5
						 detach.noise = F, 			#6
						 ksmooth.kernel = "box",	#7
						 ksmooth.bandwidth = 0.5,	#8
						 loess.family = "symmetric",#9
						 loess.span = 2/3,			#10
						 loess.degree = 1,			#11
						 mean.halfSpan = 250,		#12
						 spline.df = 5,				#13
						 spline.dfOffset = 0,		#14
						 spline.spar = 0,			#15
						 spline.penalty = 1,		#16
						 spline.cv = F,				#17
						 spline.allKnots = F,		#18
						 supsmu.span = "cv",		#19
						 supsmu.bass = 0,			#20
						 supsmu.periodic = F,		#21
						 saveAs = paste(deparse(substitute(x)), ".noise", sep = ""), 	#22	 				 
						 printObj = T, 				#23
						 printHistory = T,			#24 
						 plotResult = T, 				#25 display tab
						 plot.xaxis.variable = "mass",	#26 display tab
						 plot.spectra.subset = 1,		#27 display tab
						 plot.spectra.offset = NULL, 	#28 display tab
						 imageResult = T,				#29 display tab
						 image.xaxis.variable = "mass",	#30 display tab					 
						 image.spectra.subset = NULL	#31 display tab
						 ){
						 	

	out = switch(FUN,
	
		"ksmooth" = {msNoise(x = x, 
							 FUN = "ksmooth", 
							 pre = pre, 
							 event = event,
							 detach.noise = detach.noise,
							 kernel = ksmooth.kernel,
							 bandwidth = ksmooth.bandwidth)
			},
		"loess" = {msNoise(x = x, 
							 FUN = "loess", 
							 pre = pre, 
							 event = event,
							 detach.noise = detach.noise,
							 family = loess.family,
							 span = loess.span,
							 degree = loess.degree)
			},
		"mean" = {msNoise(x = x, 
							 FUN = "mean", 
							 pre = pre, 
							 event = event,
							 detach.noise = detach.noise,
							 half.span = mean.halfSpan)
			},
		"spline" = {msNoise(x = x, 
							 FUN = "spline", 
							 pre = pre, 
							 event = event,
							 detach.noise = detach.noise,
							 df = spline.df,
							 df.offset = spline.dfOffset,
							 spar = spline.spar,
							 penalty = spline.penalty,
							 cv = spline.cv,
							 all.knots = spline.allKnots)
							 
			},
		"supsmu" ={	 msNoise(x = x, 
							 FUN = "supsmu", 
							 pre = pre, 
							 event = event,
							 detach.noise = detach.noise,
							 span = supsmu.span,
							 periodic = supsmu.periodic,
							 bass = supsmu.bass)
			})

	## Save and print	
	assign(saveAs, out, where = 1)
	if(printObj) print(out)
	if(printHistory) print(summary(out))
	
	# plot if requested
	if(plotResult){	
		plotFromGUI(out, 
					process = "msNoise",
					spectra.offset = plot.spectra.offset,
					spectra.subset = plot.spectra.subset,
					xaxis.variable = plot.xaxis.variable,
					data.name = deparse(substitute(x)))
	}
	
	## image options
	if(imageResult){
		imageFromGUI(out,
				 	 what = "noise.local",
				 	 spectra.subset = image.spectra.subset,
				 	 xaxis.variable = image.xaxis.variable,
				 	 data.name = deparse(substitute(x)))
	}

	invisible()	
}
