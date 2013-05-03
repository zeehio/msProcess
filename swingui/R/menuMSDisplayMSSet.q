## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDisplayMSSet.q#7 $
## $DateTime: 2008/08/29 16:56:49 $

menuMSDisplayMSSet = function( 	x,							#1
								plotVariable = "msPrepare",	#2
								plotXaxis = "mass",			#3
								plotSubset = "<All>", 		#4
								plotOffset = NULL,			#5
								imageVariable = "spectra",	#6
								imageXaxis = "mass",		#7
								imageSubset = "<All>",		#8
								doPlot = T, 				#9
								doImage = T,				#10
								printObj = T, 				#11
								printHist = T, 				#12
								plotXlim = NULL,			#13
								pch = 1,					#14
								lty = 1:2,					#15
								col = 1:8,					#16
								lwd = 1,					#17
								plotAdd = F,				#18
								imageXlim = NULL,			#19
								xaxs = "internal",			#20
								yaxs = "internal",			#21
								imageAdd = F)				#22
{

	#print
	if(printObj) print(x)
	
	#summary
	if(printHist) print(summary(x))
			 


	if(doPlot){

		#plotSubset
#		if(plotSubset == "<All>") plotSubset = NULL
#		else plotSubset = parseVectorString(plotSubset)

		if(is.null(plotSubset) || is.all.white(plotSubset) || plotSubset == "<All>"){
			plotSubset = NULL
		} else {
			Names = colIds(x[["intensity"]])
			plotSubset = parseVectorString(plotSubset)
			plotSubset = seq(Names)[match(plotSubset, Names)]
		}

		#offset
		if(plotOffset == "<Auto>" || is.all.white(plotOffset)) plotOffset = NULL
		else plotOffset = as.numeric(plotOffset)

		#plotXlim	
		if(plotXlim == "<Auto>" || is.all.white(plotXlim)) plotXlim = NULL
		else plotXlim = parseVectorString(plotXlim)

		#pch						S-PLUS symbol number
  		symbolStyleList <-
    		c("Box, Empty",				#0
    	  	"Circle, Empty",			#1
          	"Triangle, Up, Empty",		#2
          	"Plus",						#3
          	"X",						#4
          	"Diamond, Empty",			#5
          	"Triangle, Dn, Empty",		#6
          	"Box X",					#7
          	"Plus X",					#8
          	"Diamond +",				#9
          	"Circle +",					#10
          	"Tri. Up Down",				#11
          	"Box +",					#12
          	"Circle X",					#13
          	"Tri. Up Box",				#14
          	"Box, Solid",				#15
          	"Circle, Solid",			#16
          	"Triangle, Up, Solid",		#17
          	"Diamond, Solid",			#18
          	"Triangle, Dn, Solid",		#19
          	"Tri. Dn Box",				#27
          	"Cross",					#20
          	"|",						#23
          	"Diamond X")				#24

		if(pch == "<Auto>" || is.all.white(pch)) pch = seq(numCols(x$intensity))
		else if(!is.na(match(pch, symbolStyleList)))
			pch = c(0:19, 27, 20, 23:24)[match(pch, symbolStyleList)]
		else pch = parseVectorString(pch)

		#lty
		if(lty == "Auto" || lty == "<Auto>" || is.all.white(lty) || is.null(lty)) lty = 1:2
		else {
#			nch = nchar(lty)
#			lty = substring(lty, nch - 4, nch - 4)
#			if(any(lty == "o"))  lty = 1:2
#			else lty = as.numeric(lty)
			lty <- as.numeric(sapply(unpaste(lty, sep=","), 
				function(str) substring(str, first=nchar(str)-4, last=nchar(str)-4)))
		}
		#col	
		if(is.all.white(col) || is.null(col)) col = seq(numCols(x$intensity))
		else {
			colorFiles = unpaste(unlist(unpaste(col, sep = ",")), sep = "\\\\")
			col = unpaste(colorFiles[[length(colorFiles)]], sep = '.')[[1]]
			if(any(col == "Auto") || any(col == "<Auto>")) col = seq(numCols(x$intensity))
		}
		#else col = parseVectorString(col)

		#lwd	
		if(lwd == "<Auto>" || is.all.white(lwd)) lwd = 1
		else lwd = parseVectorString(lwd)
		
		plot(x = x,
		 	process = plotVariable,
		 	subset = plotSubset,
		 	offset = plotOffset,
		 	xaxis = plotXaxis,
		 	xlim = plotXlim,
		 	pch = pch,
		 	lty = lty,
		 	col = col,
		 	lwd = lwd,
		 	add = plotAdd,
		 	main = paste(plotVariable, "for", deparse(substitute(x))))
	}
	
	if(doImage){

		#imageSubset
#		if(imageSubset == "<All>") imageSubset = NULL
#		else imageSubset = parseVectorString(imageSubset)

		if(is.null(imageSubset) || is.all.white(imageSubset) || imageSubset == "<All>"){
			imageSubset = NULL
		} else {
			Names = colIds(x[["intensity"]])
			imageSubset = parseVectorString(imageSubset)
			imageSubset = seq(Names)[match(imageSubset, Names)]
		}

		#imageXlim	
		if(imageXlim == "<Auto>" || is.all.white(imageXlim)) imageXlim = NULL
		else imageXlim = parseVectorString(imageXlim)

		#xaxs and yaxs
		xaxs = substring(xaxs, 1, 1)
		yaxs = substring(yaxs, 1, 1)	

		image(x = x,
		  	  what = imageVariable,
		  	  subset = imageSubset,
		  	  xaxis = imageXaxis,
		  	  xlim = imageXlim,
		  	  add = imageAdd,
		  	  xaxs = xaxs,
		  	  yaxs = yaxs,
		  	  main = paste(imageVariable, "for", deparse(substitute(x))))			
		
	}
	

	invisible()
}

